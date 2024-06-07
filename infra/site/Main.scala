import besom.*
import besom.json.*
import besom.internal.CustomTimeouts
import besom.api.{kubernetes => k8s}
import besom.api.{cloudflare => cf}
import scala.concurrent.duration.*

import k8s.core.v1.enums.*
import k8s.core.v1.inputs.*
import k8s.apps.v1.inputs.*
import k8s.meta.v1.inputs.*
import k8s.apps.v1.{Deployment, DeploymentArgs, StatefulSet, StatefulSetArgs}
import k8s.core.v1.{Namespace, NamespaceArgs, Service, ServiceArgs}
import k8s.networking.v1.{Ingress, IngressArgs}
import k8s.networking.v1.inputs.{
  IngressSpecArgs,
  IngressRuleArgs,
  HttpIngressRuleValueArgs,
  HttpIngressPathArgs,
  IngressBackendArgs,
  IngressServiceBackendArgs,
  ServiceBackendPortArgs
}

import besom.cfg.k8s.ConfiguredContainerArgs
import besom.cfg.Struct

@main def main = Pulumi.run {
  val platformStackRef = StackReference("platform-stackRef", StackReferenceArgs("organization/scala.today/platform-prod"))

  val kubeconfig = platformStackRef.flatMap(
    _.getOutput("kubeconfig")
      .flatMap {
        case Some(JsString(s)) => Output(s)
        case other             => Output.fail(RuntimeException(s"Expected string, got $other"))
      }
  )

  val nodes = platformStackRef.flatMap(
    _.getOutput("nodes")
      .flatMap {
        case Some(JsArray(arr)) => Output(arr.collect { case JsString(s) => s })
        case other              => Output.fail(RuntimeException(s"Expected array, got $other"))
      }
  )

  val k3sProvider = k8s.Provider(
    "k8s",
    k8s.ProviderArgs(
      kubeconfig = kubeconfig
    )
  )

  val appNamespace = Namespace(
    "app",
    NamespaceArgs(
      metadata = ObjectMetaArgs(
        name = "app"
      )
    ),
    opts(provider = k3sProvider, protect = true)
  )

  val dbNamespace = Namespace(
    "db",
    NamespaceArgs(
      metadata = ObjectMetaArgs(
        name = "db"
      )
    ),
    opts(provider = k3sProvider, protect = true)
  )

  val dbLabels = Map("db" -> "postgresml")
  val labels   = Map("app" -> "scala.today")

  val postgresPort  = 5432
  val dashboardPort = 8000
  val containerPort = 8080
  val servicePort   = 8080
  val ingressHost   = "dev.scala.today"

  val postgresmlStatefulSet = k8s.apps.v1.StatefulSet(
    "postgresml",
    k8s.apps.v1.StatefulSetArgs(
      metadata = ObjectMetaArgs(
        name = "postgresml",
        namespace = dbNamespace.metadata.name,
        labels = dbLabels
      ),
      spec = StatefulSetSpecArgs(
        serviceName = "postgresml",
        replicas = 1,
        selector = LabelSelectorArgs(matchLabels = dbLabels),
        template = PodTemplateSpecArgs(
          metadata = ObjectMetaArgs(
            labels = dbLabels
          ),
          spec = PodSpecArgs(
            containers = ContainerArgs(
              name = "postgresml",
              image = "ghcr.io/postgresml/postgresml:2.8.2",
              args = List("tail", "-f", "/dev/null"),
              readinessProbe = ProbeArgs(
                exec = ExecActionArgs(
                  command = List("psql", "-d", "postgresml", "-c", "SELECT 1")
                ),
                initialDelaySeconds = 15,
                timeoutSeconds = 2
              ),
              livenessProbe = ProbeArgs(
                exec = ExecActionArgs(
                  command = List("psql", "-d", "postgresml", "-c", "SELECT 1")
                ),
                initialDelaySeconds = 45,
                timeoutSeconds = 2
              ),
              ports = List(
                ContainerPortArgs(name = "postgres", containerPort = postgresPort),
                ContainerPortArgs(name = "dashboard", containerPort = dashboardPort)
              )
            ) :: Nil
          )
        )
      )
    ),
    opts(customTimeouts = CustomTimeouts(create = 10.minutes), provider = k3sProvider)
  )

  val postgresMlService = Service(
    "postgresml-svc",
    ServiceArgs(
      spec = ServiceSpecArgs(
        selector = dbLabels,
        ports = List(
          ServicePortArgs(name = "postgres", port = postgresPort, targetPort = postgresPort),
          ServicePortArgs(name = "dashboard", port = dashboardPort, targetPort = dashboardPort)
        )
      ),
      metadata = ObjectMetaArgs(
        namespace = dbNamespace.metadata.name,
        labels = labels
      )
    ),
    opts(
      customTimeouts = CustomTimeouts(create = 10.minutes),
      dependsOn = postgresmlStatefulSet,
      provider = k3sProvider
    )
  )

  val dbNs           = dbNamespace.metadata.name.getOrFail(Exception("db namespace name not found!"))
  val postgresmlHost = postgresMlService.metadata.name.getOrFail(Exception("postgresml service name not found!"))
  val jdbcUrl        = p"jdbc:postgresql://${postgresmlHost}.${dbNs}.svc.cluster.local:${postgresPort}/postgresml"

  val appDeployment =
    Deployment(
      "app-deployment",
      DeploymentArgs(
        spec = DeploymentSpecArgs(
          selector = LabelSelectorArgs(matchLabels = labels),
          replicas = 1,
          template = PodTemplateSpecArgs(
            metadata = ObjectMetaArgs(
              name = "app-deployment",
              labels = labels,
              namespace = appNamespace.metadata.name
            ),
            spec = PodSpecArgs(
              containers = ConfiguredContainerArgs(
                name = "app",
                image = "ghcr.io/lbialy/scala.today:0.0.15",
                configuration = Struct(
                  jdbcUrl = jdbcUrl,
                  port = containerPort,
                  dbUser = config.requireString("db_user"),
                  dbPassword = config.requireString("db_password")
                ),
                ports = List(
                  ContainerPortArgs(name = "http", containerPort = containerPort)
                ),
                readinessProbe = ProbeArgs(
                  httpGet = HttpGetActionArgs(
                    path = "/",
                    port = containerPort
                  ),
                  initialDelaySeconds = 10,
                  periodSeconds = 5
                ),
                livenessProbe = ProbeArgs(
                  httpGet = HttpGetActionArgs(
                    path = "/",
                    port = containerPort
                  ),
                  initialDelaySeconds = 10,
                  periodSeconds = 5
                )
              ) :: Nil
            )
          )
        ),
        metadata = ObjectMetaArgs(
          namespace = appNamespace.metadata.name
        )
      ),
      opts(provider = k3sProvider)
    )

  val appService =
    Service(
      "app-svc",
      ServiceArgs(
        spec = ServiceSpecArgs(
          selector = labels,
          ports = List(
            ServicePortArgs(name = "http", port = servicePort, targetPort = containerPort)
          ),
          `type` = ServiceSpecType.ClusterIP
        ),
        metadata = ObjectMetaArgs(
          namespace = appNamespace.metadata.name,
          labels = labels
        )
      ),
      opts(deleteBeforeReplace = true, provider = k3sProvider)
    )

  val appIngress =
    Ingress(
      "app-ingress",
      IngressArgs(
        spec = IngressSpecArgs(
          rules = List(
            IngressRuleArgs(
              host = ingressHost,
              http = HttpIngressRuleValueArgs(
                paths = List(
                  HttpIngressPathArgs(
                    path = "/",
                    pathType = "Prefix",
                    backend = IngressBackendArgs(
                      service = IngressServiceBackendArgs(
                        name = appService.metadata.name.getOrElse("app"),
                        port = ServiceBackendPortArgs(
                          number = servicePort
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        metadata = ObjectMetaArgs(
          namespace = appNamespace.metadata.name,
          labels = labels,
          annotations = Map(
            "kubernetes.io/ingress.class" -> "traefik"
          )
        )
      ),
      opts(provider = k3sProvider)
    )

  val cfProvider = cf.Provider(
    "cloudflare-provider",
    cf.ProviderArgs(
      apiToken = config.requireString("cloudflare_token")
    )
  )

  val aRecords = nodes
    .map(_.zipWithIndex)
    .flatMap { vec =>
      vec.map { case (ipv4Address, idx) =>
        val recordIdx = idx + 1

        cf.Record(
          s"scala.today-a-record-$recordIdx",
          cf.RecordArgs(
            name = ingressHost,
            `type` = "A",
            value = ipv4Address,
            zoneId = config.requireString("cloudflare_zone_id"),
            ttl = 1,
            proxied = true
          ),
          opts(provider = cfProvider)
        )
      }.parSequence
    }

  Stack(k3sProvider, appDeployment, appIngress, aRecords).exports(
    jdbcUrl = jdbcUrl
  )
}
