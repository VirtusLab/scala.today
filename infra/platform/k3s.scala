import besom.*
import besom.api.command.*

case class AuthArgs(registry: Input[NonEmptyString], username: Input[NonEmptyString], password: Input[NonEmptyString])

case class K3SArgs private (
  clusterName: Output[NonEmptyString],
  servers: Vector[Output[String]],
  privateKey: Output[String],
  k3sVersion: Output[String],
  registryAuth: Output[List[AuthArgs]]
):
  def authFileContents(using Context): Output[String] =
    registryAuth.flatMap { registryCreds =>
      registryCreds.foldLeft(Output("configs:\n")) { case (acc, cred) =>
        acc.flatMap { str =>
          val block =
            p"""  ${cred.registry}:
               |    auth:
               |      username: ${cred.username}
               |      password: ${cred.password}""".stripMargin

          block.map(b => str + b)
        }
      }
    }

object K3SArgs:
  def apply(
    clusterName: Input[NonEmptyString],
    servers: Vector[Input[String]],
    privateKey: Input[String],
    k3sVersion: Input[String],
    registryAuth: Input.OneOrList[AuthArgs] = List.empty
  )(using Context): K3SArgs =
    new K3SArgs(
      clusterName.asOutput(),
      servers.map(_.asOutput()),
      privateKey.asOutput(),
      k3sVersion.asOutput(),
      registryAuth.asManyOutput()
    )

case class K3S(kubeconfig: Output[String], leaderIp: Output[String], followerIps: Output[Vector[String]])(using ComponentBase)
    extends ComponentResource derives RegistersOutputs

object K3S:
  def apply(name: NonEmptyString, args: K3SArgs, resourceOpts: ComponentResourceOptions)(using Context): Output[K3S] =
    component(name, "user:component:K3S", resourceOpts) {
      val echoFileCommand =
        p"""mkdir -p /etc/rancher/k3s/ && cat << EOF > /etc/rancher/k3s/registries.yaml
           |${args.authFileContents}
           |EOF""".stripMargin

      val k3sVersion = args.k3sVersion

      val leaderIp = args.servers.headOption match
        case Some(ip) => ip
        case None     => Output.fail(Exception("Can't deploy K3S without servers, silly."))

      val followers = if args.servers.isEmpty then Vector.empty else args.servers.tail

      val leaderConn = remote.inputs.ConnectionArgs(
        host = leaderIp,
        user = "root",
        privateKey = args.privateKey
      )

      val initializeK3sLeader = remote.Command(
        "start-k3s-leader",
        remote.CommandArgs(
          connection = leaderConn,
          create = p"curl -sfL https://get.k3s.io | INSTALL_K3S_VERSION=$k3sVersion sh -s - --flannel-backend=wireguard-native",
          delete = "sh /usr/local/bin/k3s-uninstall.sh"
        )
      )

      val token =
        remote
          .Command(
            "get-leader-token",
            remote
              .CommandArgs(
                connection = leaderConn,
                create = "cat /var/lib/rancher/k3s/server/node-token"
              ),
            opts(dependsOn = initializeK3sLeader)
          )
          .stdout

      val insertGhcrToken =
        remote.Command(
          "insert-ghcr-token-leader",
          remote.CommandArgs(
            connection = leaderConn,
            create = echoFileCommand
          ),
          opts(dependsOn = initializeK3sLeader)
        )

      val restartK3sLeader =
        remote.Command(
          "restart-k3s-leader",
          remote.CommandArgs(
            connection = leaderConn,
            create = "sudo systemctl force-reload k3s"
          ),
          opts(dependsOn = insertGhcrToken)
        )

      val kubeconfig =
        remote
          .Command(
            "get-kubeconfig",
            remote.CommandArgs(
              connection = leaderConn,
              create = "cat /etc/rancher/k3s/k3s.yaml"
            ),
            opts(dependsOn = initializeK3sLeader)
          )
          .stdout

      val initializeFollowers = followers.zipWithIndex.map { case (followerIpOutput, idx) =>
        val followerIdx = idx + 1

        val followerConnection = remote.inputs.ConnectionArgs(
          host = followerIpOutput,
          user = "root",
          privateKey = args.privateKey
        )

        val installOnFollower = remote.Command(
          s"start-k3s-follower-$followerIdx",
          remote.CommandArgs(
            connection = followerConnection,
            create =
              p"curl -sfL https://get.k3s.io | INSTALL_K3S_VERSION=$k3sVersion K3S_URL=https://${leaderIp}:6443 K3S_TOKEN=${token} sh -s -"
          ),
          opts(dependsOn = restartK3sLeader)
        )

        val insertGhcrToken = remote.Command(
          s"insert-ghcr-token-$followerIdx",
          remote.CommandArgs(
            connection = followerConnection,
            create = echoFileCommand
          ),
          opts(dependsOn = installOnFollower)
        )

        val restartK3sFollower = remote.Command(
          s"""restart-k3s-follower-$followerIdx""",
          remote.CommandArgs(
            connection = followerConnection,
            create = "sudo systemctl force-reload k3s-agent"
          ),
          opts(dependsOn = insertGhcrToken)
        )

        restartK3sFollower
      }.parSequence

      val adjustedKubeconfig =
        for
          _           <- restartK3sLeader
          _           <- initializeFollowers
          config      <- kubeconfig
          clusterName <- args.clusterName
          ip          <- leaderIp
        yield config.replace("default", clusterName).replace("127.0.0.1", ip)

      new K3S(adjustedKubeconfig, leaderIp, Output.sequence(followers))
    }
