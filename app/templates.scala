package scala.today

import scalatags.Text.all.*
import scalatags.Text.tags2.{style => styleTag, title => siteTitle, nav}
import scalatags.Text.svgTags.{attr => _, *}
import scalatags.Text.svgAttrs.{width => svgWidth, height => svgHeight, fill => svgFill, viewBox => svgViewBox, d}
import scala.today.db.ProjectWithLatestRelease
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter

import AlpineAttributes.*
import HtmxAttributes.*

object Templates:

  private val dtFormatter    = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm 'UTC'")
  private val dtFormatterSec = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss 'UTC'")

  val pagingWindowSize = 3

  private def scope = attr("scope")

  def computePagesVector(activeIdx: Int, totalPages: Int, windowSize: Int = 5): Vector[Int] =
    val halfOfW           = windowSize / 2
    val (lower, lowerRem) = if activeIdx - halfOfW < 1 then (1, halfOfW - activeIdx + 1) else (activeIdx - halfOfW, 0)
    val (upper, upperRem) = if activeIdx + halfOfW > totalPages then (totalPages, totalPages - (activeIdx + halfOfW)) else (activeIdx + halfOfW, 0)
    (scala.math.max(lower + upperRem, 1) to scala.math.min(upper + lowerRem, totalPages)).toVector

  private given Ordering[ProjectWithLatestRelease] = new Ordering[ProjectWithLatestRelease]:
    def compare(x: ProjectWithLatestRelease, y: ProjectWithLatestRelease): Int =
      -x.releaseDate.compareTo(y.releaseDate)

  case class ProjectAndArtifacts(
    project: String,
    latestVersion: String,
    latestReleaseDate: OffsetDateTime,
    artifacts: Vector[ProjectWithLatestRelease]
  )

  object ProjectAndArtifacts:
    import scala.collection.immutable.ListMap
    def groupByPreserveOrder[A, K](vec: Vector[A])(keyFunc: A => K): ListMap[K, Vector[A]] =
      vec.foldLeft(ListMap.empty[K, Vector[A]]) { (map, elem) =>
        val key = keyFunc(elem)
        map + (key -> (map.getOrElse(key, Vector.empty) :+ elem))
      }

    def fromDb(projectsWithLatestReleaseDate: Vector[(ProjectWithLatestRelease, OffsetDateTime)]): Vector[ProjectAndArtifacts] =
      groupByPreserveOrder(projectsWithLatestReleaseDate)(_._1.project).map { case (project, releases) =>
        val lastReleasedVersion = releases.maxBy(_._1.releaseDate)._1.lastVersion
        ProjectAndArtifacts(project, lastReleasedVersion, releases.head._2, releases.map(_._1))
      }.toVector

  def oobPagingNavigation(currentPage: Int, totalPages: Int) =
    val pages = computePagesVector(currentPage, totalPages, pagingWindowSize)
    pagingNavigation(pages, currentPage, totalPages, oob = true)

  def pagingNavigation(pages: Vector[Int], activeIdx: Int, totalPages: Int, oob: Boolean = false) =
    val pagesWithChevrons = Vector(Int.MinValue) ++ pages ++ Vector(Int.MaxValue)
    nav(
      id := "pagination-nav",
      cls := "flex items-center space-x-2",
      hxOobSwap := oob,
      // generate pages with active page based on the activeIdx
      pagesWithChevrons.zipWithIndex.map {
        case (_, idx) if idx == 0 =>
          a(
            cls := "text-gray-400 hover:text-red-600 p-4 inline-flex items-center gap-2 font-medium rounded-md",
            span(attr("aria-hidden") := "true", "«"),
            span(cls := "sr-only", "Previous"),
            href := s"/?page=1",
            hxPushUrl := s"/?page=1",
            hxTarget := "#projects-table",
            hxTrigger := "click",
            hxGet := s"/paging?page=1"
          )

        case (_, idx) if idx == pagesWithChevrons.size - 1 =>
          a(
            cls := "text-gray-400 hover:text-red-600 p-4 inline-flex items-center gap-2 font-medium rounded-md",
            span(attr("aria-hidden") := "true", "»"),
            span(cls := "sr-only", "Next"),
            href := s"/?page=${totalPages}",
            hxPushUrl := s"/?page=${totalPages}",
            hxTarget := "#projects-table",
            hxTrigger := "click",
            hxGet := s"/paging?page=${totalPages}"
          )

        case (page, idx) =>
          // for normal pages just render an anchor with the page number
          if page == activeIdx then
            a(
              cls := "w-10 h-10 bg-red-500 text-white p-4 inline-flex items-center text-sm font-medium rounded-full",
              attr("aria-current") := "page",
              href := s"/?page=$page",
              hxPushUrl := s"/?page=$page",
              hxTarget := "#projects-table",
              hxGet := s"/paging?page=$page",
              hxTrigger := "click",
              page.toString
            )
          else
            a(
              cls := "w-10 h-10 text-gray-400 hover:text-red-600 p-4 inline-flex items-center text-sm font-medium rounded-full",
              href := s"/?page=$page",
              hxPushUrl := s"/?page=$page",
              hxTarget := "#projects-table",
              hxGet := s"/paging?page=$page",
              hxTrigger := "click",
              page.toString
            )
      }
    )

  def tableRow(project: ProjectAndArtifacts) =
    tbody(
      cls := "dark:divide-gray-700 hover:bg-gray-100 dark:hover:bg-gray-700",
      `x-data`("{ open : false }"),
      tr(
        cls := "divide-y divide-gray-200",
        `@click`("open = !open"),
        td(
          cls := "px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-800 dark:text-gray-200",
          a(`@click.stop`, href := s"https://github.com/${project.project}", project.project)
        ),
        td(cls := "px-6 py-4 whitespace-nowrap text-sm text-gray-800 dark:text-gray-200", project.latestVersion),
        td(
          cls := "px-6 py-4 whitespace-nowrap text-sm text-gray-800 dark:text-gray-200",
          s"${project.latestReleaseDate.format(dtFormatter)}"
        ),
        td(
          cls := "px-6 py-4 whitespace-nowrap text-right text-sm font-medium",
          a(
            `@click.stop`,
            `:class`(s"wasVotedOn('${project.project}') ? 'text-gray-500 hover:text-gray-700' : 'text-red-500 hover:text-red-700'"),
            "It's blocking me!",
            `@click`(s"vote('${project.project}')")
          )
        )
      ),
      tr(
        `x-show`("open"),
        `x-transition`(),
        td(
          cls := "whitespace-nowrap text-sm font-medium text-gray-800 dark:text-gray-200",
          colspan := 4,
          div(
            cls := "bg-gray-50 dark:bg-gray-800",
            table(
              cls := "min-w-full divide-y divide-gray-200 dark:divide-gray-700",
              thead(
                cls := "bg-gray-50 dark:bg-gray-700",
                tr(
                  cls := "hover:bg-gray-100 dark:hover:bg-gray-700",
                  th(scope := "col", cls := "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase", "Group ID"),
                  th(scope := "col", cls := "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase", "Artifact Name"),
                  th(scope := "col", cls := "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase", "Release Date"),
                  th(scope := "col", cls := "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase", "Licenses"),
                  th(scope := "col", cls := "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase", "Language"),
                  th(scope := "col", cls := "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase", "Platform")
                )
              ),
              project.artifacts.sorted.map { artifact =>
                tr(
                  cls := "dark:divide-gray-700 hover:bg-gray-100 dark:hover:bg-gray-700",
                  td(cls := "px-6 py-4 whitespace-nowrap text-sm text-gray-800 dark:text-gray-200", artifact.groupId),
                  td(cls := "px-6 py-4 whitespace-nowrap text-sm text-gray-800 dark:text-gray-200", artifact.artifactName),
                  td(
                    cls := "px-6 py-4 whitespace-nowrap text-sm text-gray-800 dark:text-gray-200",
                    s"${artifact.releaseDate.format(dtFormatterSec)}"
                  ),
                  td(cls := "px-6 py-4 whitespace-nowrap text-sm text-gray-800 dark:text-gray-200", artifact.licenses.sorted.mkString(", ")),
                  td(cls := "px-6 py-4 whitespace-nowrap text-sm text-gray-800 dark:text-gray-200", artifact.language.sorted.mkString(", ")),
                  td(cls := "px-6 py-4 whitespace-nowrap text-sm text-gray-800 dark:text-gray-200", artifact.platform.sorted.mkString(", "))
                )
              }
            )
          )
        )
      )
    )

  def projectsTable(projects: Vector[ProjectAndArtifacts]) =
    table(
      `x-data`("voting()"),
      id := "projects-table",
      cls := "min-w-full divide-y divide-gray-200 dark:divide-gray-700",
      thead(
        cls := "bg-gray-50 dark:bg-gray-700",
        tr(
          cls := "hover:bg-gray-100 dark:hover:bg-gray-700",
          th(scope := "col", cls := "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase", "Project"),
          th(scope := "col", cls := "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase", "Version"),
          th(scope := "col", cls := "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase", "Latest release"),
          th(scope := "col", cls := "px-6 py-3 text-right text-xs font-medium text-gray-500 uppercase", "Action")
        )
      ),
      projects.map(project => tableRow(project))
    )

  def index(projects: Vector[ProjectAndArtifacts], totalPages: Int, currentPage: Int) =
    html(
      lang := "en",
      head(
        meta(charset := "UTF-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1.0"),
        siteTitle("Scala.today"),
        link(href := "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css", rel := "stylesheet"),
        link(rel := "preconnect", href := "https://fonts.googleapis.com"),
        link(rel := "preconnect", href := "https://fonts.gstatic.com", crossorigin := ""),
        link(href := "https://fonts.googleapis.com/css2?family=Open+Sans:wght@300&display=swap", rel := "stylesheet"),
        styleTag("""h1 {
              font-family: 'Open Sans', sans-serif;
          }""")
      ),
      body(
        cls := "flex justify-center h-screen bg-chalk",
        div(
          cls := "container mx-auto p-4",
          div(
            cls := "justify-center text-center mt-5 mb-10 flex",
            h1(cls := "text-6xl font-bold", a(href := "/", "Scala.today")),
            img(src := "static/scala-transparent.png", width := "40", cls := "inline-flex ml-4")
          ),
          div(
            cls := "flex flex-col",
            div(
              cls := "-m-1.5 overflow-x-auto",
              div(
                cls := "p-1.5 min-w-full inline-block align-middle",
                div(
                  cls := "border rounded-lg divide-y divide-gray-200 dark:border-gray-700 dark:divide-gray-700",
                  div(
                    cls := "py-3 px-4",
                    div(
                      cls := "relative max-w-xs justify-end",
                      label(`for` := "hs-table-with-pagination-search", cls := "sr-only", "Search"),
                      input(
                        `type` := "search",
                        hxGet := "/search",
                        hxTrigger := "keyup changed delay:500ms, search",
                        hxTarget := "#projects-table",
                        hxSync := "this:replace",
                        hxPushUrl := true,
                        name := "q",
                        id := "hs-table-with-pagination-search",
                        cls := "p-3 pl-10 block w-full border-gray-200 rounded-md text-sm focus:border-red-500 focus:ring-red-500 dark:bg-slate-900 dark:border-gray-700 dark:text-gray-400",
                        placeholder := "Search for items"
                      ),
                      div(
                        cls := "absolute inset-y-0 left-0 flex items-center pointer-events-none pl-4",
                        svg(
                          cls := "h-3.5 w-3.5 text-gray-400",
                          xmlns := "http://www.w3.org/2000/svg",
                          width := "16",
                          height := "16",
                          svgFill := "currentColor",
                          svgViewBox := "0 0 16 16",
                          path(
                            d := "M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h-.001c.03.04.062.078.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1.007 1.007 0 0 0-.115-.1zM12 6.5a5.5 5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0z"
                          )
                        )
                      )
                    )
                  ),
                  div(
                    cls := "overflow-hidden",
                    projectsTable(projects)
                  ),
                  div(
                    cls := "py-1 px-4",
                    pagingNavigation(computePagesVector(currentPage, totalPages, pagingWindowSize), currentPage, totalPages)
                  )
                )
              )
            )
          )
        ),
        script(src := "https://cdn.jsdelivr.net/npm/alpinejs@3.14.0/dist/cdn.min.js", defer := true),
        script(
          src := "https://unpkg.com/htmx.org@1.9.12",
          integrity := "sha384-ujb1lZYygJmzgSwoxRggbCHcjc0rB2XoQrxeTUQyRjrOnlCoYta87iKBWq3EsdM2",
          crossorigin := "anonymous"
        ),
        script(src := "static/voting.js")
      )
    )
