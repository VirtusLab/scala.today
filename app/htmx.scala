package scala.today

import scalatags.Text.all.*

object HtmxAttributes:
  def hxBoost   = attr("hx-boost")
  def hxPushUrl = attr("hx-push-url")

  def hxConfirm = attr("hx-confirm")

  def hxDelete                                        = attr("hx-delete")
  def hxGet                                           = attr("hx-get")
  def hxIndicator(indicatorType: String = "#spinner") = attr("hx-indicator") := indicatorType
  def hxIndicator                                     = attr("hx-indicator")
  def hxSelect                                        = attr("hx-select")
  def hxSwap                                          = attr("hx-swap")
  def hxSync                                          = attr("hx-sync")

  def hxTarget  = attr("hx-target")
  def hxTrigger = attr("hx-trigger")
  def hxOobSwap = attr("hx-swap-oob")

object AlpineAttributes:
  def `x-data`(value: String)         = attr("x-data") := value
  def `x-show`(value: String)         = attr("x-show") := value
  def `@click`(value: String)         = attr("@click", raw = true) := value
  def `@click.prevent`(value: String) = attr("@click", raw = true) := value
  def `@click.stop`                   = attr("@click.stop", raw = true) := ""
  def `:class`(value: String)         = attr(":class") := value
  def `x-transition`(suffix: Option[String] = None, value: Option[String] = None) =
    attr("x-transition" + suffix.fold("")(s => s":$s")) := value.fold("")(identity)
