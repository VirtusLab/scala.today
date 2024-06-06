package scala.today

import scalatags.Text.all.*

object HtmxAttributes:
  def hxBoost(value: Boolean = true) = attr("hx-boost") := value
  def hxPushUrl(value: String | Boolean = true) = value match
    case bool: Boolean => attr("hx-push-url") := bool
    case str: String   => attr("hx-push-url") := str

  def hxConfirm(message: String) = attr("hx-confirm") := message

  def hxDelete(endpoint: String)                      = attr("hx-delete") := endpoint
  def hxGet(endpoint: String)                         = attr("hx-get") := endpoint
  def hxIndicator(indicatorType: String = "#spinner") = attr("hx-indicator") := indicatorType
  def hxSelect(value: String)                         = attr("hx-select") := value
  def hxSwap(value: String)                           = attr("hx-swap") := value

  def hxTarget(element: String) = attr("hx-target") := element
  def hxTrigger(value: String)  = attr("hx-trigger") := value
  def hxOobSwap(value: Boolean) = attr("hx-swap-oob") := value

object AlpineAttributes:
  def `x-data`(value: String) = attr("x-data") := value
  def `x-show`(value: String) = attr("x-show") := value
  def `@click`(value: String) = attr("@click", raw = true) := value
  def `x-transition`(suffix: Option[String] = None, value: Option[String] = None) =
    attr("x-transition" + suffix.fold("")(s => s":$s")) := value.fold("")(identity)
