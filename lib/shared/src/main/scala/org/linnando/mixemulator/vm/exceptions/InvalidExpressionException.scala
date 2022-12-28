package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("InvalidExpressionException")
class InvalidExpressionException(@JSExport val expression: String) extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName
}
