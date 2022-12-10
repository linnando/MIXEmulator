package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("WrongLabelException")
class WrongLabelException(@JSExport val label: String) extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName
}
