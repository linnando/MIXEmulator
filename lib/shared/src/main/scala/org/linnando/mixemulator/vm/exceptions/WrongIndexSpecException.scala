package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("WrongIndexSpecException")
class WrongIndexSpecException(@JSExport val indexSpec: Byte) extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName
}
