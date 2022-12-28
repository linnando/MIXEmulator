package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("WrongFieldSpecException")
class WrongFieldSpecException(@JSExport val fieldSpec: Byte) extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName
}
