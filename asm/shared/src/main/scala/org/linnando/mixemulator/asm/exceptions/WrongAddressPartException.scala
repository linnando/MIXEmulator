package org.linnando.mixemulator.asm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("WrongAddressPartException")
class WrongAddressPartException(@JSExport val addressPart: String, @JSExport val line: Int) extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName
}
