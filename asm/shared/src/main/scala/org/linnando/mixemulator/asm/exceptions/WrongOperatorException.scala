package org.linnando.mixemulator.asm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("WrongOperatorException")
class WrongOperatorException(@JSExport val operator: String, @JSExport val line: Int) extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName
}
