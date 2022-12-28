package org.linnando.mixemulator.asm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("WrongLineException")
class WrongLineException(@JSExport val line: Int) extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName
}
