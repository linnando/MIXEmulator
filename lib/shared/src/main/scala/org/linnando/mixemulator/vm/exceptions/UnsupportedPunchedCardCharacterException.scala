package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("UnsupportedPunchedCardCharacterException")
class UnsupportedPunchedCardCharacterException(val char: Char) extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName

  @JSExport("char")
  def charJs: String = char.toString
}
