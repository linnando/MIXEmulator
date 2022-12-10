package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("UnsupportedCharacterException")
class UnsupportedCharacterException(val char: Char) extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName

  @JSExport("char")
  def charJs: String = char.toString
}
