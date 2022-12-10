package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("WrongCharacterCodeException")
class WrongCharacterCodeException(@JSExport val code: Byte) extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName
}
