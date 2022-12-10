package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("EndOfFileException")
class EndOfFileException extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName
}
