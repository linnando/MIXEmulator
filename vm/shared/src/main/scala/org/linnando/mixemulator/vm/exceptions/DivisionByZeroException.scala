package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("DivisionByZeroException")
class DivisionByZeroException extends UndeterminedStateException {
  @JSExport
  def tag: String = getClass.getSimpleName
}
