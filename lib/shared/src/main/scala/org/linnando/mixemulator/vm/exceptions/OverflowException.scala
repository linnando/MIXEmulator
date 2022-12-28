package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("OverflowException")
class OverflowException extends UndeterminedStateException {
  @JSExport
  def tag: String = getClass.getSimpleName
}
