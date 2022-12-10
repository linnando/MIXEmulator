package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("InconsistentReadException")
class InconsistentReadException extends UndeterminedStateException {
  @JSExport
  def tag: String = getClass.getSimpleName
}
