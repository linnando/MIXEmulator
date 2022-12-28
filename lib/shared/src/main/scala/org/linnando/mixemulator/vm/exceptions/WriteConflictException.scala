package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("WriteConflictException")
class WriteConflictException extends UndeterminedStateException {
  @JSExport
  def tag: String = getClass.getSimpleName
}
