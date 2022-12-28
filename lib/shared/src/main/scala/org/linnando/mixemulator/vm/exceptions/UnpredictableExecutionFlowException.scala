package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("UnpredictableExecutionFlowException")
class UnpredictableExecutionFlowException extends UndeterminedStateException {
  @JSExport
  def tag: String = getClass.getSimpleName
}
