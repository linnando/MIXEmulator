package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("BackFromInitialStateException")
class BackFromInitialStateException extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName
}
