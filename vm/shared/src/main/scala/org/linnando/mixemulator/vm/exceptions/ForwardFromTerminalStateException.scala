package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("ForwardFromTerminalStateException")
class ForwardFromTerminalStateException extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName
}
