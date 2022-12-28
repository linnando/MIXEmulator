package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("UnsupportedIoOperationException")
class UnsupportedIoOperationException(@JSExport val operation: String, @JSExport val deviceNum: Int) extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName
}
