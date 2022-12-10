package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("DuplicateSymbolException")
class DuplicateSymbolException(@JSExport val symbol: String) extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName
}
