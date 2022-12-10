package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("UndefinedSymbolException")
class UndefinedSymbolException(@JSExport val symbol: String) extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName
}
