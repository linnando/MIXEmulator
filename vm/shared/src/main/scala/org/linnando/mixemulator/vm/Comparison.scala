package org.linnando.mixemulator.vm

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

object Comparison extends Enumeration {
  @JSExportTopLevel("Comparison")
  protected case class ComparisonVal(@JSExport signum: Int) extends super.Val {}
  import scala.language.implicitConversions
  implicit def valueToComparisonVal(x: Value): ComparisonVal = x.asInstanceOf[ComparisonVal]

  val LESS: Value = ComparisonVal(-1)
  val EQUAL: Value = ComparisonVal(0)
  val GREATER: Value = ComparisonVal(1)
}
