package org.linnando.mixemulator.vm

object Comparison extends Enumeration {
  protected case class ComparisonVal(signum: Int) extends super.Val {}
  import scala.language.implicitConversions
  implicit def valueToComparisonVal(x: Value): ComparisonVal = x.asInstanceOf[ComparisonVal]

  val LESS: Value = ComparisonVal(-1)
  val EQUAL: Value = ComparisonVal(0)
  val GREATER: Value = ComparisonVal(1)
}
