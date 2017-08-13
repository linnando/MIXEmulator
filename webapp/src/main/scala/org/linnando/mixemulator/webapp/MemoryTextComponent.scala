package org.linnando.mixemulator.webapp

import angulate2.core.{AfterViewInit, ElementRef}
import angulate2.std._
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.scalajs.js

@Component(
  selector = "memory-text",
  templateUrl = "webapp/src/main/resources/memory-text.component.html",
  styleUrls = @@@("webapp/src/main/resources/memory-text.component.css")
)
class MemoryTextComponent(virtualMachineService: VirtualMachineService) extends OnInit with AfterViewInit {
  private def machineState = virtualMachineService.machineState

  private def symbols = virtualMachineService.symbols

  @ViewChild("scrolling")
  var scrolling: ElementRef = _

  override def ngOnInit(): Unit = {
    virtualMachineService.stateChange.subscribe(_ => positionToProgramCounter())
  }

  override def ngAfterViewInit(): Unit = {
    positionToProgramCounter()
  }

  private def positionToProgramCounter(): Unit = virtualMachineService.getProgramCounterIndex match {
    case Some(index) =>
      val scrollHeight = scrolling.nativeElement.scrollHeight.asInstanceOf[Double]
      val clientHeight = scrolling.nativeElement.clientHeight.asInstanceOf[Double]
      scrolling.nativeElement.scrollTop = index * scrollHeight / symbols.length - clientHeight / 2
    case None =>
  }

  def indices(): js.Array[Int] = js.Array[Int]() ++ symbols.indices

  def isCurrent(index: Int): Boolean = virtualMachineService.getProgramCounterIndex.contains(index)

  def hasWord(index: Int): Boolean = symbols(index)._1.isDefined

  def cellAddress(index: Int): Short = symbols(index)._1.getOrElse(0)

  def cellSign(index: Int): String =
    cellContent(index).map(w => if (w.negative) "-" else "+").getOrElse("+")

  def cellByte(index: Int, pos: Int): Byte =
    cellContent(index).map(_.bytes(pos)).getOrElse(0)

  private def cellContent(index: Int): Option[IOWord] = {
    val address = symbols(index)._1
    machineState.flatMap(m => address.map(m.get))
  }

  def hasLine(index: Int): Boolean = symbols(index)._2.isDefined

  def lineNumber(index: Int): Int = symbols(index)._2.getOrElse(0)

  def line(index: Int): String = symbols(index)._2.map(virtualMachineService.getLine).getOrElse("")
}
