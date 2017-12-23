package org.linnando.mixemulator.webapp

import angulate2.core.{AfterViewInit, ElementRef}
import angulate2.std._

import scala.scalajs.js

@Component(
  selector = "memory-text",
  templateUrl = "webapp/src/main/resources/memory-text.component.html",
  styleUrls = @@@("webapp/src/main/resources/memory-text.component.css")
)
class MemoryTextComponent(virtualMachineService: VirtualMachineService) extends OnInit with AfterViewInit {
  @ViewChild("scrolling")
  var scrolling: ElementRef = _

  override def ngOnInit(): Unit = {
    virtualMachineService.stateChange.subscribe(_ => positionToProgramCounter())
  }

  override def ngAfterViewInit(): Unit = {
    positionToProgramCounter()
  }

  private def positionToProgramCounter(): Unit = virtualMachineService.programCounterIndex match {
    case Some(index) =>
      val scrollHeight = scrolling.nativeElement.scrollHeight.asInstanceOf[Double]
      val clientHeight = scrolling.nativeElement.clientHeight.asInstanceOf[Double]
      scrolling.nativeElement.scrollTop = index * scrollHeight / virtualMachineService.symbolsLength - clientHeight / 2
    case None =>
  }

  def indices: js.Array[Int] = js.Array[Int]() ++ virtualMachineService.symbolIndices

  def toggleBreakpoint(index: Int): Unit = virtualMachineService.toggleBreakpointAt(index)

  def hasBreakpointAt(index: Int): Boolean = virtualMachineService.breakpointAt(index).exists(b => b)

  def isCurrent(index: Int): Boolean = virtualMachineService.programCounterIndex.contains(index)

  def hasWordAt(index: Int): Boolean = {
    val address = virtualMachineService.addressAt(index)
    address.isDefined
  }

  def cellAddressAt(index: Int): Short = {
    val address = virtualMachineService.addressAt(index)
    address.getOrElse(0)
  }

  def cellSignAt(index: Int): String = {
    val cellContent = virtualMachineService.cellContent(index)
    cellContent.map(w => if (w.negative) "-" else "+").getOrElse("+")
  }

  def cellByteAt(index: Int, pos: Int): Byte = {
    val cellContent = virtualMachineService.cellContent(index)
    cellContent.map(_.bytes(pos)).getOrElse(0)
  }

  def hasLineAt(index: Int): Boolean = {
    val lineNumber = virtualMachineService.lineNumberAt(index)
    lineNumber.isDefined
  }

  def lineNumberAt(index: Int): Int = {
    val lineNumber = virtualMachineService.lineNumberAt(index)
    lineNumber.getOrElse(0)
  }

  def lineAt(index: Int): String = {
    val line = virtualMachineService.lineAt(index)
    line.getOrElse("")
  }

  def lineIsModifiedAt(index: Int): Boolean = virtualMachineService.lineIsModifiedAt(index).getOrElse(false)
}
