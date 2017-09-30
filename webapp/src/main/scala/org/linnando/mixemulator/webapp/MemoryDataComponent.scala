package org.linnando.mixemulator.webapp

import angulate2.std._
import org.linnando.mixemulator.vm.VirtualMachine
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.immutable.NumericRange
import scala.scalajs.js

@Component(
  selector = "memory-data",
  templateUrl = "webapp/src/main/resources/memory-data.component.html",
  styleUrls = @@@("webapp/src/main/resources/memory-data.component.css")
)
class MemoryDataComponent(virtualMachineService: VirtualMachineService) {
  val addresses: js.Array[Short] = js.Array[Short]() ++ NumericRange[Short](0, VirtualMachine.MEMORY_SIZE, 1)

  private def machineState = virtualMachineService.machineState

  def cellSign(address: Short): String =
    cellContent(address).map(w => if (w.negative) "-" else "+").getOrElse("+")

  def cellByte(address: Short, pos: Int): Byte =
    cellContent(address).map(_.bytes(pos)).getOrElse(0)

  private def cellContent(address: Short): Option[IOWord] = machineState.map(_.get(address))
}
