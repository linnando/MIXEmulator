package org.linnando.mixemulator.webapp

import angulate2.std._
import org.linnando.mixemulator.{DevicesFrontEnd, VirtualMachineFrontEnd}
import org.linnando.mixemulator.vm.VirtualMachineState
import org.linnando.mixemulator.vm.io.data.IOWord
import rxjs.Subject

import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Injectable()
class VirtualMachineService {
  var text: String =
    """START      OUT  HELLO(18)
           JBUS *(18)
           HLT
HELLO      ALF  HELLO
           ALF  , WOR
           ALF  LD
           END START"""
  var mode = "binary"
  var tracking = true

  val stateChange: Subject[Unit] = new Subject()

  private var maybeMachine: Option[VirtualMachineFrontEnd] = None
  private val devicesFrontEnd = new DevicesFrontEnd(
    new BlockAccessFileOpsImpl,
    new LineAccessFileInputOpsImpl,
    new LineAccessFileOutputOpsImpl)

  def assemble(): Future[Unit] = Future {
    val lines = text.split("\n").toVector
    maybeMachine = Some(
      mode match {
        case "binary" =>
          if (tracking) VirtualMachineFrontEnd.createBinaryTracking(lines, devicesFrontEnd)
          else VirtualMachineFrontEnd.createBinaryNonTracking(lines, devicesFrontEnd)
        case "decimal" =>
          if (tracking) VirtualMachineFrontEnd.createDecimalTracking(lines, devicesFrontEnd)
          else VirtualMachineFrontEnd.createDecimalNonTracking(lines, devicesFrontEnd)
      }
    )
  }

  def go(): Future[Unit] = {
    val eventualMachine: Future[VirtualMachineFrontEnd] = {
      mode match {
        case "binary" =>
          if (tracking) VirtualMachineFrontEnd.goBinaryTracking(devicesFrontEnd)
          else VirtualMachineFrontEnd.goBinaryNonTracking(devicesFrontEnd)
        case "decimal" =>
          if (tracking) VirtualMachineFrontEnd.goDecimalTracking(devicesFrontEnd)
          else VirtualMachineFrontEnd.goDecimalNonTracking(devicesFrontEnd)
      }
    }
    eventualMachine map { machine =>
      maybeMachine = Some(machine)
    }
  }

  def switchOffMachine(): Unit = {
    maybeMachine = None
  }

  def isActive: Boolean = maybeMachine.isDefined

  def canMoveForward: Boolean = maybeMachine.exists(_.canMoveForward)

  def runForward(): Future[Unit] = maybeMachine.map(_.runForward().map(stateChange.next)).getOrElse(Future {})

  def stepForward(): Future[Unit] = maybeMachine.map(_.stepForward().map(stateChange.next)).getOrElse(Future {})

  def canMoveBack: Boolean = maybeMachine.exists(_.canMoveBack)

  def runBack(): Unit = maybeMachine.foreach(m => {
    m.runBack()
    stateChange.next(Unit)
  })

  def stepBack(): Unit = maybeMachine.foreach(m => {
    m.stepBack()
    stateChange.next(Unit)
  })

  def machineState: Option[VirtualMachineState] = maybeMachine.map(_.currentState)

  def symbolsLength: Int = maybeMachine.map(_.symbolsLength).getOrElse(0)

  def symbolIndices: Range = maybeMachine.map(_.symbolIndices).getOrElse(0 until 0)

  def programCounterIndex: Option[Int] = maybeMachine.map(_.programCounterIndex)

  def toggleBreakpointAt(index: Int): Unit = maybeMachine.foreach(_.toggleBreakpointAt(index))

  def addressAt(index: Int): Option[Short] = maybeMachine.flatMap(_.addressAt(index))

  def breakpointAt(index: Int): Option[Boolean] = maybeMachine.map(_.breakpointAt(index))

  def cellContent(index: Int): Option[IOWord] = maybeMachine.flatMap(_.cellContent(index))

  def lineNumberAt(index: Int): Option[Int] = maybeMachine.flatMap(_.lineNumberAt(index))

  def lineAt(index: Int): Option[String] = maybeMachine.flatMap(_.lineAt(index))

  def lineIsModifiedAt(index: Int): Option[Boolean] = maybeMachine.map(_.lineIsModifiedAt(index))

  def deviceNumbers: Iterable[Int] = VirtualMachineService.deviceNames.keys

  def deviceName(deviceNum: Int): String = VirtualMachineService.deviceNames(deviceNum)

  def blockDeviceData(deviceNum: Int): Future[IndexedSeq[IOWord]] =
    maybeMachine.map(_.blockDeviceData(deviceNum)).getOrElse(devicesFrontEnd.getBlockDeviceData(deviceNum))

  def saveBlockDevice(deviceNum: Int, data: Array[Byte]): Future[Unit] =
    devicesFrontEnd.saveBlockDevice(deviceNum, data)

  def lineDeviceData(deviceNum: Int): Future[IndexedSeq[String]] =
    maybeMachine.map(_.lineDeviceData(deviceNum)).getOrElse(devicesFrontEnd.getLineDeviceData(deviceNum))

  def saveLineDevice(deviceNum: Int, data: String): Future[Unit] =
    devicesFrontEnd.saveLineDevice(deviceNum, data)
}

object VirtualMachineService {
  private val deviceNames: Map[Int, String] = SortedMap(
    0 -> "Tape Unit 0",
    1 -> "Tape Unit 1",
    2 -> "Tape Unit 2",
    3 -> "Tape Unit 3",
    4 -> "Tape Unit 4",
    5 -> "Tape Unit 5",
    6 -> "Tape Unit 6",
    7 -> "Tape Unit 7",
    8 -> "Disk Unit 0",
    9 -> "Disk Unit 1",
    10 -> "Disk Unit 2",
    11 -> "Disk Unit 3",
    12 -> "Disk Unit 4",
    13 -> "Disk Unit 5",
    14 -> "Disk Unit 6",
    15 -> "Disk Unit 7",
    16 -> "Card Reader",
    17 -> "Card Punch",
    18 -> "Line Printer",
    20 -> "Paper Tape"
  )
}
