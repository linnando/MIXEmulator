package org.linnando.mixemulator.webapp

import angulate2.std._
import org.linnando.mixemulator.asm.{MixAssembler, MixDisassembler}
import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.{TrackingVirtualMachine, VirtualMachine, VirtualMachineState, binary}
import rxjs.Subject

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

@Injectable()
class VirtualMachineService {
  var text = ""

  val stateChange: Subject[Unit] = new Subject()

  private var lines: Vector[String] = Vector.empty

  private var _machine: Option[VirtualMachine] = None

  private var _symbols: Vector[(Option[Short], Option[Int])] = Vector.empty

  private var _addressSymbols: Vector[Int] = Vector.empty

  private var disassembler: MixDisassembler = _

  def assembleBinaryNonTracking(): Future[Unit] = Future {
    lines = text.split("\n").toVector
    val builder = binary.createVirtualMachineBuilder()
    saveAssemble(MixAssembler.translateNonTracking(builder, lines))
    disassembler = new MixDisassembler(binary)
  }

  private def saveAssemble(assemble: (VirtualMachine, List[(Option[Short], Option[Int])])): Unit = {
    _machine = Some(assemble._1)
    _symbols = assemble._2.toVector
    _addressSymbols = _symbols.indices.foldLeft(Vector.fill(VirtualMachine.MEMORY_SIZE)(0)) { (s, index) =>
      _symbols(index) match {
        case (Some(address), _) => s.updated(address, index)
        case (None, _) => s
      }
    }
  }

  def assembleBinaryTracking(): Future[Unit] = Future {
    lines = text.split("\n").toVector
    val builder = binary.createVirtualMachineBuilder()
    saveAssemble(MixAssembler.translateTracking(builder, lines))
    disassembler = new MixDisassembler(binary)
  }

  def isActive: Boolean = _machine.isDefined

  def canMoveForward: Boolean = _machine.exists(_.canMoveForward)

  def runForward(): Unit = _machine match {
    case Some(m) =>
      m.runForward()
      stateChange.next(Unit)
    case None => throw new Error
  }

  def stepForward(): Unit = _machine match {
    case Some(m) =>
      m.stepForward()
      stateChange.next(Unit)
    case None => throw new Error
  }

  def canMoveBack: Boolean = _machine.exists({
    case m: TrackingVirtualMachine => m.canMoveBack
    case _ => false
  })

  def runBack(): Unit = _machine match {
    case Some(m: TrackingVirtualMachine) =>
      m.runBack()
      stateChange.next(Unit)
    case _ => throw new Error
  }

  def stepBack(): Unit = _machine match {
    case Some(m: TrackingVirtualMachine) =>
      m.stepBack()
      stateChange.next(Unit)
    case _ => throw new Error
  }

  def machineState: Option[VirtualMachineState] = _machine.map(_.currentState)

  def symbolsLength: Int = _symbols.length

  def symbolIndices: Range = _symbols.indices

  def programCounterIndex: Option[Int] = machineState.map(s => _addressSymbols(s.getProgramCounter))

  def toggleBreakpointAt(index: Int): Unit = _machine match {
    case Some(m) => addressAt(index).foreach(m.toggleBreakpoint)
    case None => throw new Error
  }

  def addressAt(index: Int): Option[Short] = _symbols(index)._1

  def breakpointAt(index: Int): Option[Boolean] = _machine.map(m => addressAt(index).exists(m.breakpoints))

  def cellContent(index: Int): Option[IOWord] = {
    val address = addressAt(index)
    machineState.flatMap(s => address.map(s.get))
  }

  def lineNumberAt(index: Int): Option[Int] = _symbols(index)._2

  def lineAt(index: Int): Option[String] = _machine.flatMap(m => {
    val (address, lineNumber) = _symbols(index)
    lineNumber.map(ln =>
      if (address.exists(m.isModified)) {
        val cellContent = m.currentState.get(address.get)
        disassembler.disassembleLine(cellContent)
      } else lines(ln)
    )
  })

  def lineIsModifiedAt(index: Int): Option[Boolean] = _machine.map(m => {
    val address = addressAt(index)
    address.exists(m.isModified)
  })
}
