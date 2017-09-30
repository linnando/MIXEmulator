package org.linnando.mixemulator.webapp

import angulate2.std._
import org.linnando.mixemulator.asm.{MixAssembler, MixDisassembler}
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

  def getLine(index: Int, address: Option[Short]): String =
    _machine.flatMap(m => address.flatMap(a =>
      if (m.isModified(a)) Some(disassembler.disassembleLine(m.currentState.get(a)))
      else None
    )).getOrElse(lines(index))

  def lineIsModified(address: Option[Short]): Boolean = _machine.exists(m => address.exists(a => m.isModified(a)))

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

  def canMoveForward: Boolean = _machine match {
    case None => false
    case Some(m) => m.canMoveForward
  }

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

  def canMoveBack: Boolean = _machine match {
    case Some(m: TrackingVirtualMachine) => m.canMoveBack
    case _ => false
  }

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

  def getProgramCounterIndex: Option[Int] = machineState.map(s => _addressSymbols(s.getProgramCounter))

  def symbols: Vector[(Option[Short], Option[Int])] = _symbols
}
