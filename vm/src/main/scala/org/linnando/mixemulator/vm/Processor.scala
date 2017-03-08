package org.linnando.mixemulator.vm

import org.linnando.mixemulator.vm.exceptions.{HaltException, UnpredictableExecutionFlowException}
import org.linnando.mixemulator.vm.io._

import scala.collection.immutable.Queue

trait Processor {
  this: DataModel =>

  val commands: Array[(State, W) => State] = Array(
    nop, add, sub, mul, div, c5, c6, move,          // 0..7
    lda, ld1, ld2, ld3, ld4, ld5, ld6, ldx,         // 8..15
    ldan, ld1n, ld2n, ld3n, ld4n, ld5n, ld6n, ldxn, // 16..23
    sta, st1, st2, st3, st4, st5, st6, stx,         // 24..31
    stj, stz, jbus, ioc, in, out, jred, c39,        // 32..39
    c40, c41, c42, c43, c44, c45, c46, c47,         // 40..47
    c48, c49, c50, c51, c52, c53, c54, c55,         // 48..55
    cmpa, cmp1, cmp2, cmp3, cmp4, cmp5, cmp6, cmpx  // 56..63
  )

  val commands5: Array[(State, W) => State] = Array(num, char, hlt)
  val commands6: Array[(State, W) => State] = Array(sla, sra, slax, srax, slc, src)
  val commands39: Array[(State, W) => State] = Array(jmp, jsj, jov, jnov, jl, je, jg, jge, jne, jle)
  val commands40: Array[(State, W) => State] = Array(jan, jaz, jap, jann, janz, janp)
  val commands41: Array[(State, W) => State] = Array(j1n, j1z, j1p, j1nn, j1nz, j1np)
  val commands42: Array[(State, W) => State] = Array(j2n, j2z, j2p, j2nn, j2nz, j2np)
  val commands43: Array[(State, W) => State] = Array(j3n, j3z, j3p, j3nn, j3nz, j3np)
  val commands44: Array[(State, W) => State] = Array(j4n, j4z, j4p, j4nn, j4nz, j4np)
  val commands45: Array[(State, W) => State] = Array(j5n, j5z, j5p, j5nn, j5nz, j5np)
  val commands46: Array[(State, W) => State] = Array(j6n, j6z, j6p, j6nn, j6nz, j6np)
  val commands47: Array[(State, W) => State] = Array(jxn, jxz, jxp, jxnn, jxnz, jxnp)
  val commands48: Array[(State, W) => State] = Array(inca, deca, enta, enna)
  val commands49: Array[(State, W) => State] = Array(inc1, dec1, ent1, enn1)
  val commands50: Array[(State, W) => State] = Array(inc2, dec2, ent2, enn2)
  val commands51: Array[(State, W) => State] = Array(inc3, dec3, ent3, enn3)
  val commands52: Array[(State, W) => State] = Array(inc4, dec4, ent4, enn4)
  val commands53: Array[(State, W) => State] = Array(inc5, dec5, ent5, enn5)
  val commands54: Array[(State, W) => State] = Array(inc6, dec6, ent6, enn6)
  val commands55: Array[(State, W) => State] = Array(incx, decx, entx, ennx)

  def forward(state: State): State = {
    val command = state.memory.get(state.programCounter)
    execute(state, command)
  }

  def execute(state: State, command: W): State = {
    val opCode = command.getOpCode
    commands(opCode.toInt)(state, command)
  }

  // C = 00
  def nop(state: State, command: W = getZero): State = {
    state.copy(
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 01
  def add(state: State, command: W): State = {
    val contents = getMemoryWord(state, command)
    val sum = state.registers.getA + contents
    val updatedRegisters = (
      if (sum._1) state.registers.updatedOV(true)
      else state.registers
    ).updatedA(sum._2)
    state.copy(
      registers = updatedRegisters,
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  def getMemoryWord(state: State, command: W): W = {
    val address = getIndexedAddress(state, command)
    val fieldSpec = command.getFieldSpec
    state.memory.get(address).getField(fieldSpec)
  }

  def getIndexedAddress(state: State, command: W): I = {
    val address = command.getAddress
    val indexSpec = command.getIndexSpec
    if (indexSpec.isZero) address else address + state.registers.getI(indexSpec)
  }

  // C = 02
  def sub(state: State, command: W): State = {
    val contents = getMemoryWord(state, command)
    val difference = state.registers.getA - contents
    val updatedRegisters = (
      if (difference._1) state.registers.updatedOV(true)
      else state.registers
    ).updatedA(difference._2)
    state.copy(
      registers = updatedRegisters,
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 03
  def mul(state: State, command: W): State = {
    val contents = getMemoryWord(state, command)
    val product = state.registers.getA * contents
    state.copy(
      registers = state.registers.updatedAX(product, product.isNegative),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 10
    )
  }

  // C = 04
  def div(state: State, command: W): State = {
    val contents = getMemoryWord(state, command)
    val quotientAndRemainder = state.registers.getAX / contents
    state.copy(
      registers = state.registers.updatedA(quotientAndRemainder._1).updatedX(quotientAndRemainder._2),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 12
    )
  }

  // C = 05
  def c5(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands5(modifier.toInt)(state, command)
  }

  // C = 05, F = 0
  def num(state: State, command: W): State = {
    val charCode = state.registers.getAX
    state.copy(
      registers = state.registers.updatedA(charCode.charToNumber),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 10
    )
  }

  // C = 05, F = 1
  def char(state: State, command: W): State = {
    val number = state.registers.getA
    state.copy(
      registers = state.registers.updatedAX(number.toCharCode, state.registers.getX.isNegative),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 10
    )
  }

  // C = 05, F = 2
  def hlt(state: State, command: W): State = {
    throw new HaltException
  }

  // C = 06
  def c6(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands6(modifier.toInt)(state, command)
  }

  // C = 06, F = 0
  def sla(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val shifted = state.registers.getA << value
    state.copy(
      registers = state.registers.updatedA(shifted),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 06, F = 1
  def sra(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val shifted = state.registers.getA >> value
    state.copy(
      registers = state.registers.updatedA(shifted),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 06, F = 2
  def slax(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val shifted = state.registers.getAX << value
    state.copy(
      registers = state.registers.updatedAX(shifted, state.registers.getX.isNegative),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 06, F = 3
  def srax(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val shifted = state.registers.getAX >> value
    state.copy(
      registers = state.registers.updatedAX(shifted, state.registers.getX.isNegative),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 06, F = 4
  def slc(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val shifted = state.registers.getAX <<| value
    state.copy(
      registers = state.registers.updatedAX(shifted, state.registers.getX.isNegative),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 06, F = 5
  def src(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val shifted = state.registers.getAX >>| value
    state.copy(
      registers = state.registers.updatedAX(shifted, state.registers.getX.isNegative),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 07
  def move(state: State, command: W): State = {
    val source = getIndexedAddress(state, command)
    val destination = state.registers.getI(1)
    val n = command.getFieldSpec.toInt
    val updatedMemory = (0 until n).foldLeft(state.memory)((s, i) => {
      val src = s.get(source + i)
      s.updated(destination + i, src)
    })
    state.copy(
      memory = updatedMemory,
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1 + 2 * n
    )
  }

  // C = 08
  def lda(state: State, command: W): State = {
    val contents = getMemoryWord(state, command)
    state.copy(
      registers = state.registers.updatedA(contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 09
  def ld1(state: State, command: W): State = {
    val contents = getMemoryIndex(state, command)
    state.copy(
      registers = state.registers.updatedI(1, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  def getMemoryIndex(state: State, command: W): I = getMemoryWord(state, command).toIndex

  // C = 10
  def ld2(state: State, command: W): State = {
    val contents = getMemoryIndex(state, command)
    state.copy(
      registers = state.registers.updatedI(2, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 11
  def ld3(state: State, command: W): State = {
    val contents = getMemoryIndex(state, command)
    state.copy(
      registers = state.registers.updatedI(3, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 12
  def ld4(state: State, command: W): State = {
    val contents = getMemoryIndex(state, command)
    state.copy(
      registers = state.registers.updatedI(4, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 13
  def ld5(state: State, command: W): State = {
    val contents = getMemoryIndex(state, command)
    state.copy(
      registers = state.registers.updatedI(5, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 14
  def ld6(state: State, command: W): State = {
    val contents = getMemoryIndex(state, command)
    state.copy(
      registers = state.registers.updatedI(6, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 15
  def ldx(state: State, command: W): State = {
    val contents = getMemoryWord(state, command)
    state.copy(
      registers = state.registers.updatedX(contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 16
  def ldan(state: State, command: W): State = {
    val contents = getMemoryWord(state, command)
    state.copy(
      registers = state.registers.updatedA(-contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 17
  def ld1n(state: State, command: W): State = {
    val contents = getMemoryIndex(state, command)
    state.copy(
      registers = state.registers.updatedI(1, -contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 18
  def ld2n(state: State, command: W): State = {
    val contents = getMemoryIndex(state, command)
    state.copy(
      registers = state.registers.updatedI(2, -contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 19
  def ld3n(state: State, command: W): State = {
    val contents = getMemoryIndex(state, command)
    state.copy(
      registers = state.registers.updatedI(3, -contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 20
  def ld4n(state: State, command: W): State = {
    val contents = getMemoryIndex(state, command)
    state.copy(
      registers = state.registers.updatedI(4, -contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 21
  def ld5n(state: State, command: W): State = {
    val contents = getMemoryIndex(state, command)
    state.copy(
      registers = state.registers.updatedI(5, -contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 22
  def ld6n(state: State, command: W): State = {
    val contents = getMemoryIndex(state, command)
    state.copy(
      registers = state.registers.updatedI(6, -contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 23
  def ldxn(state: State, command: W): State = {
    val contents = getMemoryWord(state, command)
    state.copy(
      registers = state.registers.updatedX(-contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 24
  def sta(state: State, command: W): State = {
    val contents = state.registers.getA
    state.copy(
      memory = setMemoryContents(state, command, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  def setMemoryContents(state: State, command: W, contents: W): MS = {
    val address = getIndexedAddress(state, command)
    val fieldSpec = command.getFieldSpec
    state.memory.updated(address, fieldSpec, contents)
  }

  // C = 25
  def st1(state: State, command: W): State = {
    val contents = state.registers.getI(1)
    state.copy(
      memory = setMemoryContents(state, command, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  def setMemoryContents(state: State, command: W, contents: I): MS = {
    setMemoryContents(state, command, contents.toWord)
  }

  // C = 26
  def st2(state: State, command: W): State = {
    val contents = state.registers.getI(2)
    state.copy(
      memory = setMemoryContents(state, command, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 27
  def st3(state: State, command: W): State = {
    val contents = state.registers.getI(3)
    state.copy(
      memory = setMemoryContents(state, command, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 28
  def st4(state: State, command: W): State = {
    val contents = state.registers.getI(4)
    state.copy(
      memory = setMemoryContents(state, command, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 29
  def st5(state: State, command: W): State = {
    val contents = state.registers.getI(5)
    state.copy(
      memory = setMemoryContents(state, command, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 30
  def st6(state: State, command: W): State = {
    val contents = state.registers.getI(6)
    state.copy(
      memory = setMemoryContents(state, command, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 31
  def stx(state: State, command: W): State = {
    val contents = state.registers.getX
    state.copy(
      memory = setMemoryContents(state, command, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 32
  def stj(state: State, command: W): State = {
    val contents = state.registers.getJ
    state.copy(
      memory = setMemoryContents(state, command, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 33
  def stz(state: State, command: W): State = {
    val contents = getZero
    state.copy(
      memory = setMemoryContents(state, command, contents),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 34
  def jbus(state: State, command: W): State = {
    val deviceNum = command.getFieldSpec.toInt
    val device = state.devices(deviceNum)
    if (device._1.isBusy) {
      val address = getIndexedAddress(state, command)
      if (address == state.programCounter) {
        val flushedDevice = device._1.flush()
        val readBlocks = flushedDevice._2 zip device._2
        val updatedMemory = readBlocks.foldLeft(state.memory) { (ms, block) =>
          block._1.indices.foldLeft(ms) { (s, i) => s.updated(block._2 + i, getWord(block._1(i))) }
        }
        val updatedDevice = (flushedDevice._1, Queue.empty)
        state.copy(
          memory = updatedMemory.withoutLocks(deviceNum),
          programCounter = state.programCounter.next,
          timeCounter = state.timeCounter + 1,
          devices = state.devices.updated(deviceNum, updatedDevice)
        )
      }
      else throw new UnpredictableExecutionFlowException
    }
    else nop(state)
  }

  // C = 35
  def ioc(state: State, command: W): State = {
    val deviceNum = command.getFieldSpec.toInt
    val device = state.devices(deviceNum)
    val updatedDevice = (
      device._1 match {
        case d: TapeUnit => d.positioned(getIndexedAddress(state, command).toWord.toLong)
        case d: DiskUnit => d.positioned(state.registers.getX.toLong)
        case d: LinePrinter => d.newPage()
        case d: PaperTape => d.reset()
        case _ => throw new UnsupportedOperationException
      },
      device._2
    )
    state.copy(
      devices = state.devices.updated(deviceNum, updatedDevice),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 36
  def in(state: State, command: W): State = {
    val deviceNum = command.getFieldSpec.toInt
    val destination = getIndexedAddress(state, command)
    val device = state.devices(deviceNum)
    val updatedDevice = (
      device._1 match {
        case d: PositionalInputDevice => d.read()
        case d: RandomAccessIODevice => d.read(state.registers.getX.toLong)
        case _ => throw new UnsupportedOperationException
      },
      device._2.enqueue(destination)
    )
    state.copy(
      memory = state.memory.withExclusiveLock(destination, device._1.blockSize, deviceNum),
      devices = state.devices.updated(deviceNum, updatedDevice),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 37
  def out(state: State, command: W): State = {
    val deviceNum = command.getFieldSpec.toInt
    val source = getIndexedAddress(state, command)
    val device = state.devices(deviceNum)
    val block = (0 until device._1.blockSize) map { i => state.memory.get(source + i).toIOWord }
    val updatedDevice = (
      device._1 match {
        case d: PositionalOutputDevice => d.write(block)
        case d: RandomAccessIODevice => d.write(state.registers.getX.toLong, block)
        case _ => throw new UnsupportedOperationException
      },
      device._2
    )
    state.copy(
      memory = state.memory.withSharedLock(source, device._1.blockSize, deviceNum),
      devices = state.devices.updated(deviceNum, updatedDevice),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 38
  def jred(state: State, command: W): State = {
    val deviceNum = command.getFieldSpec.toInt
    val device = state.devices(deviceNum)
    if (device._1.isBusy) throw new UnpredictableExecutionFlowException
    else jmp(state, command)
  }

  // C = 39
  def c39(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands39(modifier.toInt)(state, command)
  }

  // C = 39, F = 0
  def jmp(state: State, command: W): State = {
    val address = getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedJ(state.programCounter.next),
      programCounter = address,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 39, F = 1
  def jsj(state: State, command: W): State = {
    val address = getIndexedAddress(state, command)
    state.copy(
      programCounter = address,
      timeCounter = state.timeCounter + 1)
  }

  // C = 39, F = 2
  def jov(state: State, command: W): State = {
    if (state.registers.getOV) {
      val stateWithResetOV = state.copy(registers = state.registers.updatedOV(false))
      jmp(stateWithResetOV, command)
    }
    else nop(state)
  }

  // C = 39, F = 3
  def jnov(state: State, command: W): State = {
    if (state.registers.getOV) nop(state)
    else jmp(state, command)
  }

  // C = 39, F = 4
  def jl(state: State, command: W): State = {
    if (state.registers.getCMP == Comparison.LESS) jmp(state, command)
    else nop(state)
  }

  // C = 39, F = 5
  def je(state: State, command: W): State = {
    if (state.registers.getCMP == Comparison.EQUAL) jmp(state, command)
    else nop(state)
  }

  // C = 39, F = 6
  def jg(state: State, command: W): State = {
    if (state.registers.getCMP == Comparison.GREATER) jmp(state, command)
    else nop(state)
  }

  // C = 39, F = 7
  def jge(state: State, command: W): State = {
    if (state.registers.getCMP == Comparison.LESS) nop(state)
    else jmp(state, command)
  }

  // C = 39, F = 8
  def jne(state: State, command: W): State = {
    if (state.registers.getCMP == Comparison.EQUAL) nop(state)
    else jmp(state, command)
  }

  // C = 39, F = 9
  def jle(state: State, command: W): State = {
    if (state.registers.getCMP == Comparison.GREATER) nop(state)
    else jmp(state, command)
  }

  // C = 40
  def c40(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands40(modifier.toInt)(state, command)
  }

  // C = 40, F = 0
  def jan(state: State, command: W): State = {
    if ((state.registers.getA <=> getZero) == Comparison.LESS) jmp(state, command)
    else nop(state)
  }

  // C = 40, F = 1
  def jaz(state: State, command: W): State = {
    if ((state.registers.getA <=> getZero) == Comparison.EQUAL) jmp(state, command)
    else nop(state)
  }

  // C = 40, F = 2
  def jap(state: State, command: W): State = {
    if ((state.registers.getA <=> getZero) == Comparison.GREATER) jmp(state, command)
    else nop(state)
  }

  // C = 40, F = 3
  def jann(state: State, command: W): State = {
    if ((state.registers.getA <=> getZero) == Comparison.LESS) nop(state)
    else jmp(state, command)
  }

  // C = 40, F = 4
  def janz(state: State, command: W): State = {
    if ((state.registers.getA <=> getZero) == Comparison.EQUAL) nop(state)
    else jmp(state, command)
  }

  // C = 40, F = 5
  def janp(state: State, command: W): State = {
    if ((state.registers.getA <=> getZero) == Comparison.GREATER) nop(state)
    else jmp(state, command)
  }

  // C = 41
  def c41(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands41(modifier.toInt)(state, command)
  }

  // C = 41, F = 0
  def j1n(state: State, command: W): State = {
    if ((state.registers.getI(1) <=> getZero) == Comparison.LESS) jmp(state, command)
    else nop(state)
  }

  // C = 41, F = 1
  def j1z(state: State, command: W): State = {
    if ((state.registers.getI(1) <=> getZero) == Comparison.EQUAL) jmp(state, command)
    else nop(state)
  }

  // C = 41, F = 2
  def j1p(state: State, command: W): State = {
    if ((state.registers.getI(1) <=> getZero) == Comparison.GREATER) jmp(state, command)
    else nop(state)
  }

  // C = 41, F = 3
  def j1nn(state: State, command: W): State = {
    if ((state.registers.getI(1) <=> getZero) == Comparison.LESS) nop(state)
    else jmp(state, command)
  }

  // C = 41, F = 4
  def j1nz(state: State, command: W): State = {
    if ((state.registers.getI(1) <=> getZero) == Comparison.EQUAL) nop(state)
    else jmp(state, command)
  }

  // C = 41, F = 5
  def j1np(state: State, command: W): State = {
    if ((state.registers.getI(1) <=> getZero) == Comparison.GREATER) nop(state)
    else jmp(state, command)
  }

  // C = 42
  def c42(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands42(modifier.toInt)(state, command)
  }

  // C = 42, F = 0
  def j2n(state: State, command: W): State = {
    if ((state.registers.getI(2) <=> getZero) == Comparison.LESS) jmp(state, command)
    else nop(state)
  }

  // C = 42, F = 1
  def j2z(state: State, command: W): State = {
    if ((state.registers.getI(2) <=> getZero) == Comparison.EQUAL) jmp(state, command)
    else nop(state)
  }

  // C = 42, F = 2
  def j2p(state: State, command: W): State = {
    if ((state.registers.getI(2) <=> getZero) == Comparison.GREATER) jmp(state, command)
    else nop(state)
  }

  // C = 42, F = 3
  def j2nn(state: State, command: W): State = {
    if ((state.registers.getI(2) <=> getZero) == Comparison.LESS) nop(state)
    else jmp(state, command)
  }

  // C = 42, F = 4
  def j2nz(state: State, command: W): State = {
    if ((state.registers.getI(2) <=> getZero) == Comparison.EQUAL) nop(state)
    else jmp(state, command)
  }

  // C = 42, F = 5
  def j2np(state: State, command: W): State = {
    if ((state.registers.getI(2) <=> getZero) == Comparison.GREATER) nop(state)
    else jmp(state, command)
  }

  // C = 43
  def c43(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands43(modifier.toInt)(state, command)
  }

  // C = 43, F = 0
  def j3n(state: State, command: W): State = {
    if ((state.registers.getI(3) <=> getZero) == Comparison.LESS) jmp(state, command)
    else nop(state)
  }

  // C = 43, F = 1
  def j3z(state: State, command: W): State = {
    if ((state.registers.getI(3) <=> getZero) == Comparison.EQUAL) jmp(state, command)
    else nop(state)
  }

  // C = 43, F = 2
  def j3p(state: State, command: W): State = {
    if ((state.registers.getI(3) <=> getZero) == Comparison.GREATER) jmp(state, command)
    else nop(state)
  }

  // C = 43, F = 3
  def j3nn(state: State, command: W): State = {
    if ((state.registers.getI(3) <=> getZero) == Comparison.LESS) nop(state)
    else jmp(state, command)
  }

  // C = 43, F = 4
  def j3nz(state: State, command: W): State = {
    if ((state.registers.getI(3) <=> getZero) == Comparison.EQUAL) nop(state)
    else jmp(state, command)
  }

  // C = 43, F = 5
  def j3np(state: State, command: W): State = {
    if ((state.registers.getI(3) <=> getZero) == Comparison.GREATER) nop(state)
    else jmp(state, command)
  }

  // C = 44
  def c44(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands44(modifier.toInt)(state, command)
  }

  // C = 44, F = 0
  def j4n(state: State, command: W): State = {
    if ((state.registers.getI(4) <=> getZero) == Comparison.LESS) jmp(state, command)
    else nop(state)
  }

  // C = 44, F = 1
  def j4z(state: State, command: W): State = {
    if ((state.registers.getI(4) <=> getZero) == Comparison.EQUAL) jmp(state, command)
    else nop(state)
  }

  // C = 44, F = 2
  def j4p(state: State, command: W): State = {
    if ((state.registers.getI(4) <=> getZero) == Comparison.GREATER) jmp(state, command)
    else nop(state)
  }

  // C = 44, F = 3
  def j4nn(state: State, command: W): State = {
    if ((state.registers.getI(4) <=> getZero) == Comparison.LESS) nop(state)
    else jmp(state, command)
  }

  // C = 44, F = 4
  def j4nz(state: State, command: W): State = {
    if ((state.registers.getI(4) <=> getZero) == Comparison.EQUAL) nop(state)
    else jmp(state, command)
  }

  // C = 44, F = 5
  def j4np(state: State, command: W): State = {
    if ((state.registers.getI(4) <=> getZero) == Comparison.GREATER) nop(state)
    else jmp(state, command)
  }

  // C = 45
  def c45(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands45(modifier.toInt)(state, command)
  }

  // C = 45, F = 0
  def j5n(state: State, command: W): State = {
    if ((state.registers.getI(5) <=> getZero) == Comparison.LESS) jmp(state, command)
    else nop(state)
  }

  // C = 45, F = 1
  def j5z(state: State, command: W): State = {
    if ((state.registers.getI(5) <=> getZero) == Comparison.EQUAL) jmp(state, command)
    else nop(state)
  }

  // C = 45, F = 2
  def j5p(state: State, command: W): State = {
    if ((state.registers.getI(5) <=> getZero) == Comparison.GREATER) jmp(state, command)
    else nop(state)
  }

  // C = 45, F = 3
  def j5nn(state: State, command: W): State = {
    if ((state.registers.getI(5) <=> getZero) == Comparison.LESS) nop(state)
    else jmp(state, command)
  }

  // C = 45, F = 4
  def j5nz(state: State, command: W): State = {
    if ((state.registers.getI(5) <=> getZero) == Comparison.EQUAL) nop(state)
    else jmp(state, command)
  }

  // C = 45, F = 5
  def j5np(state: State, command: W): State = {
    if ((state.registers.getI(5) <=> getZero) == Comparison.GREATER) nop(state)
    else jmp(state, command)
  }

  // C = 46
  def c46(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands46(modifier.toInt)(state, command)
  }

  // C = 46, F = 0
  def j6n(state: State, command: W): State = {
    if ((state.registers.getI(6) <=> getZero) == Comparison.LESS) jmp(state, command)
    else nop(state)
  }

  // C = 46, F = 1
  def j6z(state: State, command: W): State = {
    if ((state.registers.getI(6) <=> getZero) == Comparison.EQUAL) jmp(state, command)
    else nop(state)
  }

  // C = 46, F = 2
  def j6p(state: State, command: W): State = {
    if ((state.registers.getI(6) <=> getZero) == Comparison.GREATER) jmp(state, command)
    else nop(state)
  }

  // C = 46, F = 3
  def j6nn(state: State, command: W): State = {
    if ((state.registers.getI(6) <=> getZero) == Comparison.LESS) nop(state)
    else jmp(state, command)
  }

  // C = 46, F = 4
  def j6nz(state: State, command: W): State = {
    if ((state.registers.getI(6) <=> getZero) == Comparison.EQUAL) nop(state)
    else jmp(state, command)
  }

  // C = 46, F = 5
  def j6np(state: State, command: W): State = {
    if ((state.registers.getI(6) <=> getZero) == Comparison.GREATER) nop(state)
    else jmp(state, command)
  }

  // C = 47
  def c47(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands47(modifier.toInt)(state, command)
  }

  // C = 47, F = 0
  def jxn(state: State, command: W): State = {
    if ((state.registers.getX <=> getZero) == Comparison.LESS) jmp(state, command)
    else nop(state)
  }

  // C = 47, F = 1
  def jxz(state: State, command: W): State = {
    if ((state.registers.getX <=> getZero) == Comparison.EQUAL) jmp(state, command)
    else nop(state)
  }

  // C = 47, F = 2
  def jxp(state: State, command: W): State = {
    if ((state.registers.getX <=> getZero) == Comparison.GREATER) jmp(state, command)
    else nop(state)
  }

  // C = 47, F = 3
  def jxnn(state: State, command: W): State = {
    if ((state.registers.getX <=> getZero) == Comparison.LESS) nop(state)
    else jmp(state, command)
  }

  // C = 47, F = 4
  def jxnz(state: State, command: W): State = {
    if ((state.registers.getX <=> getZero) == Comparison.EQUAL) nop(state)
    else jmp(state, command)
  }

  // C = 47, F = 5
  def jxnp(state: State, command: W): State = {
    if ((state.registers.getX <=> getZero) == Comparison.GREATER) nop(state)
    else jmp(state, command)
  }

  // C = 48
  def c48(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands48(modifier.toInt)(state, command)
  }

  // C = 48, F = 0
  def inca(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val sum = state.registers.getA + value.toWord
    val updatedRegisters = (
      if (sum._1) state.registers.updatedOV(true)
      else state.registers
    ).updatedA(sum._2)
    state.copy(
      registers = updatedRegisters,
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 48, F = 1
  def deca(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val difference = state.registers.getA - value.toWord
    val updatedRegisters = (
      if (difference._1) state.registers.updatedOV(true)
      else state.registers
    ).updatedA(difference._2)
    state.copy(
      registers = updatedRegisters,
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 48, F = 2
  def enta(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedA(value.toWord),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 48, F = 3
  def enna(state: State, command: W): State = {
    val value = -getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedA(value.toWord),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 49
  def c49(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands49(modifier.toInt)(state, command)
  }

  // C = 49, F = 0
  def inc1(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val sum = state.registers.getI(1) + value
    state.copy(
      registers = state.registers.updatedI(1, sum),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 49, F = 1
  def dec1(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val difference = state.registers.getI(1) - value
    state.copy(
      registers = state.registers.updatedI(1, difference),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 49, F = 2
  def ent1(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedI(1, value),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 49, F = 3
  def enn1(state: State, command: W): State = {
    val value = -getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedI(1, value),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 50
  def c50(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands50(modifier.toInt)(state, command)
  }

  // C = 50, F = 0
  def inc2(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val sum = state.registers.getI(2) + value
    state.copy(
      registers = state.registers.updatedI(2, sum),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 50, F = 1
  def dec2(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val difference = state.registers.getI(2) - value
    state.copy(
      registers = state.registers.updatedI(2, difference),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 50, F = 2
  def ent2(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedI(2, value),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 50, F = 3
  def enn2(state: State, command: W): State = {
    val value = -getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedI(2, value),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 51
  def c51(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands51(modifier.toInt)(state, command)
  }

  // C = 51, F = 0
  def inc3(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val sum = state.registers.getI(3) + value
    state.copy(
      registers = state.registers.updatedI(3, sum),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 51, F = 1
  def dec3(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val difference = state.registers.getI(3) - value
    state.copy(
      registers = state.registers.updatedI(3, difference),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 51, F = 2
  def ent3(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedI(3, value),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 51, F = 3
  def enn3(state: State, command: W): State = {
    val value = -getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedI(3, value),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 52
  def c52(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands52(modifier.toInt)(state, command)
  }

  // C = 52, F = 0
  def inc4(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val sum = state.registers.getI(4) + value
    state.copy(
      registers = state.registers.updatedI(4, sum),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 52, F = 1
  def dec4(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val difference = state.registers.getI(4) - value
    state.copy(
      registers = state.registers.updatedI(4, difference),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 52, F = 2
  def ent4(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedI(4, value),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 52, F = 3
  def enn4(state: State, command: W): State = {
    val value = -getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedI(4, value),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 53
  def c53(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands53(modifier.toInt)(state, command)
  }

  // C = 53, F = 0
  def inc5(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val sum = state.registers.getI(5) + value
    state.copy(
      registers = state.registers.updatedI(5, sum),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 53, F = 1
  def dec5(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val difference = state.registers.getI(5) - value
    state.copy(
      registers = state.registers.updatedI(5, difference),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 53, F = 2
  def ent5(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedI(5, value),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 53, F = 3
  def enn5(state: State, command: W): State = {
    val value = -getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedI(5, value),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 54
  def c54(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands54(modifier.toInt)(state, command)
  }

  // C = 54, F = 0
  def inc6(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val sum = state.registers.getI(6) + value
    state.copy(
      registers = state.registers.updatedI(6, sum),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 54, F = 1
  def dec6(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val difference = state.registers.getI(6) - value
    state.copy(
      registers = state.registers.updatedI(6, difference),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 54, F = 2
  def ent6(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedI(6, value),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 54, F = 3
  def enn6(state: State, command: W): State = {
    val value = -getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedI(6, value),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 55
  def c55(state: State, command: W): State = {
    val modifier = command.getFieldSpec
    commands55(modifier.toInt)(state, command)
  }

  // C = 55, F = 0
  def incx(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val sum = state.registers.getX + value.toWord
    val updatedRegisters = (
      if (sum._1) state.registers.updatedOV(true)
      else state.registers
    ).updatedX(sum._2)
    state.copy(
      registers = updatedRegisters,
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 55, F = 1
  def decx(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    val difference = state.registers.getX - value.toWord
    val updatedRegisters = (
      if (difference._1) state.registers.updatedOV(true)
      else state.registers
    ).updatedX(difference._2)
    state.copy(
      registers = updatedRegisters,
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 55, F = 2
  def entx(state: State, command: W): State = {
    val value = getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedX(value.toWord),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 55, F = 3
  def ennx(state: State, command: W): State = {
    val value = -getIndexedAddress(state, command)
    state.copy(
      registers = state.registers.updatedX(value.toWord),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 1
    )
  }

  // C = 56
  def cmpa(state: State, command: W): State = {
    val memory = getMemoryWord(state, command)
    val fieldSpec = command.getFieldSpec
    val register = state.registers.getA.getField(fieldSpec)
    state.copy(
      registers = state.registers.updatedCMP(register <=> memory),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 57
  def cmp1(state: State, command: W): State = {
    val memory = getMemoryWord(state, command)
    val fieldSpec = command.getFieldSpec
    val register = state.registers.getI(1).toWord.getField(fieldSpec)
    state.copy(
      registers = state.registers.updatedCMP(register <=> memory),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 58
  def cmp2(state: State, command: W): State = {
    val memory = getMemoryWord(state, command)
    val fieldSpec = command.getFieldSpec
    val register = state.registers.getI(2).toWord.getField(fieldSpec)
    state.copy(
      registers = state.registers.updatedCMP(register <=> memory),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 59
  def cmp3(state: State, command: W): State = {
    val memory = getMemoryWord(state, command)
    val fieldSpec = command.getFieldSpec
    val register = state.registers.getI(3).toWord.getField(fieldSpec)
    state.copy(
      registers = state.registers.updatedCMP(register <=> memory),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 60
  def cmp4(state: State, command: W): State = {
    val memory = getMemoryWord(state, command)
    val fieldSpec = command.getFieldSpec
    val register = state.registers.getI(4).toWord.getField(fieldSpec)
    state.copy(
      registers = state.registers.updatedCMP(register <=> memory),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 61
  def cmp5(state: State, command: W): State = {
    val memory = getMemoryWord(state, command)
    val fieldSpec = command.getFieldSpec
    val register = state.registers.getI(5).toWord.getField(fieldSpec)
    state.copy(
      registers = state.registers.updatedCMP(register <=> memory),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 62
  def cmp6(state: State, command: W): State = {
    val memory = getMemoryWord(state, command)
    val fieldSpec = command.getFieldSpec
    val register = state.registers.getI(6).toWord.getField(fieldSpec)
    state.copy(
      registers = state.registers.updatedCMP(register <=> memory),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

  // C = 63
  def cmpx(state: State, command: W): State = {
    val memory = getMemoryWord(state, command)
    val fieldSpec = command.getFieldSpec
    val register = state.registers.getX.getField(fieldSpec)
    state.copy(
      registers = state.registers.updatedCMP(register <=> memory),
      programCounter = state.programCounter.next,
      timeCounter = state.timeCounter + 2
    )
  }

}
