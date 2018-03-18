package org.linnando.mixemulator.vm

import org.linnando.mixemulator.vm.Comparison.Comparison
import org.linnando.mixemulator.vm.exceptions._
import org.linnando.mixemulator.vm.io.Device
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object decimal extends ProcessingModel {
  override type RS = RegisterState
  override type MS = MemoryState
  override type B = MixByte
  override type I = MixIndex
  override type W = MixWord
  override type DW = MixDWord
  override type VMB = DecimalVirtualMachineBuilder

  override def BYTE_SIZE: Byte = 100

  override def createVirtualMachineBuilder(): VirtualMachineBuilder = DecimalVirtualMachineBuilder()

  case class DecimalVirtualMachineBuilder(state: State = initialState,
                                         counter: I = MixIndex(0),
                                         symbols: Map[String, W] = Map.empty,
                                         forwardReferences: Map[String, Seq[I]] = Map.empty.withDefaultValue(Seq.empty),
                                         literals: Map[W, Seq[I]] = Map.empty.withDefaultValue(Seq.empty)) extends AbstractVirtualMachineBuilder {

    protected def withoutChanges: VMB = this

    protected def withSymbolValue(label: String, value: W): VMB =
      copy(symbols = symbols.updated(label, value), forwardReferences = forwardReferences - label)
        .withDefinedForwardReference(forwardReferences(label), value)

    protected def withAddressFields(addresses: Seq[I], addressFieldValue: W): VMB = {
      val nextState = addresses.foldLeft(state) { (s, address) =>
        val value = s.memory.get(address).updated(getByte(2), addressFieldValue)
        s.copy(memory = s.memory.updated(address, value))
      }
      copy(state = nextState)
    }

    protected def withChangedCounter(address: W): VMB =
      copy(counter = address.toIndex)

    protected def withValue(value: W): VMB = {
      val index = counter.toShort
      if (index < 0 || index >= VirtualMachine.MEMORY_SIZE)
        throw new WrongMemoryAddressException(index)
      state.memory.updated(counter, value)
      copy(
        state = state.copy(memory = state.memory.updated(counter, value)),
        counter = counter.next
      )
    }

    protected def withProgramCounter(value: I): VMB =
      copy(state = state.copy(programCounter = value))

    protected def withForwardReference(symbol: String): VMB =
      copy(forwardReferences = forwardReferences.updated(symbol, forwardReferences(symbol) :+ counter))

    protected def withLiteral(value: W): VMB =
      copy(literals = literals.updated(value, literals(value) :+ counter))

    override def withDevices(devices: Map[Int, Device]): DecimalVirtualMachineBuilder =
      copy(state = state.copy(devices = devices.mapValues((_, None))))
  }

  def initialState = State(
    registers = RegisterState.initialState,
    memory = MemoryState.initialState,
    programCounter = MixIndex(0.toShort),
    timeCounter = 0,
    isHalted = false,
    devices = Map.empty
  )

  def go(devices: Map[Int, Device], deviceNum: Int): Future[VirtualMachine] = {
    val state = initialState.copy(devices = devices.mapValues((_, None)))
    go(state, deviceNum).map(new VirtualMachineImpl(_))
  }

  def goTracking(devices: Map[Int, Device], deviceNum: Int): Future[VirtualMachine] = {
    val state = initialState.copy(devices = devices.mapValues((_, None)))
    go(state, deviceNum).map(new TrackingVirtualMachineImpl(_))
  }

  override def getByte(value: Byte): B = {
    if (value < 0 || value >= BYTE_SIZE)
      throw new OverflowException
    MixByte(value)
  }

  override def getIndex(value: Short): I = {
    val sign = if (value < 0) 0x4000 else 0
    val abs = value.abs
    if (abs >= 10000)
      throw new OverflowException
    MixIndex((sign | abs).toShort)
  }

  override def getWord(value: Long): W = {
    val sign = if (value < 0) 0x400000000L else 0L
    val abs = value.abs
    if (abs >= 10000000000L)
      throw new OverflowException
    MixWord(sign | abs)
  }

  override def getWord(address: I, indexSpec: B, fieldSpec: B, opCode: B): W = {
    val sign = (address.contents & 0x4000).toLong << 20
    val abs = ((address.contents & 0x3fff) * 1000000L) + (indexSpec.contents * 10000L) + (fieldSpec.contents * 100L) + opCode.contents
    MixWord(sign | abs)
  }

  override def getWord(ioWord: IOWord): W = {
    val sign = if (ioWord.negative) 0x400000000L else 0L
    val abs = ioWord.bytes.foldLeft(0L)((w, b) => w * BYTE_SIZE + b)
    MixWord(sign | abs)
  }

  override def getWord(chars: String): W = {
    if (chars.length != 5) throw new Error
    val value = chars.foldLeft(0L) { (w, c) =>
      VirtualMachine.CODES.get(c) match {
        case Some(code) => w * BYTE_SIZE + code
        case None => throw new UnsupportedCharacterException(c)
      }
    }
    MixWord(value)
  }

  case class RegisterState(a: Long, x: Long, i: Vector[Short], j: Short, ov: Boolean, cmp: Comparison)
    extends AbstractRegisterState {
    override def getA: W = MixWord(a)

    override def getX: W = MixWord(x)

    override def getAX: DW = MixDWord(a, x & 0x3ffffffffL)

    override def getI(index: B): I = getI(index.contents)

    override def getI(index: Int): I = {
      if (index < 1 || index > i.length) throw new WrongIndexSpecException(index.toByte)
      MixIndex(i(index - 1))
    }

    override def getJ: MixIndex = MixIndex(j)

    override def getOV: Boolean = ov

    override def getCMP: Comparison = cmp

    override def updatedA(value: W): RS = copy(a = value.contents)

    override def updatedX(value: W): RS = copy(x = value.contents)

    override def updatedAX(value: DW, xIsNegative: Boolean): RS = {
      val xSign = if (xIsNegative) 0x400000000L else 0L
      copy(a = value.left, x = xSign | value.right)
    }

    override def updatedI(index: B, value: I): RS = updatedI(index.contents, value)

    override def updatedI(index: Int, value: I): RS = {
      if (index < 1 || index > i.length) throw new WrongIndexSpecException(index.toByte)
      copy(i = i.updated(index - 1, value.contents))
    }

    override def updatedJ(value: I): RS = copy(j = value.contents)

    override def updatedOV(value: Boolean): RS = copy(ov = value)

    override def updatedCMP(value: Comparison): RS = copy(cmp = value)
  }

  object RegisterState {
    def initialState = RegisterState(
      a = 0L,
      x = 0L,
      i = Vector.fill(6)(0),
      j = 0,
      ov = false,
      cmp = Comparison.EQUAL
    )
  }

  case class MemoryState(contents: Vector[Long], sharedLocks: List[(I, Int, Int)], exclusiveLocks: List[(I, Int, Int)]) extends AbstractMemoryState {
    override def get(address: I): W = get(address.toShort)

    override def get(address: Short): W = {
      if (exclusiveLocks exists { l => conflicts(l, address) }) throw new InconsistentReadException
      getCurrent(address)
    }

    override def getCurrent(address: Short): MixWord = {
      if (address < 0 || address >= VirtualMachine.MEMORY_SIZE)
        throw new WrongMemoryAddressException(address)
      MixWord(contents(address))
    }

    private def conflicts(lock: (I, Int, Int), address: Short, size: Int = 1) =
      address < lock._1.toShort + lock._2 && lock._1.toShort < address + size

    override def updated(address: I, value: W): MS = {
      val index = address.toShort
      if (index < 0 || index >= VirtualMachine.MEMORY_SIZE)
        throw new WrongMemoryAddressException(index)
      if (sharedLocks exists { l => conflicts(l, index) }) throw new InconsistentReadException
      if (exclusiveLocks exists { l => conflicts(l, index) }) throw new WriteConflictException
      copy(contents = contents.updated(index, value.contents))
    }

    override def withSharedLock(address: I, size: Int, deviceNum: Int): MS = {
      val index = address.toShort
      if (index < 0 || index + size > VirtualMachine.MEMORY_SIZE)
        throw new WrongMemoryAddressException(index)
      if (exclusiveLocks exists { l => conflicts(l, index, size) }) throw new InconsistentReadException
      copy(sharedLocks = (address, size, deviceNum) :: sharedLocks)
    }

    override def withExclusiveLock(address: I, size: Int, deviceNum: Int): MS = {
      val index = address.toShort
      if (index < 0 || index + size > VirtualMachine.MEMORY_SIZE)
        throw new WrongMemoryAddressException(index)
      if (sharedLocks exists { l => conflicts(l, index, size) }) throw new InconsistentReadException
      if (exclusiveLocks exists { l => conflicts(l, index, size) }) throw new WriteConflictException
      copy(exclusiveLocks = (address, size, deviceNum) :: exclusiveLocks)
    }

    override def withoutLocks(deviceNum: Int): MS = copy(
      sharedLocks = sharedLocks filterNot { l => l._3 == deviceNum },
      exclusiveLocks = exclusiveLocks filterNot { l => l._3 == deviceNum }
    )
  }

  object MemoryState {
    def initialState = MemoryState(
      contents = Vector.fill(VirtualMachine.MEMORY_SIZE)(0L),
      sharedLocks = List.empty,
      exclusiveLocks = List.empty
    )
  }

  case class MixByte(contents: Byte) extends AbstractMixByte {
    override def toInt: Int = contents.toInt

    override def toByte: Byte = contents

    override def isZero: Boolean = contents == 0
  }

  case class MixIndex(contents: Short) extends AbstractMixIndex {
    override def isPositive: Boolean = (contents & 0x4000) == 0

    override def isNegative: Boolean = (contents & 0x4000) > 0

    override def unary_-(): MixIndex = MixIndex((contents ^ 0x4000).toShort)

    override def +(other: I): I =
      if ((contents & 0x4000) == (other.contents & 0x4000)) {
        val abs = (contents & 0x3fff) + (other.contents & 0x3fff)
        if (abs >= 10000) throw new OverflowException
        MixIndex((contents & 0x4000 | abs).toShort)
      }
      else if ((contents & 0x3fff) >= (other.contents & 0x3fff)) {
        val abs = (contents & 0x3fff) - (other.contents & 0x3fff)
        MixIndex((contents & 0x4000 | abs).toShort)
      }
      else {
        val abs = (other.contents & 0x3fff) - (contents & 0x3fff)
        MixIndex((other.contents & 0x4000 | abs).toShort)
      }

    override def +(other: Int): I = {
      if (other < 0) throw new Error
      if (isPositive) {
        val abs = (contents & 0x3fff) + other
        if (abs >= 10000) throw new OverflowException
        MixIndex(abs.toShort)
      }
      else if ((contents & 0x3fff) >= other) {
        val abs = (contents & 0x3fff) - other
        MixIndex((0x4000 | abs).toShort)
      }
      else {
        val abs = other - (contents & 0x3fff)
        MixIndex(abs.toShort)
      }
    }

    override def -(subtrahend: I): I =
      if ((contents & 0x4000) == (subtrahend.contents & 0x4000))
        if ((contents & 0x3fff) >= (subtrahend.contents & 0x3fff)) {
          val abs = (contents & 0x3fff) - (subtrahend.contents & 0x3fff)
          MixIndex((contents & 0x4000 | abs).toShort)
        }
        else {
          val abs = (subtrahend.contents & 0x3fff) - (contents & 0x3fff)
          MixIndex((~contents & 0x4000 | abs).toShort)
        }
      else {
        val abs = (contents & 0x3fff) + (subtrahend.contents & 0x3fff)
        if (abs >= 10000) throw new OverflowException
        MixIndex((contents & 0x4000 | abs).toShort)
      }

    override def <=>(other: W): Comparison = {
      if (((contents & 0x4000).toLong << 20) == (other.contents & 0x400000000L))
        if ((contents & 0x3fff) == (other.contents & 0x3ffffffffL)) Comparison.EQUAL
        else if (isPositive && (contents & 0x3fff) > (other.contents & 0x3ffffffffL)) Comparison.GREATER
        else if (isNegative && (contents & 0x3fff) < (other.contents & 0x3ffffffffL)) Comparison.GREATER
        else Comparison.LESS
      else if ((contents & 0x3fff) == 0 && (other.contents & 0x3ffffffffL) == 0) Comparison.EQUAL
      else if (isPositive) Comparison.GREATER
      else Comparison.LESS
    }

    override def next: I = {
      if (isNegative && contents != 0x4000) throw new Error
      if (contents == 0x4000) MixIndex(1)
      else {
        val nextIndex = contents + 1
        if (nextIndex >= 10000) throw new OverflowException
        MixIndex(nextIndex.toShort)
      }
    }

    override def toShort: Short =
      if (isPositive) contents
      else (-(contents ^ 0x4000)).toShort

    override def toWord: W = MixWord((contents & 0x4000).toLong << 20 | (contents & 0x3fff))

    override def toIOWord: IOWord = {
      val abs = contents & 0x3fff
      val bytes = List[Byte](0, 0, 0, (abs / 100).toByte, (abs % 100).toByte)
      IOWord(isNegative, bytes)
    }
  }

  case class MixWord(contents: Long) extends AbstractMixWord {
    override def isPositive: Boolean = (contents & 0x400000000L) == 0

    override def isNegative: Boolean = (contents & 0x400000000L) > 0

    override def getAddress: I = {
      val sign = (contents & 0x400000000L) >> 20
      val abs = (contents & 0x3ffffffffL) / 1000000L % 10000L
      MixIndex((sign | abs).toShort)
    }

    override def getIndexSpec: B ={
      val byte = (contents & 0x3ffffffffL) / 10000L % 100L
      MixByte(byte.toByte)
    }

    override def getFieldSpec: B = {
      val byte = (contents & 0x3ffffffffL) / 100L % 100L
      MixByte(byte.toByte)
    }

    override def getOpCode: B = {
      val byte = (contents & 0x3ffffffffL) % 100L
      MixByte(byte.toByte)
    }

    override def getField(fieldSpec: B): W = {
      val l = fieldSpec.contents >> 3
      val r = fieldSpec.contents & 0x07
      if (l > 5 || r > 5 || l > r) throw new WrongFieldSpecException(fieldSpec.contents)
      val sign = if (l == 0) contents & 0x400000000L else 0L
      val shifted = (0 until 5 - r).foldLeft(contents & 0x3ffffffffL) { (n, _) => n / 100L }
      val fieldLength = if (l == 0) r else r - l + 1
      val fieldSize = (0 until fieldLength).foldLeft(1L) { (p, _) => p * 100L }
      val abs = shifted % fieldSize
      MixWord(sign | abs)
    }

    override def updated(fieldSpec: B, value: W): W = {
      val l = fieldSpec.contents >> 3
      val r = fieldSpec.contents & 0x07
      if (l > 5 || r > 5 || l > r) throw new WrongFieldSpecException(fieldSpec.contents)
      val rightSize = (0 until 5 - r).foldLeft(1L) { (n, _) => n * 100L }
      val fieldLength = if (l == 0) r else r - l + 1
      val fieldSize = (0 until fieldLength).foldLeft(1L) { (n, _) => n * 100L }
      val sign = (if (l == 0) value.contents else contents) & 0x400000000L
      val absCurrent = contents & 0x3ffffffffL
      val absValue = value.contents & 0x3ffffffffL
      val abs = absCurrent - absCurrent % (fieldSize * rightSize) +
        absValue % fieldSize * rightSize + absCurrent % rightSize
      MixWord(sign | abs)
    }

    override def unary_-(): W = MixWord(contents ^ 0x400000000L)

    override def +(other: W): (Boolean, W) =
      if ((contents & 0x400000000L) == (other.contents & 0x400000000L)) {
        val abs = (contents & 0x3ffffffffL) + (other.contents & 0x3ffffffffL)
        (abs >= 10000000000L, MixWord(contents & 0x400000000L | abs % 10000000000L))
      }
      else if ((contents & 0x3ffffffffL) >= (other.contents & 0x3ffffffffL)) {
        val abs = (contents & 0x3ffffffffL) - (other.contents & 0x3ffffffffL)
        (false, MixWord(contents & 0x400000000L | abs))
      }
      else {
        val abs = (other.contents & 0x3ffffffffL) - (contents & 0x3ffffffffL)
        (false, MixWord(other.contents & 0x400000000L | abs))
      }

    override def -(subtrahend: W): (Boolean, W) =
      if ((contents & 0x400000000L) == (subtrahend.contents & 0x400000000L))
        if ((contents & 0x3ffffffffL) >= (subtrahend.contents & 0x3ffffffffL)) {
          val abs = (contents & 0x3ffffffffL) - (subtrahend.contents & 0x3ffffffffL)
          (false, MixWord(contents & 0x400000000L | abs))
        }
        else {
          val abs = (subtrahend.contents & 0x3ffffffffL) - (contents & 0x3ffffffffL)
          (false, MixWord(~contents & 0x400000000L | abs))
        }
      else {
        val abs = (contents & 0x3ffffffffL) + (subtrahend.contents & 0x3ffffffffL)
        (abs >= 10000000000L, MixWord((contents & 0x400000000L) | (abs % 10000000000L)))
      }

    override def *(other: W): DW = {
      val sign = (contents & ~other.contents | ~contents & other.contents) & 0x400000000L
      val abs = BigInt(contents & 0x3ffffffffL) * BigInt(other.contents & 0x3ffffffffL) /% BigInt(10000000000L)
      MixDWord(sign | abs._1.toLong, abs._2.toLong)
    }

    override def /(divisor: W): (W, W) = {
      if ((divisor.contents & 0x3ffffffffL) == 0)
        throw new DivisionByZeroException
      val absDividend = contents & 0x3ffffffffL
      val absDivisor = divisor.contents & 0x3ffffffffL
      val absQuotient = absDividend / absDivisor
      val absRemainder = absDividend % absDivisor
      val signQuotient = (contents & ~divisor.contents | ~contents & divisor.contents) & 0x400000000L
      (MixWord(signQuotient | absQuotient), MixWord(contents & 0x400000000L | absRemainder))
    }

    override def /\(divisor: W): (W, W) = {
      if ((divisor.contents & 0x3ffffffffL) == 0)
        throw new DivisionByZeroException
      if ((contents & 0x3ffffffffL) >= (divisor.contents & 0x3ffffffffL))
        throw new OverflowException
      val absDividend = BigInt(contents & 0x3ffffffffL) * BigInt(10000000000L)
      val absDivisor = BigInt(divisor.contents & 0x3ffffffffL)
      val (absQuotient, absRemainder) = absDividend /% absDivisor
      val signQuotient = (contents & ~divisor.contents | ~contents & divisor.contents) & 0x400000000L
      (MixWord(signQuotient | absQuotient.toLong), MixWord(contents & 0x400000000L | absRemainder.toLong))
    }

    override def :*(other: W): W = {
      val shiftedLeft = MixWord(contents & 0x400000000L | ((contents & 0x07fffffffL) << 3))
      (shiftedLeft + other)._2
    }

    override def <=>(other: W): Comparison =
      if ((contents & 0x400000000L) == (other.contents & 0x400000000L))
        if ((contents & 0x3ffffffffL) == (other.contents & 0x3ffffffffL)) Comparison.EQUAL
        else if (isPositive && (contents & 0x3ffffffffL) > (other.contents & 0x3ffffffffL)) Comparison.GREATER
        else if (isNegative && (contents & 0x3ffffffffL) < (other.contents & 0x3ffffffffL)) Comparison.GREATER
        else Comparison.LESS
      else if ((contents & 0x3ffffffffL) == 0 && (other.contents & 0x3ffffffffL) == 0) Comparison.EQUAL
      else if (isPositive) Comparison.GREATER
      else Comparison.LESS

    override def <<(n: I): W = {
      val abs = (0 until n.contents).foldLeft(contents & 0x3ffffffffL) { (w, _) => (w % 100000000L) * 100L }
      MixWord(contents & 0x400000000L | abs)
    }

    override def >>(n: MixIndex): MixWord = {
      val abs = (0 until n.contents).foldLeft(contents & 0x3ffffffffL) { (w, _) => w / 100L }
      MixWord(contents & 0x400000000L | abs)
    }

    override def toByte: MixByte = {
      if ((contents & 0x400000000L) > 0) throw new OverflowException
      if ((contents & 0x3ffffffffL) >= 100L) throw new OverflowException
      MixByte((contents & 0x3fffffffffL).toByte)
    }

    override def toIndex: I = {
      if ((contents & 0x3ffffffffL) >= 10000L) throw new OverflowException
      MixIndex(((contents & 0x400000000L) >> 20 | (contents & 0x3fffffffL)).toShort)
    }

    override def toLong: Long = {
      if (isPositive) contents
      else -(contents ^ 0x400000000L)
    }

    override def toIOWord: IOWord = IOWord(isNegative, bytes)

    private[decimal] def bytes: Seq[Byte] = {
      (0 until 4).foldLeft(List(contents & 0x3ffffffffL))((bs, _) =>
        (bs.head / 100) :: (bs.head % 100) :: bs.tail
      ).map(_.toByte)
    }

    override def toCharCode: MixDWord = {
      val (leftDigits, rightDigits) = (0 until 9).foldLeft(List(contents & 0x3ffffffffL)) { (ds, _) =>
        (ds.head / 10) :: (ds.head % 10) :: ds.tail
      }.map(_ + 30L).splitAt(5)
      val left = leftDigits.reduceLeft { (n, d) => n * 100L + d }
      val right = rightDigits.reduceLeft { (n, d) => n * 100L + d }
      MixDWord(contents & 0x400000000L | left, right)
    }
  }

  case class MixDWord(left: Long, right: Long) extends AbstractMixDWord {
    override def isPositive: Boolean = (left & 0x400000000L) == 0

    override def isNegative: Boolean = (left & 0x400000000L) > 0

    override def /(divisor: W): (W, W) = {
      if ((divisor.contents & 0x3ffffffffL) == 0)
        throw new DivisionByZeroException
      if ((left & 0x3ffffffffL) >= (divisor.contents & 0x3ffffffffL))
        throw new OverflowException
      val absDividend = BigInt(left & 0x3ffffffffL) * BigInt(10000000000L) + BigInt(right & 0x3ffffffffL)
      val absDivisor = BigInt(divisor.contents & 0x3ffffffffL)
      val (absQuotient, absRemainder) = absDividend /% absDivisor
      val signQuotient = (left & ~divisor.contents | ~left & divisor.contents) & 0x400000000L
      (MixWord(signQuotient | absQuotient.toLong), MixWord(left & 0x400000000L | absRemainder.toLong))
    }

    override def <<(n: I): DW = {
      val abs = (0 until n.contents).foldLeft((left & 0x3ffffffffL, right)) { (s, _) =>
        ((s._1 % 100000000L) * 100L + s._2 / 100000000L, (s._2 % 100000000L) * 100L)
      }
      MixDWord(left & 0x400000000L | abs._1, abs._2)
    }

    override def >>(n: I): DW = {
      val abs = (0 until n.contents).foldLeft((left & 0x3ffffffffL, right)) { (s, _) =>
        (s._1 / 100L, s._1 % 100L * 100000000L + s._2 / 100L)
      }
      MixDWord(left & 0x400000000L | abs._1, abs._2)
    }

    override def <<|(n: I): DW = {
      val abs = (0 until n.contents % 5).foldLeft((left & 0x3ffffffffL, right)) { (s, _) =>
        ((s._1 % 100000000L) * 100L + s._2 / 100000000L, (s._2 % 100000000L) * 100L + s._1 / 100000000L)
      }
      MixDWord(left & 0x400000000L | abs._1, abs._2)
    }

    override def >>|(n: I): DW = {
      val abs = (0 until n.contents % 5).foldLeft((left & 0x3ffffffffL, right)) { (s, _) =>
        (s._2 % 100L * 100000000L + s._1 / 100L, s._1 % 100L * 100000000L + s._2 / 100L)
      }
      MixDWord(left & 0x400000000L | abs._1, abs._2)
    }

    override def charToNumber: MixWord = {
      val leftNumber = MixWord(left).bytes.foldLeft(0L) { (n, b) => n * 10L + b % 10 }
      val number = MixWord(right).bytes.foldLeft(leftNumber) { (n, b) => n * 10L + b % 10 }
      MixWord(left & 0x400000000L | number)
    }

    override def toWord: MixWord = {
      if ((left & 0x3ffffffffL) > 0) throw new OverflowException
      MixWord(left & 0x400000000L | right)
    }
  }
}
