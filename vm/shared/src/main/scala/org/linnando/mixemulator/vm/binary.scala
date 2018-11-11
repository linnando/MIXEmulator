package org.linnando.mixemulator.vm

import org.linnando.mixemulator.vm.Comparison.Comparison
import org.linnando.mixemulator.vm.exceptions._
import org.linnando.mixemulator.vm.io.Device
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object binary extends ProcessingModel {
  override type RS = RegisterState
  override type MS = MemoryState
  override type B = MixByte
  override type I = MixIndex
  override type W = MixWord
  override type DW = MixDWord
  override type VMB = BinaryVirtualMachineBuilder

  override val BYTE_SIZE: Byte = 64

  override def createVirtualMachineBuilder(): VirtualMachineBuilder = BinaryVirtualMachineBuilder()

  case class BinaryVirtualMachineBuilder(state: State = initialState,
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

    override def withDevices(devices: Map[Int, Device]): BinaryVirtualMachineBuilder =
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

  override def goNonTracking(devices: Map[Int, Device], deviceNum: Int): Future[VirtualMachine] = {
    val state = initialState.copy(devices = devices.mapValues((_, None)))
    go(state, deviceNum).map(new VirtualMachineImpl(_))
  }

  override def goTracking(devices: Map[Int, Device], deviceNum: Int): Future[VirtualMachine] = {
    val state = initialState.copy(devices = devices.mapValues((_, None)))
    go(state, deviceNum).map(new TrackingVirtualMachineImpl(_))
  }

  override def getByte(value: Byte): B = {
    if ((value & ~0x3f) != 0)
      throw new OverflowException
    MixByte(value)
  }

  override def getIndex(value: Short): I = {
    val sign = if (value < 0) 0x1000 else 0x0
    val abs = value.abs
    if ((abs & ~0x0fffL) != 0)
      throw new OverflowException
    MixIndex((sign | abs).toShort)
  }

  override def getWord(value: Long): W = {
    val sign = if (value < 0) 0x40000000 else 0x0
    val abs = value.abs
    if ((abs & ~0x3fffffffL) != 0)
      throw new OverflowException
    MixWord(sign | abs.toInt)
  }

  override def getWord(address: I, indexSpec: B, fieldSpec: B, opCode: B): W = {
    val value = (address.contents << 18) | (indexSpec.contents << 12) | (fieldSpec.contents << 6) | opCode.contents
    MixWord(value)
  }

  override def getWord(ioWord: IOWord): W = {
    val init = if (ioWord.negative) 1 else 0
    MixWord(ioWord.bytes.foldLeft(init)((w, b) => (w << 6) | b))
  }

  override def getWord(chars: String): W = {
    if (chars.length != 5) throw new Error
    val value = chars.foldLeft(0) { (w, c) =>
      VirtualMachine.CODES.get(c) match {
        case Some(code) => (w << 6) | code
        case None => throw new UnsupportedCharacterException(c)
      }
    }
    MixWord(value)
  }

  case class RegisterState(a: Int, x: Int, i: Vector[Short], j: Short, ov: Boolean, cmp: Comparison)
    extends AbstractRegisterState {
    override def getA: W = MixWord(a)

    override def getX: W = MixWord(x)

    override def getAX: DW = MixDWord((a.toLong << 30) | x & 0x3fffffff)

    override def getI(indexSpec: B): I = getI(indexSpec.contents)

    override def getI(indexSpec: Int): I = {
      if (indexSpec < 1 || indexSpec > i.length) throw new WrongIndexSpecException(indexSpec.toByte)
      MixIndex(i(indexSpec - 1))
    }

    override def getJ: I = MixIndex(j)

    override def getOV: Boolean = ov

    override def getCMP: Comparison = cmp

    override def updatedA(value: W): RS = copy(a = value.contents)

    override def updatedX(value: W): RS = copy(x = value.contents)

    override def updatedAX(value: DW, xIsNegative: Boolean): RS = {
      val xSign = if (xIsNegative) 0x40000000 else 0
      copy(a = (value.contents >> 30).toInt, x = xSign | (value.contents & 0x3fffffff).toInt)
    }

    override def updatedI(indexSpec: B, value: I): RS = updatedI(indexSpec.contents, value)

    override def updatedI(indexSpec: Int, value: I): RS = {
      if (indexSpec < 1 || indexSpec > i.length) throw new WrongIndexSpecException(indexSpec.toByte)
      copy(i = i.updated(indexSpec - 1, value.contents))
    }

    override def updatedJ(value: I): RS = copy(j = value.contents)

    override def updatedOV(value: Boolean): RS = copy(ov = value)

    override def updatedCMP(value: Comparison): RS = copy(cmp = value)
  }

  object RegisterState {
    def initialState = RegisterState(
      a = 0,
      x = 0,
      i = Vector.fill(6)(0),
      j = 0,
      ov = false,
      cmp = Comparison.EQUAL
    )
  }

  case class MemoryState(contents: Vector[Int], sharedLocks: List[(I, Int, Int)], exclusiveLocks: List[(I, Int, Int)]) extends AbstractMemoryState {
    override def get(address: I): W = get(address.toShort)

    override def get(address: Short): MixWord = {
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
      contents = Vector.fill(VirtualMachine.MEMORY_SIZE)(0),
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
    override def isPositive: Boolean = (contents & 0x1000) == 0

    override def isNegative: Boolean = (contents & 0x1000) > 0

    override def unary_-(): I = MixIndex((contents ^ 0x1000).toShort)

    override def +(other: I): I =
      if ((contents & 0x1000) == (other.contents & 0x1000)) {
        val abs = (contents & 0xfff) + (other.contents & 0xfff)
        if ((abs & 0x1000) > 0) throw new OverflowException
        MixIndex((contents & 0x1000 | abs).toShort)
      }
      else if ((contents & 0xfff) >= (other.contents & 0xfff)) {
        val abs = (contents & 0xfff) - (other.contents & 0xfff)
        MixIndex((contents & 0x1000 | abs).toShort)
      }
      else {
        val abs = (other.contents & 0xfff) - (contents & 0xfff)
        MixIndex((other.contents & 0x1000 | abs).toShort)
      }

    override def +(other: Int): I = {
      if (other < 0) throw new Error
      if (isPositive) {
        val abs = (contents & 0xfff) + other
        if ((abs & 0x1000) > 0) throw new OverflowException
        MixIndex(abs.toShort)
      }
      else if ((contents & 0xfff) >= other) {
        val abs = (contents & 0xfff) - other
        MixIndex((0x1000 | abs).toShort)
      }
      else {
        val abs = other - (contents & 0xfff)
        MixIndex(abs.toShort)
      }
    }

    override def -(subtrahend: I): I =
      if ((contents & 0x1000) == (subtrahend.contents & 0x1000))
        if ((contents & 0xfff) >= (subtrahend.contents & 0xfff)) {
          val abs = (contents & 0xfff) - (subtrahend.contents & 0xfff)
          MixIndex((contents & 0x1000 | abs).toShort)
        }
        else {
          val abs = (subtrahend.contents & 0xfff) - (contents & 0xfff)
          MixIndex((~contents & 0x1000 | abs).toShort)
        }
      else {
        val abs = (contents & 0xfff) + (subtrahend.contents & 0xfff)
        if ((abs & 0x1000) > 0) throw new OverflowException
        MixIndex((contents & 0x1000 | abs).toShort)
      }

    override def <=>(other: W): Comparison =
      if (((contents & 0x1000) << 18) == (other.contents & 0x40000000))
        if ((contents & 0xfff) == (other.contents & 0x3fffffff)) Comparison.EQUAL
        else if (isPositive && (contents & 0xfff) > (other.contents & 0x3fffffff)) Comparison.GREATER
        else if (isNegative && (contents & 0xfff) < (other.contents & 0x3fffffff)) Comparison.GREATER
        else Comparison.LESS
      else if ((contents & 0xfff) == 0 && (other.contents & 0x3fffffff) == 0) Comparison.EQUAL
      else if (isPositive) Comparison.GREATER
      else Comparison.LESS

    override def next: I = {
      if (isNegative && contents != 0x1000) throw new Error
      if (contents == 0x1000) MixIndex(0x1)
      else {
        val nextIndex = contents + 1
        if ((nextIndex & 0x1000) > 0) throw new OverflowException
        MixIndex(nextIndex.toShort)
      }
    }

    override def toShort: Short =
      if (isPositive) contents
      else (-(contents ^ 0x1000)).toShort

    override def toWord: W = MixWord((contents & 0x1000) << 18 | (contents & 0xfff))

    override def toIOWord: IOWord = {
      val bytes = Seq[Byte](0, 0, 0) ++ (1 to 2).map(i => ((contents & MixIndex.masks(i)) >> (6 * (2 - i))).toByte)
      IOWord(isNegative, bytes)
    }
  }

  object MixIndex {
    val masks: Array[Int] = Array(0x1000, 0xfc0, 0x3f)

    def bitMask(l: Int, r: Int): Int = {
      if (l > 5 || r > 5) throw new WrongFieldSpecException((8 * l + r).toByte)
      (l to r).foldLeft(0) { (m, i) => m | masks(i - 3) }
    }
  }

  case class MixWord(contents: Int) extends AbstractMixWord {
    override def isPositive: Boolean = (contents & 0x40000000) == 0

    override def isNegative: Boolean = (contents & 0x40000000) > 0

    override def getAddress: I = MixIndex(((contents & 0x7ffc0000) >> 18).toShort)

    override def getIndexSpec: B = MixByte(((contents & 0x3f000) >> 12).toByte)

    override def getFieldSpec: B = MixByte(((contents & 0xfc0) >> 6).toByte)

    override def getOpCode: B = MixByte((contents & 0x3f).toByte)

    override def getField(fieldSpec: B): W = {
      val l = fieldSpec.contents >> 3
      val r = fieldSpec.contents & 0x07
      if (l > r) throw new WrongFieldSpecException(fieldSpec.contents)
      val sign = if (l == 0) contents & 0x40000000 else 0
      val mask = if (l == 0) MixWord.bitMask(1, r) else MixWord.bitMask(l, r)
      val abs = (contents & mask) >> (6 * (5 - r))
      MixWord(sign | abs)
    }

    override def updated(fieldSpec: B, value: W): W = {
      val l = fieldSpec.contents >> 3
      val r = fieldSpec.contents & 0x07
      if (l > r) throw new WrongFieldSpecException(fieldSpec.contents)
      val shiftedValue = value.contents & 0x40000000 | ((value.contents << (6 * (5 - r))) & 0x3fffffff)
      val mask = MixWord.bitMask(l, r)
      MixWord(shiftedValue & mask | contents & ~mask & 0x7fffffff)
    }

    override def unary_-(): W = MixWord(contents ^ 0x40000000)

    override def +(other: W): (Boolean, W) =
      if ((contents & 0x40000000) == (other.contents & 0x40000000)) {
        val abs = (contents & 0x3fffffff) + (other.contents & 0x3fffffff)
        ((abs & 0x40000000) > 0, MixWord(contents & 0x40000000 | (abs & 0x3fffffff)))
      }
      else if ((contents & 0x3fffffff) >= (other.contents & 0x3fffffff)) {
        val abs = (contents & 0x3fffffff) - (other.contents & 0x3fffffff)
        (false, MixWord(contents & 0x40000000 | abs))
      }
      else {
        val abs = (other.contents & 0x3fffffff) - (contents & 0x3fffffff)
        (false, MixWord(other.contents & 0x40000000 | abs))
      }

    override def -(subtrahend: W): (Boolean, W) =
      if ((contents & 0x40000000) == (subtrahend.contents & 0x40000000))
        if ((contents & 0x3fffffff) >= (subtrahend.contents & 0x3fffffff)) {
          val abs = (contents & 0x3fffffff) - (subtrahend.contents & 0x3fffffff)
          (false, MixWord(contents & 0x40000000 | abs))
        }
        else {
          val abs = (subtrahend.contents & 0x3fffffff) - (contents & 0x3fffffff)
          (false, MixWord(~contents & 0x40000000 | abs))
        }
      else {
        val abs = (contents & 0x3fffffff) + (subtrahend.contents & 0x3fffffff)
        ((abs & 0x40000000) > 0, MixWord(contents & 0x40000000 | (abs & 0x3fffffff)))
      }

    override def *(other: W): DW = {
      val abs = (contents & 0x3fffffff).toLong * (other.contents & 0x3fffffff)
      val sign = (contents & ~other.contents | ~contents & other.contents) & 0x40000000
      MixDWord((sign.toLong << 30) | abs)
    }

    override def /(divisor: W): (W, W) = {
      if ((divisor.contents & 0x3fffffff) == 0)
        throw new DivisionByZeroException
      val absDividend = contents & 0x3fffffff
      val absDivisor = divisor.contents & 0x3fffffff
      val absQuotient = absDividend / absDivisor
      val absRemainder = absDividend % absDivisor
      val signQuotient = (contents & ~divisor.contents | ~contents & divisor.contents) & 0x40000000
      (MixWord(signQuotient | absQuotient), MixWord(contents & 0x40000000 | absRemainder))
    }

    override def /\(divisor: W): (W, W) = {
      if ((divisor.contents & 0x3fffffff) == 0)
        throw new DivisionByZeroException
      if ((contents & 0x3ffffff) >= (divisor.contents & 0x3fffffff))
        throw new OverflowException
      val absDividend = (contents & 0x3fffffff).toLong << 30
      val absDivisor = divisor.contents & 0x3fffffff
      val absQuotient = (absDividend / absDivisor).toInt
      val absRemainder = (absDividend % absDivisor).toInt
      val signQuotient = (contents & ~divisor.contents | ~contents & divisor.contents) & 0x40000000
      (MixWord(signQuotient | absQuotient), MixWord(contents & 0x40000000 | absRemainder))
    }

    override def :*(other: W): W = {
      val shiftedLeft = MixWord(contents & 0x40000000 | ((contents & 0x07ffffff) << 3))
      (shiftedLeft + other)._2
    }

    override def <=>(other: W): Comparison = {
      if ((contents & 0x40000000) == (other.contents & 0x40000000))
        if ((contents & 0x3fffffff) == (other.contents & 0x3fffffff)) Comparison.EQUAL
        else if (isPositive && (contents & 0x3fffffff) > (other.contents & 0x3fffffff)) Comparison.GREATER
        else if (isNegative && (contents & 0x3fffffff) < (other.contents & 0x3fffffff)) Comparison.GREATER
        else Comparison.LESS
      else if ((contents & 0x3fffffff) == 0 && (other.contents & 0x3fffffff) == 0) Comparison.EQUAL
      else if (isPositive) Comparison.GREATER
      else Comparison.LESS
    }

    override def <<(n: I): W = {
      val shift = 6 * n.contents
      MixWord(contents & 0x40000000 | (contents << shift) & 0x3fffffff)
    }

    override def >>(n: I): W = {
      val shift = 6 * n.contents
      MixWord(contents & 0x40000000 | (contents & 0x3fffffff) >> shift)
    }

    override def toByte: MixByte = {
      if ((contents & 0x7fffffc0) > 0) throw new OverflowException
      MixByte((contents & 0x3f).toByte)
    }

    override def toIndex: I = {
      if ((contents & 0x3ffff000) > 0) throw new OverflowException
      MixIndex(((contents & 0x40000000) >> 18 | (contents & 0xfff)).toShort)
    }

    override def toLong: Long = {
      if (isPositive) contents
      else -(contents ^ 0x40000000)
    }

    override def toIOWord: IOWord = {
      val bytes = (1 to 5).map(i => ((contents & MixWord.masks(i)) >> (6 * (5 - i))).toByte)
      IOWord(isNegative, bytes)
    }

    override def toCharCode: DW = {
      val charCode = (0 until 10).foldLeft((0L, contents & 0x3fffffff)) { (state, i) =>
        val d = state._2 % 10 + 30
        (state._1 | (d.toLong << (6 * i)), state._2 / 10)
      }
      MixDWord(((contents & 0x40000000).toLong << 30) | charCode._1)
    }
  }

  object MixWord {
    val masks: Array[Int] = Array(0x40000000, 0x3f000000, 0xfc0000, 0x3f000, 0xfc0, 0x3f)

    def bitMask(l: Int, r: Int): Int = {
      if (l > 5 || r > 5) throw new WrongFieldSpecException((8 * l + r).toByte)
      (l to r).foldLeft(0) { (m, i) => m | masks(i) }
    }
  }

  case class MixDWord(contents: Long) extends AbstractMixDWord {
    override def isPositive: Boolean = (contents & 0x1000000000000000L) == 0

    override def isNegative: Boolean = (contents & 0x1000000000000000L) > 0

    override def /(divisor: W): (W, W) = {
      if ((divisor.contents & 0x3fffffff) == 0)
        throw new DivisionByZeroException
      if (((contents >> 30) & 0x3ffffff) >= (divisor.contents & 0x3fffffff))
        throw new OverflowException
      val absDividend = contents & 0xfffffffffffffffL
      val absDivisor = divisor.contents & 0x3fffffff
      val absQuotient = (absDividend / absDivisor).toInt
      val absRemainder = (absDividend % absDivisor).toInt
      val signQuotient = ((contents >> 30) & ~divisor.contents | (~contents >> 30) & divisor.contents).toInt & 0x40000000
      (MixWord(signQuotient | absQuotient), MixWord((contents >> 30).toInt & 0x40000000 | absRemainder))
    }

    override def <<(n: I): DW = {
      val shift = 6 * n.contents
      MixDWord(contents & 0x1000000000000000L | (contents << shift) & 0xfffffffffffffffL)
    }

    override def >>(n: I): DW = {
      val shift = 6 * n.contents
      MixDWord(contents & 0x1000000000000000L | (contents & 0xfffffffffffffffL) >> shift)
    }

    override def <<|(n: I): DW = {
      val shift = 6 * (n.contents % 10)
      MixDWord(contents & 0x1000000000000000L
          | (contents << shift) & 0xfffffffffffffffL
          | ((contents & 0xfffffffffffffffL) >> (60 - shift))
      )
    }

    override def >>|(n: I): DW = {
      val shift = 6 * (n.contents % 10)
      MixDWord(contents & 0x1000000000000000L
          | (contents & 0xfffffffffffffffL) >> shift
          | ((contents << (60 - shift)) & 0xfffffffffffffffL)
      )
    }

    override def charToNumber: W = {
      val number = (9 to 0 by -1).foldLeft((0L, 0xfc0000000000000L)) { (state, i) =>
        val d = ((contents & state._2) >> 6 * i) % 10
        (state._1 * 10 + d, state._2 >> 6)
      }
      MixWord((((contents & 0x1000000000000000L) >> 30) | (number._1 & 0x3fffffff)).toInt)
    }

    override def toWord: W = {
      if ((contents & 0xfffffffc0000000L) > 0) throw new OverflowException
      MixWord((((contents & 0x1000000000000000L) >> 30) | (contents & 0x3fffffff)).toInt)
    }
  }
}
