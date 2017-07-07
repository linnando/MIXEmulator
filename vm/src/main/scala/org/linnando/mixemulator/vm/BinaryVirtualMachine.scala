package org.linnando.mixemulator.vm
import org.linnando.mixemulator.vm.Comparison.Comparison
import org.linnando.mixemulator.vm.exceptions._
import org.linnando.mixemulator.vm.io.data.IOWord

object BinaryVirtualMachine extends VirtualMachine {
  override type RS = BinaryRegisterState
  override type MS = BinaryMemoryState
  override type B = BinaryMixByte
  override type I = BinaryMixIndex
  override type W = BinaryMixWord
  override type DW = BinaryMixDWord

  lazy val initialState = State(
    registers = BinaryRegisterState.initialState,
    memory = BinaryMemoryState.initialState,
    programCounter = BinaryMixIndex(0),
    timeCounter = 0,
    devices = IndexedSeq.empty
  )

  override def getZero: W = BinaryMixWord(0)

  override def getWord(ioWord: IOWord): W = {
    val init = if (ioWord.negative) 1 else 0
    BinaryMixWord(ioWord.bytes.foldLeft(init)((w, b) => w << 6 | b))
  }

  case class BinaryRegisterState(a: Int, x: Int, i: Vector[Short], j: Short, ov: Boolean, cmp: Comparison) extends RegisterState {
    override def getA: W = BinaryMixWord(a)

    override def getX: W = BinaryMixWord(x)

    override def getAX: DW = BinaryMixDWord((a.toLong << 30) | x & 0x3fffffff)

    override def getI(indexSpec: B): I = getI(indexSpec.contents)

    override def getI(indexSpec: Int): I = {
      if (indexSpec < 1 || indexSpec > i.length) throw new WrongIndexSpecException(indexSpec.toByte)
      BinaryMixIndex(i(indexSpec - 1))
    }

    override def getJ: I = BinaryMixIndex(j)

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

  object BinaryRegisterState {
    lazy val initialState = BinaryRegisterState(
      a = 0,
      x = 0,
      i = Vector.fill(6)(0),
      j = 0,
      ov = false,
      cmp = Comparison.EQUAL
    )
  }

  case class BinaryMemoryState(contents: Vector[Int], sharedLocks: List[(I, Int, Int)], exclusiveLocks: List[(I, Int, Int)]) extends MemoryState {
    override def get(address: I): W = {
      if (address.contents >= BinaryMemoryState.MEMORY_SIZE)
        throw new WrongMemoryAddressException(address.contents)
      if (exclusiveLocks exists { l => conflicts(l, address) }) throw new InconsistentReadException
      BinaryMixWord(contents(address.contents))
    }

    private def conflicts(lock: (I, Int, Int), address: I, size: Int = 1) =
      address.contents < lock._1.contents + lock._2 && lock._1.contents < address.contents + size

    override def updated(address: I, value: W): MS = {
      if (address.contents >= BinaryMemoryState.MEMORY_SIZE)
        throw new WrongMemoryAddressException(address.contents)
      if (sharedLocks exists { l => conflicts(l, address) }) throw new InconsistentReadException
      if (exclusiveLocks exists { l => conflicts(l, address) }) throw new WriteConflictException
      copy(contents = contents.updated(address.contents, value.contents))
    }

    override def updated(address: I, fieldSpec: B, value: W): MS = {
      if (address.contents >= BinaryMemoryState.MEMORY_SIZE)
        throw new WrongMemoryAddressException(address.contents)
      if (sharedLocks exists { l => conflicts(l, address) }) throw new InconsistentReadException
      if (exclusiveLocks exists { l => conflicts(l, address) }) throw new WriteConflictException
      val l = fieldSpec.contents >> 3
      val r = fieldSpec.contents & 0x07
      if (l > r) throw new WrongFieldSpecException(fieldSpec.contents)
      val shiftedValue = value.contents & 0x40000000 | ((value.contents << (6 * (5 - r))) & 0x3fffffff)
      val mask = BinaryMixWord.bitMask(l, r)
      val updatedWord = BinaryMixWord(shiftedValue & mask | contents(address.contents) & ~mask & 0x7fffffff)
      copy(contents = contents.updated(address.contents, updatedWord.contents))
    }

    override def withSharedLock(address: I, size: Int, deviceNum: Int): MS = {
      if (address.contents + size > BinaryMemoryState.MEMORY_SIZE)
        throw new WrongMemoryAddressException(address.contents)
      if (exclusiveLocks exists { l => conflicts(l, address, size) }) throw new InconsistentReadException
      copy(sharedLocks = (address, size, deviceNum) :: sharedLocks)
    }

    override def withExclusiveLock(address: I, size: Int, deviceNum: Int): MS = {
      if (address.contents + size > BinaryMemoryState.MEMORY_SIZE)
        throw new WrongMemoryAddressException(address.contents)
      if (sharedLocks exists { l => conflicts(l, address, size) }) throw new InconsistentReadException
      if (exclusiveLocks exists { l => conflicts(l, address, size) }) throw new WriteConflictException
      copy(exclusiveLocks = (address, size, deviceNum) :: exclusiveLocks)
    }

    override def withoutLocks(deviceNum: Int): MS = copy(
      sharedLocks = sharedLocks filterNot { l => l._3 == deviceNum },
      exclusiveLocks = exclusiveLocks filterNot { l => l._3 == deviceNum }
    )
  }

  object BinaryMemoryState {
    val MEMORY_SIZE: Short = 4000

    lazy val initialState = BinaryMemoryState(
      contents = Vector.fill(MEMORY_SIZE)(0),
      sharedLocks = List.empty,
      exclusiveLocks = List.empty
    )
  }

  case class BinaryMixByte(contents: Byte) extends MixByte {
    override def toInt: Int = contents.toInt

    override def toByte: Byte = contents

    override def isZero: Boolean = contents == 0
  }

  case class BinaryMixIndex(contents: Short) extends MixIndex {
    override def isPositive: Boolean = (contents & 0x1000) == 0

    override def isNegative: Boolean = (contents & 0x1000) > 0

    override def unary_-(): I = BinaryMixIndex((contents ^ 0x1000).toShort)

    override def +(other: I): I =
      if ((contents & 0x1000) == (other.contents & 0x1000)) {
        val abs = (contents & 0xfff) + (other.contents & 0xfff)
        if ((abs & 0x1000) > 0) throw new OverflowException
        BinaryMixIndex((contents & 0x1000 | abs).toShort)
      }
      else if ((contents & 0xfff) >= (other.contents & 0xfff)) {
        val abs = (contents & 0xfff) - (other.contents & 0xfff)
        BinaryMixIndex((contents & 0x1000 | abs).toShort)
      }
      else {
        val abs = (other.contents & 0xfff) - (contents & 0xfff)
        BinaryMixIndex((other.contents & 0x1000 | abs).toShort)
      }

    override def +(other: Int): I = {
      if (other < 0) throw new Error
      if (isPositive) {
        val abs = (contents & 0xfff) + other
        if ((abs & 0x1000) > 0) throw new OverflowException
        BinaryMixIndex(abs.toShort)
      }
      else if ((contents & 0xfff) >= other) {
        val abs = (contents & 0xfff) - other
        BinaryMixIndex((0x1000 | abs).toShort)
      }
      else {
        val abs = other - (contents & 0xfff)
        BinaryMixIndex(abs.toShort)
      }
    }

    override def -(subtrahend: I): I =
      if ((contents & 0x1000) == (subtrahend.contents & 0x1000))
        if ((contents & 0xfff) >= (subtrahend.contents & 0xfff)) {
          val abs = (contents & 0xfff) - (subtrahend.contents & 0xfff)
          BinaryMixIndex((contents & 0x1000 | abs).toShort)
        }
        else {
          val abs = (subtrahend.contents & 0xfff) - (contents & 0xfff)
          BinaryMixIndex((~contents & 0x1000 | abs).toShort)
        }
      else {
        val abs = (contents & 0xfff) + (subtrahend.contents & 0xfff)
        if ((abs & 0x1000) > 0) throw new OverflowException
        BinaryMixIndex((contents & 0x1000 | abs).toShort)
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
      if (isNegative) throw new Error
      val nextIndex = contents + 1
      if ((nextIndex & 0x1000) > 0) throw new OverflowException
      BinaryMixIndex(nextIndex.toShort)
    }

    override def toShort: Short =
      if (isPositive) contents
      else (-(contents ^ 0x1000)).toShort

    override def toWord: W = BinaryMixWord((contents & 0x1000) << 18 | (contents & 0xfff))
  }

  case class BinaryMixWord(contents: Int) extends MixWord {
    override def isPositive: Boolean = (contents & 0x40000000) == 0

    override def isNegative: Boolean = (contents & 0x40000000) > 0

    override def getAddress: I = BinaryMixIndex(((contents & 0x7ffc0000) >> 18).toShort)

    override def getIndexSpec: B = BinaryMixByte(((contents & 0x3f000) >> 12).toByte)

    override def getFieldSpec: B = BinaryMixByte(((contents & 0xfc0) >> 6).toByte)

    override def getOpCode: B = BinaryMixByte((contents & 0x3f).toByte)

    override def getField(fieldSpec: B): W = {
      val l = fieldSpec.contents >> 3
      val r = fieldSpec.contents & 0x07
      if (l > r) throw new WrongFieldSpecException(fieldSpec.contents)
      val sign = if (l == 0) contents & 0x40000000 else 0
      val mask = if (l == 0) BinaryMixWord.bitMask(1, r) else BinaryMixWord.bitMask(l, r)
      val abs = (contents & mask) >> (6 * (5 - r))
      BinaryMixWord(sign | abs)
    }

    override def unary_-(): W = BinaryMixWord(contents ^ 0x40000000)

    override def +(other: W): (Boolean, W) =
      if ((contents & 0x40000000) == (other.contents & 0x40000000)) {
        val abs = (contents & 0x3fffffff) + (other.contents & 0x3fffffff)
        ((abs & 0x40000000) > 0, BinaryMixWord(contents & 0x40000000 | (abs & 0x3fffffff)))
      }
      else if ((contents & 0x3fffffff) >= (other.contents & 0x3fffffff)) {
        val abs = (contents & 0x3fffffff) - (other.contents & 0x3fffffff)
        (false, BinaryMixWord(contents & 0x40000000 | abs))
      }
      else {
        val abs = (other.contents & 0x3fffffff) - (contents & 0x3fffffff)
        (false, BinaryMixWord(other.contents & 0x40000000 | abs))
      }

    override def -(subtrahend: W): (Boolean, W) =
      if ((contents & 0x40000000) == (subtrahend.contents & 0x40000000))
        if ((contents & 0x3fffffff) >= (subtrahend.contents & 0x3fffffff)) {
          val abs = (contents & 0x3fffffff) - (subtrahend.contents & 0x3fffffff)
          (false, BinaryMixWord(contents & 0x40000000 | abs))
        }
        else {
          val abs = (subtrahend.contents & 0x3fffffff) - (contents & 0x3fffffff)
          (false, BinaryMixWord(~contents & 0x40000000 | abs))
        }
      else {
        val abs = (contents & 0x3fffffff) + (subtrahend.contents & 0x3fffffff)
        ((abs & 0x40000000) > 0, BinaryMixWord(contents & 0x40000000 | (abs & 0x3fffffff)))
      }

    override def *(other: W): DW = {
      val abs = (contents & 0x3fffffff).toLong * (other.contents & 0x3fffffff)
      val sign = (contents & ~other.contents | ~contents & other.contents) & 0x40000000
      BinaryMixDWord((sign.toLong << 30) | abs)
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
      BinaryMixWord(contents & 0x40000000 | (contents << shift) & 0x3fffffff)
    }

    override def >>(n: I): W = {
      val shift = 6 * n.contents
      BinaryMixWord(contents & 0x40000000 | (contents & 0x3fffffff) >> shift)
    }

    override def toIndex: I = {
      if ((contents & 0x3ffff000) > 0) throw new OverflowException
      BinaryMixIndex(((contents & 0x40000000) >> 18 | (contents & 0xfff)).toShort)
    }

    override def toLong: Long = {
      if (isPositive) contents
      else -(contents ^ 0x40000000)
    }

    override def toIOWord: IOWord = IOWord(
      (contents & BinaryMixWord.masks(0)) > 0,
      (1 to 5).map(i => ((contents & BinaryMixWord.masks(i)) >> (6 * (5 - i))).toByte)
    )

    override def toCharCode: DW = {
      val charCode = (0 until 10).foldLeft((0L, contents & 0x3fffffff)) { (state, i) =>
        val d = state._2 % 10 + 30
        (state._1 | (d.toLong << (6 * i)), state._2 / 10)
      }
      BinaryMixDWord(((contents & 0x40000000).toLong << 30) | charCode._1)
    }
  }

  object BinaryMixWord {
    val masks: Array[Int] = Array(0x40000000, 0x3f000000, 0xfc0000, 0x3f000, 0xfc0, 0x3f)

    def bitMask(l: Int, r: Int): Int = {
      if (l > 5 || r > 5) throw new WrongFieldSpecException((8 * l + r).toByte)
      (l to r).foldLeft(0) { (m, i) => m | masks(i) }
    }
  }

  case class BinaryMixDWord(contents: Long) extends MixDWord {
    override def isPositive: Boolean = (contents & 0x1000000000000000L) == 0

    override def isNegative: Boolean = (contents & 0x1000000000000000L) > 0

    override def /(divisor: W): (W, W) = {
      if ((divisor.contents & 0x3fffffff) == 0)
        throw new DivisionByZeroException
      if (((contents >> 30) & 0x3ffffff) >= (divisor.contents & 0x3fffffff))
        throw new OverflowException
      val absDividend = ((contents & 0xfffffffffffffffL) / (divisor.contents & 0x3fffffff)).toInt
      val absRemainder = ((contents & 0xfffffffffffffffL) % (divisor.contents & 0x3fffffff)).toInt
      val signDividend = ((contents >> 30) & ~divisor.contents | (~contents >> 30) & divisor.contents).toInt & 0x40000000
      (BinaryMixWord(signDividend | absDividend), BinaryMixWord((contents >> 30).toInt & 0x40000000 | absRemainder))
    }

    override def <<(n: I): DW = {
      val shift = 6 * n.contents
      BinaryMixDWord(contents & 0x1000000000000000L | (contents << shift) & 0xfffffffffffffffL)
    }

    override def >>(n: I): DW = {
      val shift = 6 * n.contents
      BinaryMixDWord(contents & 0x1000000000000000L | (contents & 0xfffffffffffffffL) >> shift)
    }

    override def <<|(n: I): DW = {
      val shift = 6 * (n.contents % 10)
      BinaryMixDWord(contents & 0x1000000000000000L
          | (contents << shift) & 0xfffffffffffffffL
          | ((contents & 0xfffffffffffffffL) >> (60 - shift))
      )
    }

    override def >>|(n: I): DW = {
      val shift = 6 * (n.contents % 10)
      BinaryMixDWord(contents & 0x1000000000000000L
          | (contents & 0xfffffffffffffffL) >> shift
          | ((contents << (60 - shift)) & 0xfffffffffffffffL)
      )
    }

    override def charToNumber: W = {
      val number = (9 to 0 by -1).foldLeft((0L, 0xfc0000000000000L)) { (state, i) =>
        val d = ((contents & state._2) >> 6 * i) % 10
        (state._1 * 10 + d, state._2 >> 6)
      }
      BinaryMixWord((((contents & 0x1000000000000000L) >> 30) | (number._1 & 0x3fffffff)).toInt)
    }
  }
}
