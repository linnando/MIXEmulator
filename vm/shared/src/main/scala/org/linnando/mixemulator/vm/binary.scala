package org.linnando.mixemulator.vm

import org.linnando.mixemulator.vm.Comparison.Comparison
import org.linnando.mixemulator.vm.exceptions._
import org.linnando.mixemulator.vm.io.data.IOWord

object binary extends ProcessingModel {
  override type RS = RegisterState
  override type MS = MemoryState
  override type B = MixByte
  override type I = MixIndex
  override type W = MixWord
  override type DW = MixDWord

  override val BYTE_SIZE: Byte = 64

  override def createVirtualMachineBuilder(): VirtualMachineBuilder = BinaryVirtualMachineBuilder()

  case class BinaryVirtualMachineBuilder(state: State = initialState,
                                         counter: I = MixIndex(0),
                                         symbols: Map[String, W] = Map.empty,
                                         forwardReferences: Map[String, Seq[I]] = Map.empty.withDefaultValue(Seq.empty),
                                         literals: Map[W, Seq[I]] = Map.empty.withDefaultValue(Seq.empty)) extends VirtualMachineBuilder {

    override def getCounter: Short = counter.toShort

    override def withWValueSymbol(label: String, wValue: String): BinaryVirtualMachineBuilder =
      withSymbol(label, evaluateWValue(wValue))

    private def withSymbol(label: String, value: W) = label match {
      case null | "" => this
      case VirtualMachineBuilder.localBackReference(_*) =>
        throw new WrongLabelException(label)
      case VirtualMachineBuilder.localForwardReference(_*) =>
        throw new WrongLabelException(label)
      case VirtualMachineBuilder.localLabel(_*) =>
        copy(symbols = symbols.updated(label, value), forwardReferences = forwardReferences - label)
          .withAddressFields(forwardReferences(label), value)
      case _ =>
        if (symbols.contains(label)) throw new DuplicateSymbolException(label)
        else copy(symbols = symbols.updated(label, value), forwardReferences = forwardReferences - label)
          .withAddressFields(forwardReferences(label), value)
    }

    private def withAddressFields(addresses: Seq[I], addressFieldValue: W): BinaryVirtualMachineBuilder = {
      if (addresses.isEmpty) this
      else {
        if ((addressFieldValue.contents & 0x3ffff000) > 0)
          throw new OverflowException
        val addressField = (addressFieldValue.contents & 0x40000000) | ((addressFieldValue.contents & 0xfff) << 18)
        copy(state = addresses.foldLeft(state) { (s, address) =>
          val value = MixWord(addressField | s.memory.get(address).contents & 0x0003ffff)
          s.copy(memory = s.memory.updated(address, value))
        })
      }
    }

    private def evaluateWValue(wValue: String): W =
      wValue.split(",").foldLeft(MixWord(0)) { (word, segment) =>
        segment match {
          case VirtualMachineBuilder.expressionAndFieldSpec(expression, _, fPart) =>
            val value = evaluateExpression(expression)
            val fieldSpec = evaluateFPart(fPart, 5)
            val l = fieldSpec.contents >> 3
            val r = fieldSpec.contents & 0x07
            if (l > r) throw new WrongFieldSpecException(fieldSpec.contents)
            val shiftedValue = value.contents & 0x40000000 | ((value.contents << (6 * (5 - r))) & 0x3fffffff)
            val mask = MixWord.bitMask(l, r)
            MixWord(shiftedValue & mask | word.contents & ~mask & 0x7fffffff)
          case _ => throw new InvalidExpressionException(segment)
        }
      }

    private def evaluateExpression(expression: String): W = {
      def _op(left: W, op: String, right: W): W = op match {
        case "+" => (left + right)._2
        case "-" => (left - right)._2
        case "*" => (left * right).toWord
        case "/" => (left.toDWordRight / right)._1
        case "//" => (left.toDWordLeft / right)._1
        case ":" =>
          val shiftedLeft = MixWord(left.contents & 0x40000000 | ((left.contents & 0x07ffffff) << 3))
          (shiftedLeft + right)._2
      }

      def _evaluate(acc: W, tail: CharSequence): W =
        if (tail == null || tail.length() == 0) acc
        else VirtualMachineBuilder.expressionSegment.findPrefixMatchOf(tail) match {
          case Some(m) =>
            val op = m.group(1)
            val value = evaluateElementaryExpression(m.group(2))
            val next = _op(acc, op, value)
            _evaluate(next, m.after)
          case None =>
            throw new InvalidExpressionException(expression)
        }

      VirtualMachineBuilder.signedElementaryExpression.findPrefixMatchOf(expression) match {
        case Some(m) =>
          val sign = m.group(1)
          val value = evaluateElementaryExpression(m.group(2))
          val start = if (sign == null || sign == "" || sign == "+") value else -value
          _evaluate(start, m.after)
        case None =>
          throw new InvalidExpressionException(expression)
      }
    }

    private def evaluateElementaryExpression(expression: String): W = expression match {
      case "*" =>
        counter.toWord
      case VirtualMachineBuilder.number(_*) =>
        MixWord.get(expression.toLong)
      case VirtualMachineBuilder.localLabel(_*) =>
        throw new InvalidExpressionException(expression)
      case VirtualMachineBuilder.localBackReference(id) => symbols.get(s"${id}H") match {
        case Some(value) => value
        case None => throw new UndefinedSymbolException(expression)
      }
      case VirtualMachineBuilder.localForwardReference(_*) =>
        throw new UndefinedSymbolException(expression)
      case VirtualMachineBuilder.numberOrSymbol(_*) => symbols.get(expression) match {
        case Some(value) => value
        case None => throw new UndefinedSymbolException(expression)
      }
    }

    private def evaluateFPart(fPart: String, default: Byte): B = fPart match {
      case null | "" => MixByte(default)
      case _ => evaluateExpression(fPart).toByte
    }

    override def withCurrentCounterSymbol(label: String): BinaryVirtualMachineBuilder =
      withSymbol(label, counter.toWord)

    override def withOrig(wValue: String): BinaryVirtualMachineBuilder = {
      val address = evaluateWValue(wValue)
      if (address.isNegative || (address <=> MixWord(VirtualMachine.MEMORY_SIZE)) != Comparison.LESS)
        throw new WrongMemoryAddressException(address.toLong)
      copy(counter = address.toIndex)
    }

    override def withConstant(wValue: String): BinaryVirtualMachineBuilder =
      withValue(evaluateWValue(wValue))

    private def withValue(value: W): BinaryVirtualMachineBuilder = {
      if (counter.contents >= VirtualMachine.MEMORY_SIZE)
        throw new WrongMemoryAddressException(counter.contents)
      copy(
        state = state.copy(memory = state.memory.updated(counter, value)),
        counter = counter.next
      )
    }

    override def withCharCode(chars: String): VirtualMachineBuilder =
      withValue(translateCharCode(chars))

    private def translateCharCode(chars: String): W =
      MixWord((0 until 5).foldLeft(0) { (contents, i) =>
        val char = chars(i)
        VirtualMachine.CODES.get(char) match {
          case Some(code) => contents | (code << 6 * (4 - i))
          case None => throw new UnsupportedCharacterException(char)
        }
      })

    override def withFinalSection(label: String, value: String): VirtualMachineBuilder = {
      val withSymbols = forwardReferences.keys.foldLeft(this) { (s, ref) =>
        if (ref == label) s
        else s.withCurrentCounterSymbol(ref).withValue(MixWord(0))
      }
      val withLiterals = literals.foldLeft(withSymbols) { (s, literal) =>
        s.withAddressFields(literal._2, s.counter.toWord).withValue(literal._1)
      }
      withLiterals.withCurrentCounterSymbol(label)
        .withProgramCounter(evaluateWValue(value).toIndex)
    }

    private def withProgramCounter(value: I): BinaryVirtualMachineBuilder =
      copy(state = state.copy(programCounter = value))

    override def withCommand(aPart: String, indexPart: String, fPart: String, opCode: Byte, defaultFieldSpec: Byte): BinaryVirtualMachineBuilder = {
      val fieldSpec = evaluateFPart(fPart, defaultFieldSpec)
      val indexSpec = evaluateIndexPart(indexPart)
      try {
        val address = evaluateAPart(aPart)
        val cellValue = MixWord(address, indexSpec, fieldSpec, MixByte(opCode))
        withValue(cellValue)
      }
      catch {
        case e: ForwardReferenceException =>
          val cellValue = MixWord(MixIndex(0.toShort), indexSpec, fieldSpec, MixByte(opCode))
          withForwardReference(e.symbol).withValue(cellValue)
        case e: LiteralException =>
          val cellValue = MixWord(MixIndex(0.toShort), indexSpec, fieldSpec, MixByte(opCode))
          withLiteral(e.value).withValue(cellValue)
      }
    }

    private def evaluateIndexPart(indexPart: String): B = indexPart match {
      case null | "" => MixByte(0)
      case _ => evaluateExpression(indexPart).toByte
    }

    private def evaluateAPart(aPart: String): I = aPart match {
      case null | "" => MixIndex(0.toShort)
      case VirtualMachineBuilder.numberOrSymbol(_*) =>
        try {
          evaluateElementaryExpression(aPart).toIndex
        }
        catch {
          case e: UndefinedSymbolException => throw new ForwardReferenceException(e.symbol)
        }
      case VirtualMachineBuilder.literal(wValue) => throw new LiteralException(evaluateWValue(wValue))
      case _ => evaluateExpression(aPart).toIndex
    }

    private def withForwardReference(symbol: String): BinaryVirtualMachineBuilder =
      copy(forwardReferences = forwardReferences.updated(symbol, forwardReferences(symbol) :+ counter))

    private def withLiteral(value: W): BinaryVirtualMachineBuilder =
      copy(literals = literals.updated(value, literals(value) :+ counter))

    override def build: VirtualMachine = new VirtualMachineImpl(state)

    override def buildTracking: TrackingVirtualMachine = new TrackingVirtualMachineImpl(state)
  }

  class ForwardReferenceException(val symbol: String) extends Exception {
  }

  class LiteralException(val value: W) extends Exception {
  }

  def initialState = State(
    registers = RegisterState.initialState,
    memory = MemoryState.initialState,
    programCounter = MixIndex(0.toShort),
    timeCounter = 0,
    isHalted = false,
    devices = IndexedSeq.empty
  )

  override def getZero: W = MixWord(0)

  override def getWord(ioWord: IOWord): W = {
    val init = if (ioWord.negative) 1 else 0
    MixWord(ioWord.bytes.foldLeft(init)((w, b) => w << 6 | b))
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
    override def get(address: I): W = get(address.contents)

    override def get(address: Short): MixWord = {
      if (address >= VirtualMachine.MEMORY_SIZE)
        throw new WrongMemoryAddressException(address)
      if (exclusiveLocks exists { l => conflicts(l, address) }) throw new InconsistentReadException
      MixWord(contents(address))
    }

    private def conflicts(lock: (I, Int, Int), address: Short, size: Int = 1) =
      address < lock._1.contents + lock._2 && lock._1.contents < address + size

    override def updated(address: I, value: W): MS = {
      if (address.contents >= VirtualMachine.MEMORY_SIZE)
        throw new WrongMemoryAddressException(address.contents)
      if (sharedLocks exists { l => conflicts(l, address.contents) }) throw new InconsistentReadException
      if (exclusiveLocks exists { l => conflicts(l, address.contents) }) throw new WriteConflictException
      copy(contents = contents.updated(address.contents, value.contents))
    }

    override def updated(address: I, fieldSpec: B, value: W): MS = {
      if (address.contents >= VirtualMachine.MEMORY_SIZE)
        throw new WrongMemoryAddressException(address.contents)
      if (sharedLocks exists { l => conflicts(l, address.contents) }) throw new InconsistentReadException
      if (exclusiveLocks exists { l => conflicts(l, address.contents) }) throw new WriteConflictException
      val l = fieldSpec.contents >> 3
      val r = fieldSpec.contents & 0x07
      if (l > r) throw new WrongFieldSpecException(fieldSpec.contents)
      val shiftedValue = value.contents & 0x40000000 | ((value.contents << (6 * (5 - r))) & 0x3fffffff)
      val mask = MixWord.bitMask(l, r)
      val updatedWord = MixWord(shiftedValue & mask | contents(address.contents) & ~mask & 0x7fffffff)
      copy(contents = contents.updated(address.contents, updatedWord.contents))
    }

    override def withSharedLock(address: I, size: Int, deviceNum: Int): MS = {
      if (address.contents + size > VirtualMachine.MEMORY_SIZE)
        throw new WrongMemoryAddressException(address.contents)
      if (exclusiveLocks exists { l => conflicts(l, address.contents, size) }) throw new InconsistentReadException
      copy(sharedLocks = (address, size, deviceNum) :: sharedLocks)
    }

    override def withExclusiveLock(address: I, size: Int, deviceNum: Int): MS = {
      if (address.contents + size > VirtualMachine.MEMORY_SIZE)
        throw new WrongMemoryAddressException(address.contents)
      if (sharedLocks exists { l => conflicts(l, address.contents, size) }) throw new InconsistentReadException
      if (exclusiveLocks exists { l => conflicts(l, address.contents, size) }) throw new WriteConflictException
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
      if (isNegative) throw new Error
      val nextIndex = contents + 1
      if ((nextIndex & 0x1000) > 0) throw new OverflowException
      MixIndex(nextIndex.toShort)
    }

    override def toShort: Short =
      if (isPositive) contents
      else (-(contents ^ 0x1000)).toShort

    override def toWord: W = MixWord((contents & 0x1000) << 18 | (contents & 0xfff))

    override def toIOWord: IOWord = {
      IOWord(
        (contents & MixIndex.masks(0)) > 0,
        Seq[Byte](0, 0, 0) ++ (1 to 2).map(i => ((contents & MixIndex.masks(i)) >> (6 * (2 - i))).toByte)
      )
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

    override def toIndex: I = {
      if ((contents & 0x3ffff000) > 0) throw new OverflowException
      MixIndex(((contents & 0x40000000) >> 18 | (contents & 0xfff)).toShort)
    }

    override def toByte: MixByte = {
      if ((contents & 0x7fffffc0) > 0) throw new OverflowException
      MixByte((contents & 0x3f).toByte)
    }

    override def toLong: Long = {
      if (isPositive) contents
      else -(contents ^ 0x40000000)
    }

    override def toDWordLeft: MixDWord = MixDWord(contents.toLong << 30)

    override def toDWordRight: MixDWord =
      MixDWord(((contents & 0x40000000).toLong << 30) | (contents & 0x3fffffff).toLong)


    override def toIOWord: IOWord = IOWord(
      (contents & MixWord.masks(0)) > 0,
      (1 to 5).map(i => ((contents & MixWord.masks(i)) >> (6 * (5 - i))).toByte)
    )

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

    def get(value: Long): MixWord = {
      val sign = if (value < 0) 0x40000000 else 0x0
      val abs = value.abs
      if ((abs & ~0x3fffffffL) != 0)
        throw new OverflowException
      MixWord(sign | abs.toInt)
    }

    def apply(address: I, indexSpec: B, fieldSpec: B, opCode: B): MixWord =
      MixWord((address.contents << 18) | (indexSpec.contents << 12) | (fieldSpec.contents << 6) | opCode.contents)

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
      val absDividend = ((contents & 0xfffffffffffffffL) / (divisor.contents & 0x3fffffff)).toInt
      val absRemainder = ((contents & 0xfffffffffffffffL) % (divisor.contents & 0x3fffffff)).toInt
      val signDividend = ((contents >> 30) & ~divisor.contents | (~contents >> 30) & divisor.contents).toInt & 0x40000000
      (MixWord(signDividend | absDividend), MixWord((contents >> 30).toInt & 0x40000000 | absRemainder))
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

    override def toWord: MixWord = {
      MixWord((((contents & 0x1000000000000000L) >> 30) | (contents & 0x3fffffff)).toInt)
    }
  }
}
