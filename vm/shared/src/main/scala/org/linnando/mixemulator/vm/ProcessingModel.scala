package org.linnando.mixemulator.vm

import java.util

import org.linnando.mixemulator.vm.exceptions._

abstract class ProcessingModel extends DataModel with Processor {
  type VMB <: AbstractVirtualMachineBuilder

  def createVirtualMachineBuilder(): VirtualMachineBuilder

  abstract class AbstractVirtualMachineBuilder extends VirtualMachineBuilder {
    def state: State
    def counter: I
    def symbols: Map[String, W]
    def forwardReferences: Map[String, Seq[I]]
    def literals: Map[W, Seq[I]]

    override def getCounter: Short = counter.toShort

    override def withWValueSymbol(label: String, wValue: String): VMB =
      withSymbol(label, evaluateWValue(wValue))

    private def withSymbol(label: String, value: W) = label match {
      case null | "" => withoutChanges
      case VirtualMachineBuilder.localBackReference(_*) =>
        throw new WrongLabelException(label)
      case VirtualMachineBuilder.localForwardReference(_*) =>
        throw new WrongLabelException(label)
      case VirtualMachineBuilder.localLabel(_*) =>
        withDefinedForwardReference(label, value)
      case _ =>
        if (symbols.contains(label)) throw new DuplicateSymbolException(label)
        else withDefinedForwardReference(label, value)
    }

    protected def withoutChanges: VMB

    protected def withDefinedForwardReference(label: String, value: W): VMB

    private def evaluateWValue(wValue: String): W =
      wValue.split(",").foldLeft(getZero) { (word, segment) =>
        segment match {
          case VirtualMachineBuilder.expressionAndFieldSpec(expression, _, fPart) =>
            val value = evaluateExpression(expression)
            val fieldSpec = evaluateFPart(fPart, 5)
            updatedWord(word, fieldSpec, value)
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
        case ":" => buildFieldSpec(left, right)
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

    protected def buildFieldSpec(left: W, right: W): W

    private def evaluateElementaryExpression(expression: String): W = expression match {
      case "*" =>
        counter.toWord
      case VirtualMachineBuilder.number(_*) =>
        getWord(expression.toLong)
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

    protected def getWord(expression: Long): W

    private def evaluateFPart(fPart: String, default: Byte): B = fPart match {
      case null | "" => getByte(default)
      case _ => evaluateExpression(fPart).toByte
    }

    protected def getByte(value: Byte): B

    protected def updatedWord(word: W, fieldSpec: B, value: W): W

    override def withCurrentCounterSymbol(label: String): VMB =
      withSymbol(label, counter.toWord)

    override def withOrig(wValue: String): VMB = {
      val address = evaluateWValue(wValue)
      if (address.isNegative || (address <=> getWord(VirtualMachine.MEMORY_SIZE)) != Comparison.LESS)
        throw new WrongMemoryAddressException(address.toLong)
      withChangedCounter(address)
    }

    protected def withChangedCounter(address: W): VMB

    override def withConstant(wValue: String): VMB =
      withValue(evaluateWValue(wValue))

    protected def withValue(value: W): VMB

    override def withCharCode(chars: String): VMB =
      withValue(translateCharCode(chars))

    protected def translateCharCode(chars: String): W

    override def withFinalSection(label: String, value: String): VMB = {
      val withSymbols = forwardReferences.keys.foldLeft(withoutChanges) { (s, ref) =>
        if (ref == label) s
        else s.withCurrentCounterSymbol(ref).withValue(getZero)
      }
      val withLiterals = literals.foldLeft(withSymbols) { (s, literal) =>
        s.withAddressFields(literal._2, s.counter.toWord).withValue(literal._1)
      }
      withLiterals.withCurrentCounterSymbol(label)
        .withProgramCounter(evaluateWValue(value).toIndex)
    }

    protected def withAddressFields(addresses: Seq[I], addressFieldValue: W): VMB

    protected def withProgramCounter(value: I): VMB

    override def withCommand(aPart: String, indexPart: String, fPart: String, opCode: Byte, defaultFieldSpec: Byte): VMB = {
      val fieldSpec = evaluateFPart(fPart, defaultFieldSpec)
      val indexSpec = evaluateIndexPart(indexPart)
      try {
        val address = evaluateAPart(aPart)
        val cellValue = getWord(address, indexSpec, fieldSpec, getByte(opCode))
        withValue(cellValue)
      }
      catch {
        case e: ForwardReferenceException =>
          val cellValue = getWord(getIndex(0.toShort), indexSpec, fieldSpec, getByte(opCode))
          withForwardReference(e.symbol).withValue(cellValue)
        case e: LiteralException =>
          val cellValue = getWord(getIndex(0.toShort), indexSpec, fieldSpec, getByte(opCode))
          withLiteral(e.value).withValue(cellValue)
      }
    }

    protected def getWord(address: I, indexSpec: B, fieldSpec: B, opCode: B): W

    protected def getIndex(value: Short): I

    private def evaluateIndexPart(indexPart: String): B = indexPart match {
      case null | "" => getByte(0)
      case _ => evaluateExpression(indexPart).toByte
    }

    private def evaluateAPart(aPart: String): I = aPart match {
      case null | "" => getIndex(0)
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

    protected def withForwardReference(symbol: String): VMB

    protected def withLiteral(value: W): VMB

    override def build: VirtualMachine = new VirtualMachineImpl(state)

    override def buildTracking: TrackingVirtualMachine = new TrackingVirtualMachineImpl(state)
  }

  class ForwardReferenceException(val symbol: String) extends Exception {
  }

  class LiteralException(val value: W) extends Exception {
  }

  class VirtualMachineImpl(private val initialState: State) extends VirtualMachine {
    private var _currentState: State = initialState
    private var _breakpoints: Set[Short] = Set.empty

    override def currentState: State = _currentState

    override def breakpoints: Set[Short] = _breakpoints

    override def canMoveForward: Boolean = !_currentState.isHalted

    override def stepForward(): Unit = _currentState = forward(_currentState)

    override def runForward(): Unit =
      do stepForward()
      while (!_breakpoints(_currentState.programCounter.toShort) && !_currentState.isHalted)

    override def toggleBreakpoint(address: Short): Unit =
      if (_breakpoints(address)) _breakpoints -= address
      else _breakpoints += address.toShort

    override def isModified(address: Short): Boolean = _currentState.get(address) != initialState.get(address)
  }

  class TrackingVirtualMachineImpl(private val initialState: State) extends TrackingVirtualMachine {
    // All states encountered so far: initialState +: LinkedList[State]
    private val stateIterator = new util.LinkedList[State]().listIterator()
    // _currentState points to the current state of the virtual machine
    // If stateIterator.hasPrevious, _currentState == the state that would be returned by stateIterator.previous()
    // Otherwise, _currentState == initialState
    private var _currentState = initialState
    private var _breakpoints: Set[Short] = Set.empty

    override def currentState: State = _currentState

    override def breakpoints: Set[Short] = _breakpoints

    override def canMoveForward: Boolean = stateIterator.hasNext || !_currentState.isHalted

    override def stepForward(): Unit =
      if (stateIterator.hasNext) _currentState = stateIterator.next()
      else {
        _currentState = forward(_currentState)
        stateIterator.add(_currentState)
      }

    override def runForward(): Unit =
      do stepForward()
      while (!_breakpoints(_currentState.programCounter.toShort) && !_currentState.isHalted)

    override def canMoveBack: Boolean = stateIterator.hasPrevious

    override def stepBack(): Unit = {
      if (!stateIterator.hasPrevious)
        throw new BackFromInitialStateException
      stateIterator.previous()
      if (stateIterator.hasPrevious) {
        _currentState = stateIterator.previous()
        stateIterator.next()
      }
      else _currentState = initialState
    }

    override def runBack(): Unit = {
      if (!stateIterator.hasPrevious)
        throw new BackFromInitialStateException
      stateIterator.previous()
      if (stateIterator.hasPrevious) {
        do _currentState = stateIterator.previous()
        while (!_breakpoints(_currentState.programCounter.toShort) && stateIterator.hasPrevious)
        if (_breakpoints(_currentState.programCounter.toShort)) stateIterator.next()
        else _currentState = initialState
      } else {
        _currentState = initialState
      }
    }

    override def toggleBreakpoint(address: Short): Unit =
      if (_breakpoints(address)) _breakpoints -= address
      else _breakpoints += address.toShort

    override def isModified(address: Short): Boolean = _currentState.get(address) != initialState.get(address)
  }
}
