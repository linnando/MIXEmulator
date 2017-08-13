package org.linnando.mixemulator.vm

import java.util

import org.linnando.mixemulator.vm.exceptions.BackFromInitialStateException

abstract class ProcessingModel extends DataModel with Processor {
  def createVirtualMachineBuilder(): VirtualMachineBuilder

  class VirtualMachineImpl(initialState: State) extends VirtualMachine {
    private var _currentState: State = initialState
    private var _breakpoints: Set[Short] = Set.empty

    override def currentState: State = _currentState

    override def breakpoints: Set[Short] = _breakpoints

    override def canMoveForward: Boolean = !_currentState.isHalted

    override def stepForward(): Unit = _currentState = forward(_currentState)

    override def runForward(): Unit =
      do stepForward()
      while (!(_breakpoints(_currentState.programCounter.toShort) || _currentState.isHalted))

    override def toggleBreakpoint(address: Short): Unit =
      if (_breakpoints(address)) _breakpoints -= address
      else _breakpoints += address.toShort
  }

  class TrackingVirtualMachineImpl(initialState: State) extends TrackingVirtualMachine {
    private val stateIterator = new util.LinkedList[State]().listIterator()
    private var lastState = initialState
    private var _currentState = initialState
    private var _breakpoints: Set[Short] = Set.empty

    override def currentState: State = _currentState

    override def breakpoints: Set[Short] = _breakpoints

    override def canMoveForward: Boolean = stateIterator.hasNext || !lastState.isHalted

    override def stepForward(): Unit = {
      if (stateIterator.hasNext) _currentState = stateIterator.next()
      else {
        stateIterator.add(lastState)
        lastState = forward(lastState)
        _currentState = lastState
      }
    }

    override def runForward(): Unit =
      do stepForward()
      while (!(_breakpoints(_currentState.programCounter.toShort) || _currentState.isHalted))

    override def canMoveBack: Boolean = stateIterator.hasPrevious

    override def stepBack(): Unit =
      if (stateIterator.hasPrevious) _currentState = stateIterator.previous()
      else throw new BackFromInitialStateException

    override def runBack(): Unit =
      if (stateIterator.hasPrevious)
        do _currentState = stateIterator.previous()
        while (!_breakpoints(_currentState.programCounter.toShort) && stateIterator.hasPrevious)
      else throw new BackFromInitialStateException

    override def toggleBreakpoint(address: Short): Unit =
      if (_breakpoints(address)) _breakpoints -= address
      else _breakpoints += address.toShort
  }
}
