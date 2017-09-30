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
      while (!_breakpoints(_currentState.programCounter.toShort) && !_currentState.isHalted)

    override def toggleBreakpoint(address: Short): Unit =
      if (_breakpoints(address)) _breakpoints -= address
      else _breakpoints += address.toShort
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
  }
}
