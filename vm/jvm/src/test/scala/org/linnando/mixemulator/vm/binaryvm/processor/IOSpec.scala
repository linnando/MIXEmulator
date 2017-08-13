package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.binary
import org.linnando.mixemulator.vm.exceptions.{UnpredictableExecutionFlowException, WrongMemoryAddressException}
import org.linnando.mixemulator.vm.io.mock._
import org.specs2.mutable.Specification

import scala.collection.immutable.Queue

class IOSpec extends Specification {
  import binary._
  private val initialState = binary.initialState
  private val state = initialState.copy(
    registers = initialState.registers.updatedX(MixWord(2000)),
    devices = initialState.devices ++ Seq(
      (MockPositionalInputDevice(), Queue.empty),
      (MockPositionalOutputDevice(), Queue.empty),
      (MockRandomAccessIODevice(), Queue.empty),
      (MockTapeUnit(), Queue.empty),
      (MockDiskUnit(), Queue.empty),
      (MockLinePrinter(), Queue.empty),
      (MockPaperTape(), Queue.empty)
    )
  )

  "binary input" should {
    "call read for a positional input device" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      val nextState = execute(state, MixWord(0x0fa00024))
      val device = nextState.devices(0)
      nextState.memory.exclusiveLocks must contain((MixIndex(1000), device._1.blockSize, 0))
      device._1 match {
        case d: MockPositionalInputDevice => d.counter must beGreaterThan(0)
        case _ => throw new Error
      }
      device._2 must contain(MixIndex(1000))
    }

    "throw an exception for a positional output device" in {
      // A = 1000, I = 0, F = 1, C = 36 IN
      execute(state, MixWord(0x0fa00064)) must throwAn[UnsupportedOperationException]
    }

    "call read for a random access device" in {
      // A = 1000, I = 0, F = 2, C = 36 IN
      val nextState = execute(state, MixWord(0x0fa000a4))
      val device = nextState.devices(2)
      nextState.memory.exclusiveLocks must contain((MixIndex(1000), device._1.blockSize, 2))
      device._1 match {
        case d: MockRandomAccessIODevice => d.position must be equalTo 2000
        case _ => throw new Error
      }
      device._2 must contain(MixIndex(1000))
    }
  }

  "binary output" should {
    "throw an exception for a positional input device" in {
      // A = 1000, I = 0, F = 0, C = 37 OUT
      execute(state, MixWord(0x0fa00025)) must throwAn[UnsupportedOperationException]
    }

    "call write for a positional output device" in {
      // A = 1000, I = 0, F = 1, C = 37 OUT
      val nextState = execute(state, MixWord(0x0fa00065))
      val device = nextState.devices(1)
      nextState.memory.sharedLocks must contain((MixIndex(1000), device._1.blockSize, 1))
      device._1 match {
        case d: MockPositionalOutputDevice =>
          d.counter must beGreaterThan(0)
          d.block must not be empty
        case _ => throw new Error
      }
      device._2 must beEmpty
    }

    "call write for a random access device" in {
      // A = 1000, I = 0, F = 2, C = 37 OUT
      val nextState = execute(state, MixWord(0x0fa000a5))
      val device = nextState.devices(2)
      nextState.memory.sharedLocks must contain((MixIndex(1000), device._1.blockSize, 2))
      device._1 match {
        case d: MockRandomAccessIODevice =>
          d.position must be equalTo 2000
          d.block must not be empty
        case _ => throw new Error
      }
      device._2 must beEmpty
    }
  }

  "binary input/output control" should {
    "throw an exception for a generic device" in {
      // A = 0, I = 0, F = 0, C = 35 IOC
      execute(state, MixWord(0x00000023)) must throwAn[UnsupportedOperationException]
      // A = 0, I = 0, F = 1, C = 35 IOC
      execute(state, MixWord(0x00000063)) must throwAn[UnsupportedOperationException]
      // A = 0, I = 0, F = 2, C = 35 IOC
      execute(state, MixWord(0x000000a3)) must throwAn[UnsupportedOperationException]
    }

    "position a tape unit" in {
      // A = 10, I = 0, F = 3, C = 35 IOC
      val nextState = execute(state, MixWord(0x002800e3))
      val device = nextState.devices(3)
      device._1 match {
        case d: MockTapeUnit => d.position must be equalTo 10
        case _ => throw new Error
      }
      device._2 must beEmpty
    }

    "position a disk unit" in {
      // A = 0, I = 0, F = 4, C = 35 IOC
      val nextState = execute(state, MixWord(0x00000123))
      val device = nextState.devices(4)
      device._1 match {
        case d: MockDiskUnit => d.position must be equalTo 2000
        case _ => throw new Error
      }
      device._2 must beEmpty
    }

    "throw an exception if M != 0 for a disk unit" in {
      // A = 1, I = 0, F = 4, C = 35 IOC
      execute(state, MixWord(0x00040123)) must throwA[WrongMemoryAddressException]
    }

    "switch page on a line printer" in {
      // A = 0, I = 0, F = 5, C = 35 IOC
      val nextState = execute(state, MixWord(0x00000163))
      val device = nextState.devices(5)
      device._1 match {
        case d: MockLinePrinter => d.page must beGreaterThan(0)
        case _ => throw new Error
      }
      device._2 must beEmpty
    }

    "throw an exception if M != 0 for a line printer" in {
      // A = 1, I = 0, F = 5, C = 35 IOC
      execute(state, MixWord(0x00040163)) must throwA[WrongMemoryAddressException]
    }

    "call reset for a paper tape" in {
      // A = 0, I = 0, F = 6, C = 35 IOC
      val nextState = execute(state, MixWord(0x000001a3))
      val device = nextState.devices(6)
      device._1 match {
        case d: MockPaperTape => d.counter must beGreaterThan(0)
        case _ => throw new Error
      }
      device._2 must beEmpty
    }

    "throw an exception if M != 0 for a paper tape" in {
      // A = 1, I = 0, F = 6, C = 35 IOC
      execute(state, MixWord(0x000401a3)) must throwA[WrongMemoryAddressException]
    }
  }

  "device-conditional jump" should {
    "trigger jump ready and not trigger jump busy when device is not busy" in {
      // A = 1000, I = 0, F = 0, C = 38 JRED
      val nextStateRed = execute(state, MixWord(0x0fa00026))
      nextStateRed.programCounter must be equalTo MixIndex(1000)
      nextStateRed.registers.getJ must be equalTo MixIndex(1)
      // A = 1000, I = 0, F = 0, C = 34 JBUS
      val nextStateBus = execute(state, MixWord(0x0fa00022))
      nextStateBus.programCounter must be equalTo MixIndex(1)
      nextStateBus.registers.getJ must be equalTo MixIndex(0)
    }

    "throw an exception on jump ready when device is busy" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      val prevState = execute(state, MixWord(0x0fa00024))
      // A = 1000, I = 0, F = 0, C = 38 JRED
      execute(prevState, MixWord(0x0fa00026)) must throwAn[UnpredictableExecutionFlowException]
    }

    "throw an exception on jump busy when device is busy and the address is not the same as the program counter" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      val prevState = execute(state, MixWord(0x0fa00024))
      // A = 1000, I = 0, F = 0, C = 34 JBUS
      execute(prevState, MixWord(0x0fa00022)) must throwAn[UnpredictableExecutionFlowException]
    }

    "flush device input on jump busy when device is reading and the address is the same as the program counter" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      val prevState = execute(state, MixWord(0x0fa00024))
      // A = 1, I = 0, F = 0, C = 34 JBUS
      val nextState = execute(prevState, MixWord(0x00040022))
      val device = nextState.devices(0)
      (0 until device._1.blockSize) forall { i =>
        nextState.memory.get((1000 + i).toShort) must be equalTo MixWord(0x01041041)
      }
      nextState.memory.exclusiveLocks must not contain((MixIndex(1000), device._1.blockSize, 0))
      nextState.programCounter must be equalTo MixIndex(2)
      device._1.isBusy must beFalse
    }
  }
}
