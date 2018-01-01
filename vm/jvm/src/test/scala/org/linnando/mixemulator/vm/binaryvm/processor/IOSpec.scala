package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.binary
import org.linnando.mixemulator.vm.exceptions.{DeviceNotConnectedException, UnpredictableExecutionFlowException, WrongMemoryAddressException}
import org.linnando.mixemulator.vm.io.mock._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

import scala.collection.immutable.Queue

class IOSpec(implicit ee: ExecutionEnv) extends Specification {

  import binary._

  private val initialState = binary.initialState
  private val state = initialState.copy(
    registers = initialState.registers.updatedX(MixWord(2000)),
    devices = initialState.devices ++ Seq(
      0 -> (MockPositionalInputDevice(), Queue.empty),
      1 -> (MockPositionalOutputDevice(), Queue.empty),
      2 -> (MockRandomAccessIODevice(), Queue.empty),
      3 -> (MockTapeUnit(), Queue.empty),
      4 -> (MockDiskUnit(), Queue.empty),
      5 -> (MockLinePrinter(), Queue.empty),
      6 -> (MockPaperTape(), Queue.empty)
    )
  )

  "binary input" should {
    "call read for a positional input device" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      val nextState = execute(state, MixWord(0x0fa00024))
      nextState.map(_.memory.exclusiveLocks) must contain((MixIndex(1000), MockPositionalInputDevice.blockSize, 0)).await
      val device = nextState.map(_.devices(0))
      device.map(_._1 match { case d: MockPositionalInputDevice => d.counter }) must beGreaterThan(0).await
      device.map(_._2) must contain(MixIndex(1000)).await
    }

    "throw an exception for a positional output device" in {
      // A = 1000, I = 0, F = 1, C = 36 IN
      execute(state, MixWord(0x0fa00064)) must throwAn[UnsupportedOperationException].await
    }

    "call read for a random access device" in {
      // A = 1000, I = 0, F = 2, C = 36 IN
      val nextState = execute(state, MixWord(0x0fa000a4))
      nextState.map(_.memory.exclusiveLocks) must contain((MixIndex(1000), MockRandomAccessIODevice.blockSize, 2)).await
      val device = nextState.map(_.devices(2))
      device.map(_._1 match { case d: MockRandomAccessIODevice => d.position }) must beEqualTo(2000).await
      device.map(_._2) must contain(MixIndex(1000)).await
    }

    "throw an exception if no device is connected" in {
      // A = 1000, I = 0, F = 7, C = 36 IN
      execute(state, MixWord(0x0fa001e4)) must throwA[DeviceNotConnectedException].like({
        case e: DeviceNotConnectedException => e.deviceNum must be equalTo 7
      }).await
    }
  }

  "binary output" should {
    "throw an exception for a positional input device" in {
      // A = 1000, I = 0, F = 0, C = 37 OUT
      execute(state, MixWord(0x0fa00025)) must throwAn[UnsupportedOperationException].await
    }

    "call write for a positional output device" in {
      // A = 1000, I = 0, F = 1, C = 37 OUT
      val nextState = execute(state, MixWord(0x0fa00065))
      nextState.map(_.memory.sharedLocks) must contain((MixIndex(1000), MockPositionalOutputDevice.blockSize, 1)).await
      val device = nextState.map(_.devices(1))
      device.map(_._1 match { case d: MockPositionalOutputDevice => d.counter }) must beGreaterThan(0).await
      device.map(_._1 match { case d: MockPositionalOutputDevice => d.block }) must not be empty.await
      device.map(_._2.length) must beEqualTo(0).await
    }

    "call write for a random access device" in {
      // A = 1000, I = 0, F = 2, C = 37 OUT
      val nextState = execute(state, MixWord(0x0fa000a5))
      nextState.map(_.memory.sharedLocks) must contain((MixIndex(1000), MockRandomAccessIODevice.blockSize, 2)).await
      val device = nextState.map(_.devices(2))
      device.map(_._1 match { case d: MockRandomAccessIODevice => d.position }) must beEqualTo(2000).await
      device.map(_._1 match { case d: MockRandomAccessIODevice => d.block }) must not be empty.await
      device.map(_._2.length) must beEqualTo(0).await
    }

    "throw an exception if no device is connected" in {
      // A = 1000, I = 0, F = 7, C = 37 OUT
      execute(state, MixWord(0x0fa001e5)) must throwA[DeviceNotConnectedException].like({
        case e: DeviceNotConnectedException => e.deviceNum must be equalTo 7
      }).await
    }
  }

  "binary input/output control" should {
    "throw an exception for a generic device" in {
      // A = 0, I = 0, F = 0, C = 35 IOC
      execute(state, MixWord(0x00000023)) must throwAn[UnsupportedOperationException].await
      // A = 0, I = 0, F = 1, C = 35 IOC
      execute(state, MixWord(0x00000063)) must throwAn[UnsupportedOperationException].await
      // A = 0, I = 0, F = 2, C = 35 IOC
      execute(state, MixWord(0x000000a3)) must throwAn[UnsupportedOperationException].await
    }

    "position a tape unit" in {
      // A = 10, I = 0, F = 3, C = 35 IOC
      val nextState = execute(state, MixWord(0x002800e3))
      val device = nextState.map(_.devices(3))
      device.map(_._1 match { case d: MockTapeUnit => d.position }) must beEqualTo(10).await
      device.map(_._2.length) must beEqualTo(0).await
    }

    "position a disk unit" in {
      // A = 0, I = 0, F = 4, C = 35 IOC
      val nextState = execute(state, MixWord(0x00000123))
      val device = nextState.map(_.devices(4))
      device.map(_._1 match { case d: MockDiskUnit => d.position }) must beEqualTo(2000).await
      device.map(_._2.length) must beEqualTo(0).await
    }

    "throw an exception if M != 0 for a disk unit" in {
      // A = 1, I = 0, F = 4, C = 35 IOC
      execute(state, MixWord(0x00040123)) must throwA[WrongMemoryAddressException].await
    }

    "switch page on a line printer" in {
      // A = 0, I = 0, F = 5, C = 35 IOC
      val nextState = execute(state, MixWord(0x00000163))
      val device = nextState.map(_.devices(5))
      device.map(_._1 match { case d: MockLinePrinter => d.page }) must beGreaterThan(0).await
      device.map(_._2.length) must beEqualTo(0).await
    }

    "throw an exception if M != 0 for a line printer" in {
      // A = 1, I = 0, F = 5, C = 35 IOC
      execute(state, MixWord(0x00040163)) must throwA[WrongMemoryAddressException].await
    }

    "call reset for a paper tape" in {
      // A = 0, I = 0, F = 6, C = 35 IOC
      val nextState = execute(state, MixWord(0x000001a3))
      val device = nextState.map(_.devices(6))
      device.map(_._1 match { case d: MockPaperTape => d.counter }) must beGreaterThan(0).await
      device.map(_._2.length) must beEqualTo(0).await
    }

    "throw an exception if M != 0 for a paper tape" in {
      // A = 1, I = 0, F = 6, C = 35 IOC
      execute(state, MixWord(0x000401a3)) must throwA[WrongMemoryAddressException].await
    }

    "throw an exception if no device is connected" in {
      // A = 0, I = 0, F = 7, C = 35 IOC
      execute(state, MixWord(0x000001e3)) must throwA[DeviceNotConnectedException].like({
        case e: DeviceNotConnectedException => e.deviceNum must be equalTo 7
      }).await
    }
  }

  "device-conditional jump in the binary mode" should {
    "trigger jump ready and not trigger jump busy when device is not busy" in {
      // A = 1000, I = 0, F = 0, C = 38 JRED
      val nextStateRed = execute(state, MixWord(0x0fa00026))
      nextStateRed.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateRed.map(_.registers.getJ) must beEqualTo(MixIndex(1)).await
      // A = 1000, I = 0, F = 0, C = 34 JBUS
      val nextStateBus = execute(state, MixWord(0x0fa00022))
      nextStateBus.map(_.programCounter) must beEqualTo(MixIndex(1)).await
      nextStateBus.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }

    "throw an exception on jump ready when device is busy" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      val prevState = execute(state, MixWord(0x0fa00024))
      // A = 1000, I = 0, F = 0, C = 38 JRED
      prevState.flatMap(execute(_, MixWord(0x0fa00026))) must throwAn[UnpredictableExecutionFlowException].await
    }

    "throw an exception on jump busy when device is busy and the address is not the same as the program counter" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      val prevState = execute(state, MixWord(0x0fa00024))
      // A = 1000, I = 0, F = 0, C = 34 JBUS
      prevState.flatMap(execute(_, MixWord(0x0fa00022))) must throwAn[UnpredictableExecutionFlowException].await
    }

    "flush device input on jump busy when device is reading and the address is the same as the program counter" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      val prevState = execute(state, MixWord(0x0fa00024))
      // A = 1, I = 0, F = 0, C = 34 JBUS
      val nextState = prevState.flatMap(execute(_, MixWord(0x00040022)))
      nextState.map(_.memory.exclusiveLocks) must not(contain((MixIndex(1000), MockPositionalInputDevice.blockSize, 0))).await
      nextState.map(_.programCounter) must beEqualTo(MixIndex(2)).await
      (0 until MockPositionalInputDevice.blockSize) forall { i =>
        nextState.map(_.memory.get((1000 + i).toShort)) must beEqualTo(MixWord(0x01041041)).await
      }
      nextState.map(_.devices(0)._1.isBusy) must beFalse.await
    }

    "throw an exception if no device is connected" in {
      // A = 1000, I = 0, F = 7, C = 34 JBUS
      execute(state, MixWord(0x0fa001e2)) must throwA[DeviceNotConnectedException].like({
        case e: DeviceNotConnectedException => e.deviceNum must be equalTo 7
      }).await
      // A = 1000, I = 0, F = 7, C = 38 JRED
      execute(state, MixWord(0x0fa001e6)) must throwA[DeviceNotConnectedException].like({
        case e: DeviceNotConnectedException => e.deviceNum must be equalTo 7
      }).await
    }
  }
}
