package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.exceptions.{DeviceNotConnectedException, UnpredictableExecutionFlowException, WrongMemoryAddressException}
import org.linnando.mixemulator.vm.io.mock._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

import scala.collection.immutable.Queue

class IOSpec(implicit ee: ExecutionEnv) extends Specification {

  import decimal._

  private val initialState = decimal.initialState
  private val state = initialState.copy(
    registers = initialState.registers.updatedX(MixWord(2000L)),
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

  "decimal input" should {
    "call read for a positional input device" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      val nextState = execute(state, MixWord(1000000036L))
      nextState.map(_.memory.exclusiveLocks) must contain((MixIndex(1000), MockPositionalInputDevice.blockSize, 0)).await
      val device = nextState.map(_.devices(0))
      device.map(_._1 match {
        case d: MockPositionalInputDevice => d.counter
        case _ => throw new Error
      }) must beGreaterThan(0).await
      device.map(_._2) must contain(MixIndex(1000)).await
    }

    "throw an exception for a positional output device" in {
      // A = 1000, I = 0, F = 1, C = 36 IN
      execute(state, MixWord(1000000136L)) must throwAn[UnsupportedOperationException].await
    }

    "call read for a random access device" in {
      // A = 1000, I = 0, F = 2, C = 36 IN
      val nextState = execute(state, MixWord(1000000236L))
      nextState.map(_.memory.exclusiveLocks) must contain((MixIndex(1000), MockRandomAccessIODevice.blockSize, 2)).await
      val device = nextState.map(_.devices(2))
      device.map(_._1 match {
        case d: MockRandomAccessIODevice => d.position
        case _ => throw new Error
      }) must beEqualTo(2000).await
      device.map(_._2) must contain(MixIndex(1000)).await
    }

    "throw an exception if no device is connected" in {
      // A = 1000, I = 0, F = 7, C = 36 IN
      execute(state, MixWord(1000000736L)) must throwA[DeviceNotConnectedException].like({
        case e: DeviceNotConnectedException => e.deviceNum must beEqualTo(7)
      }).await
    }
  }

  "decimal output" should {
    "throw an exception for a positional input device" in {
      // A = 1000, I = 0, F = 0, C = 37 OUT
      execute(state, MixWord(1000000037L)) must throwAn[UnsupportedOperationException].await
    }

    "call write for a positional output device" in {
      // A = 1000, I = 0, F = 1, C = 37 OUT
      val nextState = execute(state, MixWord(1000000137L))
      nextState.map(_.memory.sharedLocks) must contain((MixIndex(1000), MockPositionalOutputDevice.blockSize, 1)).await
      val device = nextState.map(_.devices(1))
      device.map(_._1 match {
        case d: MockPositionalOutputDevice => d.counter
        case _ => throw new Error
      }) must beGreaterThan(0).await
      device.map(_._1 match {
        case d: MockPositionalOutputDevice => d.block
        case _ => throw new Error
      }) must not be empty.await
      device.map(_._2.length) must beEqualTo(0).await
    }

    "call write for a random access device" in {
      // A = 1000, I = 0, F = 2, C = 37 OUT
      val nextState = execute(state, MixWord(1000000237L))
      nextState.map(_.memory.sharedLocks) must contain((MixIndex(1000), MockRandomAccessIODevice.blockSize, 2)).await
      val device = nextState.map(_.devices(2))
      device.map(_._1 match { case d: MockRandomAccessIODevice => d.position }) must beEqualTo(2000).await
      device.map(_._1 match { case d: MockRandomAccessIODevice => d.block }) must not be empty.await
      device.map(_._2.length) must beEqualTo(0).await
    }

    "throw an exception if no device is connected" in {
      // A = 1000, I = 0, F = 7, C = 37 OUT
      execute(state, MixWord(1000000737L)) must throwA[DeviceNotConnectedException].like({
        case e: DeviceNotConnectedException => e.deviceNum must beEqualTo(7)
      }).await
    }
  }

  "decimal input/output control" should {
    "throw an exception for a generic device" in {
      // A = 0, I = 0, F = 0, C = 35 IOC
      execute(state, MixWord(35L)) must throwAn[UnsupportedOperationException].await
      // A = 0, I = 0, F = 1, C = 35 IOC
      execute(state, MixWord(135L)) must throwAn[UnsupportedOperationException].await
      // A = 0, I = 0, F = 2, C = 35 IOC
      execute(state, MixWord(235L)) must throwAn[UnsupportedOperationException].await
    }

    "position a tape unit" in {
      // A = 10, I = 0, F = 3, C = 35 IOC
      val nextState = execute(state, MixWord(10000335L))
      val device = nextState.map(_.devices(3))
      device.map(_._1 match { case d: MockTapeUnit => d.position }) must beEqualTo(10).await
      device.map(_._2.length) must beEqualTo(0).await
    }

    "position a disk unit" in {
      // A = 0, I = 0, F = 4, C = 35 IOC
      val nextState = execute(state, MixWord(435L))
      val device = nextState.map(_.devices(4))
      device.map(_._1 match { case d: MockDiskUnit => d.position }) must beEqualTo(2000).await
      device.map(_._2.length) must beEqualTo(0).await
    }

    "throw an exception if M != 0 for a disk unit" in {
      // A = 1, I = 0, F = 4, C = 35 IOC
      execute(state, MixWord(1000435L)) must throwA[WrongMemoryAddressException].await
    }

    "switch page on a line printer" in {
      // A = 0, I = 0, F = 5, C = 35 IOC
      val nextState = execute(state, MixWord(535L))
      val device = nextState.map(_.devices(5))
      device.map(_._1 match { case d: MockLinePrinter => d.page }) must beGreaterThan(0).await
      device.map(_._2.length) must beEqualTo(0).await
    }

    "throw an exception if M != 0 for a line printer" in {
      // A = 1, I = 0, F = 5, C = 35 IOC
      execute(state, MixWord(1000535L)) must throwA[WrongMemoryAddressException].await
    }

    "call reset for a paper tape" in {
      // A = 0, I = 0, F = 6, C = 35 IOC
      val nextState = execute(state, MixWord(635L))
      val device = nextState.map(_.devices(6))
      device.map(_._1 match { case d: MockPaperTape => d.counter }) must beGreaterThan(0).await
      device.map(_._2.length) must beEqualTo(0).await
    }

    "throw an exception if M != 0 for a paper tape" in {
      // A = 1, I = 0, F = 6, C = 35 IOC
      execute(state, MixWord(1000635L)) must throwA[WrongMemoryAddressException].await
    }

    "throw an exception if no device is connected" in {
      // A = 0, I = 0, F = 7, C = 35 IOC
      execute(state, MixWord(735L)) must throwA[DeviceNotConnectedException].like({
        case e: DeviceNotConnectedException => e.deviceNum must beEqualTo(7)
      }).await
    }
  }

  "device-conditional jump in the decimal mode" should {
    "trigger jump ready and not trigger jump busy when device is not busy" in {
      // A = 1000, I = 0, F = 0, C = 38 JRED
      val nextStateRed = execute(state, MixWord(1000000038L))
      nextStateRed.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateRed.map(_.registers.getJ) must beEqualTo(MixIndex(1)).await
      // A = 1000, I = 0, F = 0, C = 34 JBUS
      val nextStateBus = execute(state, MixWord(1000000034L))
      nextStateBus.map(_.programCounter) must beEqualTo(MixIndex(1)).await
      nextStateBus.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }

    "throw an exception on jump ready when device is busy" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      val prevState = execute(state, MixWord(1000000036))
      // A = 1000, I = 0, F = 0, C = 38 JRED
      prevState.flatMap(execute(_, MixWord(1000000038L))) must throwAn[UnpredictableExecutionFlowException].await
    }

    "throw an exception on jump busy when device is busy and the address is not the same as the program counter" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      val prevState = execute(state, MixWord(1000000036L))
      // A = 1000, I = 0, F = 0, C = 34 JBUS
      prevState.flatMap(execute(_, MixWord(1000000034L))) must throwAn[UnpredictableExecutionFlowException].await
    }

    "flush device input on jump busy when device is reading and the address is the same as the program counter" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      val prevState = execute(state, MixWord(1000000036L))
      // A = 1, I = 0, F = 0, C = 34 JBUS
      val nextState = prevState.flatMap(execute(_, MixWord(1000034L)))
      nextState.map(_.memory.exclusiveLocks) must not(contain((MixIndex(1000), MockPositionalInputDevice.blockSize, 0))).await
      nextState.map(_.programCounter) must beEqualTo(MixIndex(2)).await
      (0 until MockPositionalInputDevice.blockSize) forall { i =>
        nextState.map(_.memory.get((1000 + i).toShort)) must beEqualTo(MixWord(101010101L)).await
      }
      nextState.map(_.devices(0)._1.isBusy) must beFalse.await
    }

    "throw an exception if no device is connected" in {
      // A = 1000, I = 0, F = 7, C = 34 JBUS
      execute(state, MixWord(1000000734L)) must throwA[DeviceNotConnectedException].like({
        case e: DeviceNotConnectedException => e.deviceNum must beEqualTo(7)
      }).await
      // A = 1000, I = 0, F = 7, C = 38 JRED
      execute(state, MixWord(1000000738L)) must throwA[DeviceNotConnectedException].like({
        case e: DeviceNotConnectedException => e.deviceNum must beEqualTo(7)
      }).await
    }
  }
}
