package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.binary
import org.linnando.mixemulator.vm.exceptions.{DeviceNotConnectedException, UnpredictableExecutionFlowException, UnsupportedIoOperationException, WrongMemoryAddressException}
import org.linnando.mixemulator.vm.io.mock._
import org.scalatest.{Inside, Inspectors, OptionValues}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class IOSpec extends AsyncWordSpec with Matchers with Inside with Inspectors with OptionValues {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  import binary._

  private val initialState = binary.initialState
  private val state = initialState.copy(
    registers = initialState.registers.updatedX(MixWord(2000)),
    devices = initialState.devices ++ Seq(
      0 -> (MockPositionalInputDevice(), None),
      1 -> (MockPositionalOutputDevice(), None),
      2 -> (MockRandomAccessIODevice(), None),
      3 -> (MockTapeUnit(), None),
      4 -> (MockDiskUnit(), None),
      5 -> (MockLinePrinter(), None),
      6 -> (MockPaperTape(), None)
    )
  )

  "generic positional input device in the binary mode" should {
    "input data" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      execute(state, MixWord(0x0fa00024)).map(s => {
        s.memory.exclusiveLocks must contain((MixIndex(1000), MockPositionalInputDevice.blockSize, 0))
        inside(s.devices(0)) { case (device: MockPositionalInputDevice, destination: Option[MixIndex]) =>
          device.counter must be > 0
          destination.value mustEqual MixIndex(1000)
        }
      })
    }

    "throw an exception on an attempt to output data" in {
      recoverToSucceededIf[UnsupportedIoOperationException] {
        // A = 1000, I = 0, F = 0, C = 37 OUT
        execute(state, MixWord(0x0fa00025))
      }
    }

    "throw an exception on input/output control command" in {
      recoverToSucceededIf[UnsupportedIoOperationException] {
        // A = 0, I = 0, F = 0, C = 35 IOC
        execute(state, MixWord(0x00000023))
      }
    }

    "trigger jump ready when device is not busy" in {
      // A = 1000, I = 0, F = 0, C = 38 JRED
      execute(state, MixWord(0x0fa00026)).map(s => {
        s.programCounter mustEqual MixIndex(1000)
        s.registers.getJ mustEqual MixIndex(1)
      })
    }

    "not trigger jump busy when device is not busy" in {
      // A = 1000, I = 0, F = 0, C = 34 JBUS
      execute(state, MixWord(0x0fa00022)).map(s => {
        s.programCounter mustEqual MixIndex(1)
        s.registers.getJ mustEqual MixIndex(0)
      })
    }

    "throw an exception on jump ready when device is busy" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      val prevState = execute(state, MixWord(0x0fa00024))
      recoverToSucceededIf[UnpredictableExecutionFlowException] {
        // A = 1000, I = 0, F = 0, C = 38 JRED
        prevState.flatMap(execute(_, MixWord(0x0fa00026)))
      }
    }

    "throw an exception on jump busy when device is busy and the address is not the same as the program counter" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      val prevState = execute(state, MixWord(0x0fa00024))
      recoverToSucceededIf[UnpredictableExecutionFlowException] {
        // A = 1000, I = 0, F = 0, C = 34 JBUS
        prevState.flatMap(execute(_, MixWord(0x0fa00022)))
      }
    }

    "flush device input on jump busy when device is reading and the address is the same as the program counter" in {
      // A = 1000, I = 0, F = 0, C = 36 IN
      val prevState = execute(state, MixWord(0x0fa00024))
      // A = 1, I = 0, F = 0, C = 34 JBUS
      prevState.flatMap(execute(_, MixWord(0x00040022))).map(s => {
        s.memory.exclusiveLocks must not contain ((MixIndex(1000), MockPositionalInputDevice.blockSize, 0))
        s.programCounter mustEqual MixIndex(2)
        forAll(0 until MockPositionalInputDevice.blockSize) { i =>
          s.memory.get((1000 + i).toShort) mustEqual MixWord(0x01041041)
        }
        inside(s.devices(0)) { case (device, _) => device.isBusy mustEqual false }
      })
    }
  }

  "generic positional output device in the binary mode" should {
    "throw an exception on an attempt to input data" in {
      recoverToSucceededIf[UnsupportedIoOperationException] {
        // A = 1000, I = 0, F = 1, C = 36 IN
        execute(state, MixWord(0x0fa00064))
      }
    }

    "output data" in {
      // A = 1000, I = 0, F = 1, C = 37 OUT
      execute(state, MixWord(0x0fa00065)).map(s => {
        s.memory.sharedLocks must contain((MixIndex(1000), MockPositionalOutputDevice.blockSize, 1))
        inside(s.devices(1)) { case (device: MockPositionalOutputDevice, destination: Option[MixIndex]) =>
          device.counter must be > 0
          device.block must not be empty
          destination mustBe empty
        }
      })
    }

    "throw an exception on input/output control command" in {
      recoverToSucceededIf[UnsupportedIoOperationException] {
        // A = 0, I = 0, F = 1, C = 35 IOC
        execute(state, MixWord(0x00000063))
      }
    }
  }

  "generic random access device in the binary mode" should {
    "input data" in {
      // A = 1000, I = 0, F = 2, C = 36 IN
      execute(state, MixWord(0x0fa000a4)).map(s => {
        s.memory.exclusiveLocks must contain((MixIndex(1000), MockRandomAccessIODevice.blockSize, 2))
        inside(s.devices(2)) { case (device: MockRandomAccessIODevice, destination: Option[MixIndex]) =>
          device.position mustEqual 2000
          destination must contain(MixIndex(1000))
        }
      })
    }

    "output data" in {
      // A = 1000, I = 0, F = 2, C = 37 OUT
      execute(state, MixWord(0x0fa000a5)).map(s => {
        s.memory.sharedLocks must contain((MixIndex(1000), MockRandomAccessIODevice.blockSize, 2))
        inside(s.devices(2)) { case (device: MockRandomAccessIODevice, destination: Option[MixIndex]) =>
          device.position mustEqual 2000
          device.block must not be empty
          destination mustBe empty
        }
      })
    }

    "throw an exception on input/output control command" in {
      recoverToSucceededIf[UnsupportedIoOperationException] {
        // A = 0, I = 0, F = 2, C = 35 IOC
        execute(state, MixWord(0x000000a3))
      }
    }
  }

  "tape unit in the binary mode" should {
    "position the tape" in {
      // A = 10, I = 0, F = 3, C = 35 IOC
      execute(state, MixWord(0x002800e3)).map(s => {
        inside(s.devices(3)) { case (device: MockTapeUnit, destination: Option[MixIndex]) =>
          device.position mustEqual 10
          destination mustBe empty
        }
      })
    }
  }

  "disk unit in the binary mode" should {
    "position the disk" in {
      // A = 0, I = 0, F = 4, C = 35 IOC
      execute(state, MixWord(0x00000123)).map(s => {
        inside(s.devices(4)) { case (device: MockDiskUnit, destination: Option[MixIndex]) =>
          device.position mustEqual 2000
          destination mustBe empty
        }
      })
    }

    "throw an exception on input/output control command if M != 0" in {
      recoverToSucceededIf[WrongMemoryAddressException] {
        // A = 1, I = 0, F = 4, C = 35 IOC
        execute(state, MixWord(0x00040123))
      }
    }
  }

  "line printer in the binary mode" should {
    "switch page" in {
      // A = 0, I = 0, F = 5, C = 35 IOC
      execute(state, MixWord(0x00000163)).map(s => {
        inside(s.devices(5)) { case (device: MockLinePrinter, destination: Option[MixIndex]) =>
          device.page must be > 0
          destination mustBe empty
        }
      })
    }

    "throw an exception on input/output control command if M != 0" in {
      recoverToSucceededIf[WrongMemoryAddressException] {
        // A = 1, I = 0, F = 5, C = 35 IOC
        execute(state, MixWord(0x00040163))
      }
    }
  }

  "paper tape in the binary mode" should {
    "reset the tape" in {
      // A = 0, I = 0, F = 6, C = 35 IOC
      execute(state, MixWord(0x000001a3)).map(s => {
        inside(s.devices(6)) { case (d: MockPaperTape, destination: Option[MixIndex]) =>
          d.counter must be > 0
          destination mustBe empty
        }
      })
    }

    "throw an exception on input/output control command if M != 0" in {
      recoverToSucceededIf[WrongMemoryAddressException] {
        // A = 1, I = 0, F = 6, C = 35 IOC
        execute(state, MixWord(0x000401a3))
      }
    }
  }

  "absent device in the binary mode" should {
    "throw an exception on an attempt to input data" in {
      recoverToExceptionIf[DeviceNotConnectedException] {
        // A = 1000, I = 0, F = 7, C = 36 IN
        execute(state, MixWord(0x0fa001e4))
      } map {
        _.deviceNum mustEqual 7
      }
    }

    "throw an exception on an attempt to output data" in {
      recoverToExceptionIf[DeviceNotConnectedException] {
        // A = 1000, I = 0, F = 7, C = 37 OUT
        execute(state, MixWord(0x0fa001e5))
      } map {
        _.deviceNum mustEqual 7
      }
    }

    "throw an exception on input/output control command" in {
      recoverToExceptionIf[DeviceNotConnectedException] {
        // A = 0, I = 0, F = 7, C = 35 IOC
        execute(state, MixWord(0x000001e3))
      } map {
        _.deviceNum mustEqual 7
      }
    }

    "throw an exception on jump busy" in {
      recoverToExceptionIf[DeviceNotConnectedException] {
        // A = 1000, I = 0, F = 7, C = 34 JBUS
        execute(state, MixWord(0x0fa001e2))
      } map {
        _.deviceNum mustEqual 7
      }
    }

    "throw an exception on jump ready" in {
      recoverToExceptionIf[DeviceNotConnectedException] {
        // A = 1000, I = 0, F = 7, C = 38 JRED
        execute(state, MixWord(0x0fa001e6))
      } map {
        _.deviceNum mustEqual 7
      }
    }
  }
}
