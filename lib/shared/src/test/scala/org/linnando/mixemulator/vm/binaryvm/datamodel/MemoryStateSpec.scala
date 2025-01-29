package org.linnando.mixemulator.vm.binaryvm.datamodel

import org.linnando.mixemulator.vm.VirtualMachine
import org.linnando.mixemulator.vm.binary._
import org.linnando.mixemulator.vm.exceptions.{InconsistentReadException, WriteConflictException, WrongMemoryAddressException}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MemoryStateSpec extends AnyWordSpec with Matchers {
  private val initialState = MemoryState.initialState

  "binary memory state" should {
    "get memory contents by MixIndex" in {
      val state = initialState.copy(contents = initialState.contents.updated(1, 0x2))
      state.get(MixIndex(1)) mustEqual MixWord(0x2)
    }

    "get memory contents by Short" in {
      val state = initialState.copy(contents = initialState.contents.updated(1, 0x2))
      state.get(1.toShort) mustEqual MixWord(0x2)
    }

    "update memory contents" in {
      val state = initialState.updated(MixIndex(0x1), MixWord(0x2))
      state.contents(1) mustEqual 0x2
    }

    "throw an exception if memory address is negative" in {
      val getException = the[WrongMemoryAddressException] thrownBy initialState.get(MixIndex(0x1001.toShort))
      getException.address mustEqual -1L
      val updateException = the[WrongMemoryAddressException] thrownBy initialState.updated(MixIndex(0x1001), MixWord(0x1))
      updateException.address mustEqual -1L
    }

    "throw an exception if memory address is too big" in {
      val getException = the[WrongMemoryAddressException] thrownBy initialState.get(VirtualMachine.MEMORY_SIZE)
      getException.address mustEqual VirtualMachine.MEMORY_SIZE
      val updateException = the[WrongMemoryAddressException] thrownBy initialState.updated(MixIndex(VirtualMachine.MEMORY_SIZE), MixWord(0x1))
      updateException.address mustEqual VirtualMachine.MEMORY_SIZE
    }
  }

  "binary memory locking" should {
    "not allow changing a memory cell under a shared lock" in {
      val state = initialState.withSharedLock(MixIndex(0), 100, 0)
      an[InconsistentReadException] must be thrownBy state.updated(MixIndex(99), MixWord(0x0))
    }

    "allow reading a memory cell under a shared lock" in {
      val state = initialState.withSharedLock(MixIndex(0), 100, 0)
      state.get(99.toShort) mustEqual MixWord(0x0)
    }

    "not allow changing a memory cell under an exclusive lock" in {
      val state = initialState.withExclusiveLock(MixIndex(0), 100, 0)
      a[WriteConflictException] must be thrownBy state.updated(MixIndex(99), MixWord(0x0))
    }

    "not allow reading a memory cell under an exclusive lock" in {
      val state = initialState.withExclusiveLock(MixIndex(0), 100, 0)
      an[InconsistentReadException] must be thrownBy state.get(99.toShort)
    }

    "not allow acquiring an exclusive lock intersecting an earlier exclusive lock" in {
      val state = initialState.withExclusiveLock(MixIndex(0), 100, 0)
      a[WriteConflictException] must be thrownBy state.withExclusiveLock(MixIndex(99), 100, 1)
    }

    "not allow acquiring an exclusive lock intersecting an earlier shared lock" in {
      val state = initialState.withSharedLock(MixIndex(0), 100, 0)
      an[InconsistentReadException] must be thrownBy state.withExclusiveLock(MixIndex(99), 100, 1)
    }

    "not allow acquiring a shared lock intersecting an earlier exclusive lock" in {
      val state = initialState.withSharedLock(MixIndex(0), 100, 0)
      an[InconsistentReadException] must be thrownBy state.withExclusiveLock(MixIndex(99), 100, 1)
    }

    "allow acquiring a shared lock intersecting an earlier shared lock" in {
      val state = initialState.withSharedLock(MixIndex(0), 100, 0)
      noException must be thrownBy state.withSharedLock(MixIndex(99), 100, 1)
    }

    "allow changing a memory cell after lock release" in {
      val state = initialState.withExclusiveLock(MixIndex(0), 100, 0).withoutLocks(0)
      noException must be thrownBy state.updated(MixIndex(0), MixWord(0x0))
    }

    "allow changing a memory cell not covered by a lock" in {
      val state = initialState.withExclusiveLock(MixIndex(0), 100, 0)
      noException must be thrownBy state.updated(MixIndex(100), MixWord(0x0))
    }

    "throw an exception if memory address is too big" in {
      val startAddress = MixIndex((VirtualMachine.MEMORY_SIZE - 99).toShort)
      val sharedLockException = the[WrongMemoryAddressException] thrownBy initialState.withSharedLock(startAddress, 100, 0)
      sharedLockException.address mustEqual VirtualMachine.MEMORY_SIZE - 99
      val exclusiveLockException = the[WrongMemoryAddressException] thrownBy initialState.withExclusiveLock(startAddress, 100, 0)
      exclusiveLockException.address mustEqual VirtualMachine.MEMORY_SIZE - 99
    }
  }
}
