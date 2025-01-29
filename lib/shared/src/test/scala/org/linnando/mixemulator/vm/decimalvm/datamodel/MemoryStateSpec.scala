package org.linnando.mixemulator.vm.decimalvm.datamodel

import org.linnando.mixemulator.vm.VirtualMachine
import org.linnando.mixemulator.vm.decimal._
import org.linnando.mixemulator.vm.exceptions.{InconsistentReadException, WriteConflictException, WrongMemoryAddressException}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class MemoryStateSpec extends AsyncWordSpec with Matchers {
  private val initialState = MemoryState.initialState

  "decimal memory state" should {
    "get memory contents by MixIndex" in {
      val state = initialState.copy(contents = initialState.contents.updated(1, 2L))
      state.get(MixIndex(1)) mustEqual MixWord(2L)
    }

    "get memory contents by Short" in {
      val state = initialState.copy(contents = initialState.contents.updated(1, 2L))
      state.get(1.toShort) mustEqual MixWord(2L)
    }

    "update memory contents" in {
      val state = initialState.updated(MixIndex(1), MixWord(2L))
      state.contents(1) mustEqual 2L
    }

    "throw an exception if memory address is negative" in {
      a[WrongMemoryAddressException] must be thrownBy initialState.get(0x4001.toShort)
      a[WrongMemoryAddressException] must be thrownBy initialState.updated(MixIndex(0x4001), MixWord(1))
    }

    "throw an exception if memory address is too big" in {
      a[WrongMemoryAddressException] must be thrownBy initialState.get(VirtualMachine.MEMORY_SIZE)
      a[WrongMemoryAddressException] must be thrownBy initialState.updated(MixIndex(VirtualMachine.MEMORY_SIZE), MixWord(1))
    }
  }

  "decimal memory locking" should {
    "not allow changing a memory cell under a shared lock" in {
      val state = initialState.withSharedLock(MixIndex(0), 100, 0)
      an[InconsistentReadException] must be thrownBy state.updated(MixIndex(99), MixWord(0L))
    }

    "allow reading a memory cell under a shared lock" in {
      val state = initialState.withSharedLock(MixIndex(0), 100, 0)
      state.get(99.toShort) mustEqual MixWord(0L)
    }

    "not allow changing a memory cell under an exclusive lock" in {
      val state = initialState.withExclusiveLock(MixIndex(0), 100, 0)
      a[WriteConflictException] must be thrownBy state.updated(MixIndex(99), MixWord(0L))
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
      noException must be thrownBy state.updated(MixIndex(0), MixWord(0L))
    }

    "allow changing a memory cell not covered by a lock" in {
      val state = initialState.withExclusiveLock(MixIndex(0), 100, 0)
      noException must be thrownBy state.updated(MixIndex(100), MixWord(0L))
    }

    "throw an exception if memory address is too big" in {
      val startAddress = MixIndex((VirtualMachine.MEMORY_SIZE - 99).toShort)
      a[WrongMemoryAddressException] must be thrownBy initialState.withSharedLock(startAddress, 100, 0)
      a[WrongMemoryAddressException] must be thrownBy initialState.withExclusiveLock(startAddress, 100, 0)
    }
  }
}
