package org.linnando.mixemulator.vm.binaryvm.datamodel

import org.linnando.mixemulator.vm.VirtualMachine
import org.linnando.mixemulator.vm.binary._
import org.linnando.mixemulator.vm.exceptions.{InconsistentReadException, WriteConflictException, WrongFieldSpecException, WrongMemoryAddressException}
import org.specs2.mutable.Specification

class MemoryStateSpec extends Specification {
  private val initialState = MemoryState.initialState

  "binary memory state" should {
    "get memory contents by MixIndex" in {
      val state = initialState.copy(contents = initialState.contents.updated(1, 0x2))
      state.get(MixIndex(1)) must be equalTo MixWord(0x2)
    }

    "get memory contents by Short" in {
      val state = initialState.copy(contents = initialState.contents.updated(1, 0x2))
      state.get(1.toShort) must be equalTo MixWord(0x2)
    }

    "update memory contents" in {
      val state = initialState.updated(MixIndex(0x1), MixWord(0x2))
      state.contents(1) must be equalTo 0x2
    }

    "throw an exception if memory address is negative" in {
      initialState.get(0x1001.toShort) must throwA[WrongMemoryAddressException]
      initialState.updated(MixIndex(0x1001), MixWord(0x1)) must throwA[WrongMemoryAddressException]
    }

    "throw an exception if memory address is too big" in {
      initialState.get(VirtualMachine.MEMORY_SIZE) must throwA[WrongMemoryAddressException]
      initialState.updated(MixIndex(VirtualMachine.MEMORY_SIZE), MixWord(0x1)) must throwA[WrongMemoryAddressException]
    }
  }

  "binary memory locking" should {
    "not allow changing a memory cell under a shared lock" in {
      val state = initialState.withSharedLock(MixIndex(0), 100, 0)
      state.updated(MixIndex(99), MixWord(0x0)) must throwAn[InconsistentReadException]
    }

    "allow reading a memory cell under a shared lock" in {
      val state = initialState.withSharedLock(MixIndex(0), 100, 0)
      state.get(99.toShort) must be equalTo MixWord(0x0)
    }

    "not allow changing a memory cell under an exclusive lock" in {
      val state = initialState.withExclusiveLock(MixIndex(0), 100, 0)
      state.updated(MixIndex(99), MixWord(0x0)) must throwA[WriteConflictException]
    }

    "not allow reading a memory cell under an exclusive lock" in {
      val state = initialState.withExclusiveLock(MixIndex(0), 100, 0)
      state.get(99.toShort) must throwAn[InconsistentReadException]
    }

    "not allow acquiring an exclusive lock intersecting an earlier exclusive lock" in {
      val state = initialState.withExclusiveLock(MixIndex(0), 100, 0)
      state.withExclusiveLock(MixIndex(99), 100, 1) must throwA[WriteConflictException]
    }

    "not allow acquiring an exclusive lock intersecting an earlier shared lock" in {
      val state = initialState.withSharedLock(MixIndex(0), 100, 0)
      state.withExclusiveLock(MixIndex(99), 100, 1) must throwAn[InconsistentReadException]
    }

    "not allow acquiring a shared lock intersecting an earlier exclusive lock" in {
      val state = initialState.withSharedLock(MixIndex(0), 100, 0)
      state.withExclusiveLock(MixIndex(99), 100, 1) must throwAn[InconsistentReadException]
    }

    "allow acquiring a shared lock intersecting an earlier shared lock" in {
      val state = initialState.withSharedLock(MixIndex(0), 100, 0)
      state.withSharedLock(MixIndex(99), 100, 1) must not(throwA[Exception])
    }

    "allow changing a memory cell after lock release" in {
      val state = initialState.withExclusiveLock(MixIndex(0), 100, 0).withoutLocks(0)
      state.updated(MixIndex(0), MixWord(0x0)) must not(throwA[Exception])
    }

    "allow changing a memory cell not covered by a lock" in {
      val state = initialState.withExclusiveLock(MixIndex(0), 100, 0)
      state.updated(MixIndex(100), MixWord(0x0)) must not(throwA[Exception])
    }

    "throw an exception if memory address is too big" in {
      val startAddress = MixIndex((VirtualMachine.MEMORY_SIZE - 99).toShort)
      initialState.withSharedLock(startAddress, 100, 0) must throwA[WrongMemoryAddressException]
      initialState.withExclusiveLock(startAddress, 100, 0) must throwA[WrongMemoryAddressException]
    }
  }
}
