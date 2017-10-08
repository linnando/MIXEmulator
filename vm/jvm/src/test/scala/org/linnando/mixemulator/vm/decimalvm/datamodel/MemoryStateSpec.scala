package org.linnando.mixemulator.vm.decimalvm.datamodel

import org.linnando.mixemulator.vm.VirtualMachine
import org.linnando.mixemulator.vm.decimal._
import org.linnando.mixemulator.vm.exceptions.{InconsistentReadException, WriteConflictException, WrongFieldSpecException, WrongMemoryAddressException}
import org.specs2.mutable.Specification

class MemoryStateSpec extends Specification {
  private val initialState = MemoryState.initialState

  "decimal memory state" should {
    "get memory contents by MixIndex" in {
      val state = initialState.copy(contents = initialState.contents.updated(1, 2L))
      state.get(MixIndex(1)) must be equalTo MixWord(2L)
    }

    "get memory contents by Short" in {
      val state = initialState.copy(contents = initialState.contents.updated(1, 2L))
      state.get(1.toShort) must be equalTo MixWord(2L)
    }

    "update memory contents" in {
      val state = initialState.updated(MixIndex(1), MixWord(2L))
      state.contents(1) must be equalTo 2L
    }

    "throw an exception if memory address is negative" in {
      initialState.get(0x4001.toShort) must throwA[WrongMemoryAddressException]
      initialState.updated(MixIndex(0x4001), MixWord(1)) must throwA[WrongMemoryAddressException]
    }

    "throw an exception if memory address is too big" in {
      initialState.get(VirtualMachine.MEMORY_SIZE) must throwA[WrongMemoryAddressException]
      initialState.updated(MixIndex(VirtualMachine.MEMORY_SIZE), MixWord(1)) must throwA[WrongMemoryAddressException]
      initialState.updated(MixIndex(VirtualMachine.MEMORY_SIZE), MixByte(5), MixWord(1)) must throwA[WrongMemoryAddressException]
    }
  }

  "decimal memory field update" should {
    "update a field of a positive word" in {
      val state = initialState.copy(contents = initialState.contents.updated(0, 102030405L)) // + 1 2 3 4 5
      val word = MixWord(607080900L) // + 6 7 8 9 0
      state.updated(MixIndex(0), MixByte(0x00), word).contents(0) must be equalTo 102030405L // + 1 2 3 4 5
      state.updated(MixIndex(0), MixByte(0x01), word).contents(0) must be equalTo 2030405L // + 0 2 3 4 5
      state.updated(MixIndex(0), MixByte(0x02), word).contents(0) must be equalTo 900030405L // + 9 0 3 4 5
      state.updated(MixIndex(0), MixByte(0x03), word).contents(0) must be equalTo 809000405L // + 8 9 0 4 5
      state.updated(MixIndex(0), MixByte(0x04), word).contents(0) must be equalTo 708090005L // + 7 8 9 0 5
      state.updated(MixIndex(0), MixByte(0x05), word).contents(0) must be equalTo 607080900L // + 6 7 8 9 0
      state.updated(MixIndex(0), MixByte(0x09), word).contents(0) must be equalTo 2030405L // + 0 2 3 4 5
      state.updated(MixIndex(0), MixByte(0x0a), word).contents(0) must be equalTo 900030405L // + 9 0 3 4 5
      state.updated(MixIndex(0), MixByte(0x0b), word).contents(0) must be equalTo 809000405L // + 8 9 0 4 5
      state.updated(MixIndex(0), MixByte(0x0c), word).contents(0) must be equalTo 708090005L // + 7 8 9 0 5
      state.updated(MixIndex(0), MixByte(0x0d), word).contents(0) must be equalTo 607080900L // + 6 7 8 9 0
      state.updated(MixIndex(0), MixByte(0x12), word).contents(0) must be equalTo 100030405L // + 1 0 3 4 5
      state.updated(MixIndex(0), MixByte(0x13), word).contents(0) must be equalTo 109000405L // + 1 9 0 4 5
      state.updated(MixIndex(0), MixByte(0x14), word).contents(0) must be equalTo 108090005L // + 1 8 9 0 5
      state.updated(MixIndex(0), MixByte(0x15), word).contents(0) must be equalTo 107080900L // + 1 7 8 9 0
      state.updated(MixIndex(0), MixByte(0x1b), word).contents(0) must be equalTo 102000405L // + 1 2 0 4 5
      state.updated(MixIndex(0), MixByte(0x1c), word).contents(0) must be equalTo 102090005L // + 1 2 9 0 5
      state.updated(MixIndex(0), MixByte(0x1d), word).contents(0) must be equalTo 102080900L // + 1 2 8 9 0
      state.updated(MixIndex(0), MixByte(0x24), word).contents(0) must be equalTo 102030005L // + 1 2 3 0 5
      state.updated(MixIndex(0), MixByte(0x25), word).contents(0) must be equalTo 102030900L // + 1 2 3 9 0
      state.updated(MixIndex(0), MixByte(0x2d), word).contents(0) must be equalTo 102030400L // + 1 2 3 4 0
    }

    "update a field of a negative word" in {
      val state = initialState.copy(contents = initialState.contents.updated(0, 0x400000000L | 102030405L)) // - 1 2 3 4 5
      val word = MixWord(607080900L) // + 6 7 8 9 0
      state.updated(MixIndex(0), MixByte(0x00), word).contents(0) must be equalTo 102030405L // + 1 2 3 4 5
      state.updated(MixIndex(0), MixByte(0x01), word).contents(0) must be equalTo 2030405L // + 0 2 3 4 5
      state.updated(MixIndex(0), MixByte(0x02), word).contents(0) must be equalTo 900030405L // + 9 0 3 4 5
      state.updated(MixIndex(0), MixByte(0x03), word).contents(0) must be equalTo 809000405L // + 8 9 0 4 5
      state.updated(MixIndex(0), MixByte(0x04), word).contents(0) must be equalTo 708090005L // + 7 8 9 0 5
      state.updated(MixIndex(0), MixByte(0x05), word).contents(0) must be equalTo 607080900L // + 6 7 8 9 0
      state.updated(MixIndex(0), MixByte(0x09), word).contents(0) must be equalTo 0x400000000L | 2030405L // - 0 2 3 4 5
      state.updated(MixIndex(0), MixByte(0x0a), word).contents(0) must be equalTo 0x400000000L | 900030405L // - 9 0 3 4 5
      state.updated(MixIndex(0), MixByte(0x0b), word).contents(0) must be equalTo 0x400000000L | 809000405L // - 8 9 0 4 5
      state.updated(MixIndex(0), MixByte(0x0c), word).contents(0) must be equalTo 0x400000000L | 708090005L // - 7 8 9 0 5
      state.updated(MixIndex(0), MixByte(0x0d), word).contents(0) must be equalTo 0x400000000L | 607080900L // - 6 7 8 9 0
      state.updated(MixIndex(0), MixByte(0x12), word).contents(0) must be equalTo 0x400000000L | 100030405L // - 1 0 3 4 5
      state.updated(MixIndex(0), MixByte(0x13), word).contents(0) must be equalTo 0x400000000L | 109000405L // - 1 9 0 4 5
      state.updated(MixIndex(0), MixByte(0x14), word).contents(0) must be equalTo 0x400000000L | 108090005L // - 1 8 9 0 5
      state.updated(MixIndex(0), MixByte(0x15), word).contents(0) must be equalTo 0x400000000L | 107080900L // - 1 7 8 9 0
      state.updated(MixIndex(0), MixByte(0x1b), word).contents(0) must be equalTo 0x400000000L | 102000405L // - 1 2 0 4 5
      state.updated(MixIndex(0), MixByte(0x1c), word).contents(0) must be equalTo 0x400000000L | 102090005L // - 1 2 9 0 5
      state.updated(MixIndex(0), MixByte(0x1d), word).contents(0) must be equalTo 0x400000000L | 102080900L // - 1 2 8 9 0
      state.updated(MixIndex(0), MixByte(0x24), word).contents(0) must be equalTo 0x400000000L | 102030005L // - 1 2 3 0 5
      state.updated(MixIndex(0), MixByte(0x25), word).contents(0) must be equalTo 0x400000000L | 102030900L // - 1 2 3 9 0
      state.updated(MixIndex(0), MixByte(0x2d), word).contents(0) must be equalTo 0x400000000L | 102030400L // - 1 2 3 4 0
    }

    "throw an exception if field number is wrong" in {
      initialState.updated(MixIndex(0x0), MixByte(0x06), MixWord(0x0)) must throwA[WrongFieldSpecException]
    }

    "throw an exception if l > r in a field spec" in {
      initialState.updated(MixIndex(0x0), MixByte(0x08), MixWord(0x0)) must throwA[WrongFieldSpecException]
    }
  }

  "decimal memory locking" should {
    "not allow changing a memory cell under a shared lock" in {
      val state = initialState.withSharedLock(MixIndex(0), 100, 0)
      state.updated(MixIndex(99), MixWord(0L)) must throwAn[InconsistentReadException]
      state.updated(MixIndex(99), MixByte(5), MixWord(0L)) must throwAn[InconsistentReadException]
    }

    "allow reading a memory cell under a shared lock" in {
      val state = initialState.withSharedLock(MixIndex(0), 100, 0)
      state.get(99.toShort) must be equalTo MixWord(0L)
    }

    "not allow changing a memory cell under an exclusive lock" in {
      val state = initialState.withExclusiveLock(MixIndex(0), 100, 0)
      state.updated(MixIndex(99), MixWord(0L)) must throwA[WriteConflictException]
      state.updated(MixIndex(99), MixByte(5), MixWord(0L)) must throwA[WriteConflictException]
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
      state.updated(MixIndex(0), MixWord(0L)) must not(throwAn[Exception])
    }

    "allow changing a memory cell not covered by a lock" in {
      val state = initialState.withExclusiveLock(MixIndex(0), 100, 0)
      state.updated(MixIndex(100), MixWord(0L)) must not(throwA[Exception])
    }

    "throw an exception if memory address is too big" in {
      val startAddress = MixIndex((VirtualMachine.MEMORY_SIZE - 99).toShort)
      initialState.withSharedLock(startAddress, 100, 0) must throwA[WrongMemoryAddressException]
      initialState.withExclusiveLock(startAddress, 100, 0) must throwA[WrongMemoryAddressException]
    }
  }
}
