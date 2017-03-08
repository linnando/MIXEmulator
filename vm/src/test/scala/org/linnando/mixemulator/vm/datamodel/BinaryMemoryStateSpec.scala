package org.linnando.mixemulator.vm.datamodel

import org.linnando.mixemulator.vm.BinaryVirtualMachine.{BinaryMemoryState, BinaryMixByte, BinaryMixIndex, BinaryMixWord}
import org.linnando.mixemulator.vm.exceptions.{InconsistentReadException, WriteConflictException, WrongFieldSpecException, WrongMemoryAddressException}
import org.specs2.mutable.Specification

class BinaryMemoryStateSpec extends Specification {
  import BinaryMemoryState.initialState

  "binary memory state" should {
    "get memory contents" in {
      val state = initialState.copy(contents = initialState.contents.updated(1, 0x2))
      state.get(BinaryMixIndex(0x1)) must be equalTo BinaryMixWord(0x2)
    }

    "update memory contents" in {
      val state = initialState.updated(BinaryMixIndex(0x1), BinaryMixWord(0x2))
      state.contents(1) must be equalTo 0x2
    }

    "throw an exception if memory address is negative" in {
      initialState.get(BinaryMixIndex(0x1001)) must throwA[WrongMemoryAddressException]
      initialState.updated(BinaryMixIndex(0x1001), BinaryMixWord(0x1)) must throwA[WrongMemoryAddressException]
    }

    "throw an exception if memory address is too big" in {
      initialState.get(BinaryMixIndex(BinaryMemoryState.MEMORY_SIZE)) must throwA[WrongMemoryAddressException]
      initialState.updated(BinaryMixIndex(BinaryMemoryState.MEMORY_SIZE), BinaryMixWord(0x1)) must throwA[WrongMemoryAddressException]
      initialState.updated(BinaryMixIndex(BinaryMemoryState.MEMORY_SIZE), BinaryMixByte(0x5), BinaryMixWord(0x1)) must throwA[WrongMemoryAddressException]
    }
  }

  "binary memory field update" should {
    "update a field of a positive word" in {
      val state = initialState.copy(contents = initialState.contents.updated(0, 0x01083105)) // + 1 2 3 4 5
      val word = BinaryMixWord(0x061c8240) // + 6 7 8 9 0
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x00), word).contents(0) must be equalTo 0x01083105 // + 1 2 3 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x01), word).contents(0) must be equalTo 0x00083105 // + 0 2 3 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x02), word).contents(0) must be equalTo 0x09003105 // + 9 0 3 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x03), word).contents(0) must be equalTo 0x08240105 // + 8 9 0 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x04), word).contents(0) must be equalTo 0x07209005 // + 7 8 9 0 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x05), word).contents(0) must be equalTo 0x061c8240 // + 6 7 8 9 0
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x09), word).contents(0) must be equalTo 0x00083105 // + 0 2 3 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x0a), word).contents(0) must be equalTo 0x09003105 // + 9 0 3 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x0b), word).contents(0) must be equalTo 0x08240105 // + 8 9 0 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x0c), word).contents(0) must be equalTo 0x07209005 // + 7 8 9 0 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x0d), word).contents(0) must be equalTo 0x061c8240 // + 6 7 8 9 0
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x12), word).contents(0) must be equalTo 0x01003105 // + 1 0 3 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x13), word).contents(0) must be equalTo 0x01240105 // + 1 9 0 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x14), word).contents(0) must be equalTo 0x01209005 // + 1 8 9 0 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x15), word).contents(0) must be equalTo 0x011c8240 // + 1 7 8 9 0
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x1b), word).contents(0) must be equalTo 0x01080105 // + 1 2 0 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x1c), word).contents(0) must be equalTo 0x01089005 // + 1 2 9 0 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x1d), word).contents(0) must be equalTo 0x01088240 // + 1 2 8 9 0
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x24), word).contents(0) must be equalTo 0x01083005 // + 1 2 3 0 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x25), word).contents(0) must be equalTo 0x01083240 // + 1 2 3 9 0
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x2d), word).contents(0) must be equalTo 0x01083100 // + 1 2 3 4 0
    }

    "update a field of a negative word" in {
      val state = initialState.copy(contents = initialState.contents.updated(0, 0x41083105)) // - 1 2 3 4 5
      val word = BinaryMixWord(0x061c8240) // + 6 7 8 9 0
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x00), word).contents(0) must be equalTo 0x01083105 // + 1 2 3 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x01), word).contents(0) must be equalTo 0x00083105 // + 0 2 3 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x02), word).contents(0) must be equalTo 0x09003105 // + 9 0 3 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x03), word).contents(0) must be equalTo 0x08240105 // + 8 9 0 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x04), word).contents(0) must be equalTo 0x07209005 // + 7 8 9 0 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x05), word).contents(0) must be equalTo 0x061c8240 // + 6 7 8 9 0
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x09), word).contents(0) must be equalTo 0x40083105 // - 0 2 3 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x0a), word).contents(0) must be equalTo 0x49003105 // - 9 0 3 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x0b), word).contents(0) must be equalTo 0x48240105 // - 8 9 0 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x0c), word).contents(0) must be equalTo 0x47209005 // - 7 8 9 0 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x0d), word).contents(0) must be equalTo 0x461c8240 // - 6 7 8 9 0
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x12), word).contents(0) must be equalTo 0x41003105 // - 1 0 3 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x13), word).contents(0) must be equalTo 0x41240105 // - 1 9 0 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x14), word).contents(0) must be equalTo 0x41209005 // - 1 8 9 0 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x15), word).contents(0) must be equalTo 0x411c8240 // - 1 7 8 9 0
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x1b), word).contents(0) must be equalTo 0x41080105 // - 1 2 0 4 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x1c), word).contents(0) must be equalTo 0x41089005 // - 1 2 9 0 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x1d), word).contents(0) must be equalTo 0x41088240 // - 1 2 8 9 0
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x24), word).contents(0) must be equalTo 0x41083005 // - 1 2 3 0 5
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x25), word).contents(0) must be equalTo 0x41083240 // - 1 2 3 9 0
      state.updated(BinaryMixIndex(0), BinaryMixByte(0x2d), word).contents(0) must be equalTo 0x41083100 // - 1 2 3 4 0
    }

    "throw an exception if field number is wrong" in {
      initialState.updated(BinaryMixIndex(0x0), BinaryMixByte(0x06), BinaryMixWord(0x0)) must throwA[WrongFieldSpecException]
    }

    "throw an exception if l > r in a field spec" in {
      initialState.updated(BinaryMixIndex(0x0), BinaryMixByte(0x08), BinaryMixWord(0x0)) must throwA[WrongFieldSpecException]
    }
  }

  "binary memory locking" should {
    "not allow changing a memory cell under a shared lock" in {
      val state = initialState.withSharedLock(BinaryMixIndex(0), 100, 0)
      state.updated(BinaryMixIndex(99), BinaryMixWord(0x0)) must throwAn[InconsistentReadException]
      state.updated(BinaryMixIndex(99), BinaryMixByte(5), BinaryMixWord(0x0)) must throwAn[InconsistentReadException]
    }

    "allow reading a memory cell under a shared lock" in {
      val state = initialState.withSharedLock(BinaryMixIndex(0), 100, 0)
      state.get(BinaryMixIndex(99)) must be equalTo BinaryMixWord(0x0)
    }

    "not allow changing a memory cell under an exclusive lock" in {
      val state = initialState.withExclusiveLock(BinaryMixIndex(0), 100, 0)
      state.updated(BinaryMixIndex(99), BinaryMixWord(0x0)) must throwA[WriteConflictException]
      state.updated(BinaryMixIndex(99), BinaryMixByte(5), BinaryMixWord(0x0)) must throwA[WriteConflictException]
    }

    "not allow reading a memory cell under an exclusive lock" in {
      val state = initialState.withExclusiveLock(BinaryMixIndex(0), 100, 0)
      state.get(BinaryMixIndex(99)) must throwAn[InconsistentReadException]
    }

    "not allow acquiring an exclusive lock intersecting an earlier exclusive lock" in {
      val state = initialState.withExclusiveLock(BinaryMixIndex(0), 100, 0)
      state.withExclusiveLock(BinaryMixIndex(99), 100, 1) must throwA[WriteConflictException]
    }

    "not allow acquiring an exclusive lock intersecting an earlier shared lock" in {
      val state = initialState.withSharedLock(BinaryMixIndex(0), 100, 0)
      state.withExclusiveLock(BinaryMixIndex(99), 100, 1) must throwAn[InconsistentReadException]
    }

    "not allow acquiring a shared lock intersecting an earlier exclusive lock" in {
      val state = initialState.withSharedLock(BinaryMixIndex(0), 100, 0)
      state.withExclusiveLock(BinaryMixIndex(99), 100, 1) must throwAn[InconsistentReadException]
    }

    "allow acquiring a shared lock intersecting an earlier shared lock" in {
      val state = initialState.withSharedLock(BinaryMixIndex(0), 100, 0)
      state.withSharedLock(BinaryMixIndex(99), 100, 1) must not(throwA[Exception])
    }

    "allow changing a memory cell after lock release" in {
      val state = initialState.withExclusiveLock(BinaryMixIndex(0), 100, 0).withoutLocks(0)
      state.updated(BinaryMixIndex(0), BinaryMixWord(0x0)) must not(throwA[Exception])
    }

    "allow changing a memory cell not covered by a lock" in {
      val state = initialState.withExclusiveLock(BinaryMixIndex(0), 100, 0)
      state.updated(BinaryMixIndex(100), BinaryMixWord(0x0)) must not(throwA[Exception])
    }

    "throw an exception if memory address is too big" in {
      val startAddress = BinaryMixIndex((BinaryMemoryState.MEMORY_SIZE - 99).toShort)
      initialState.withSharedLock(startAddress, 100, 0) must throwA[WrongMemoryAddressException]
      initialState.withExclusiveLock(startAddress, 100, 0) must throwA[WrongMemoryAddressException]
    }
  }
}
