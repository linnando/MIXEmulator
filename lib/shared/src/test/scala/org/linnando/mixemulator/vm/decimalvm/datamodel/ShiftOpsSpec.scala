package org.linnando.mixemulator.vm.decimalvm.datamodel

import org.linnando.mixemulator.vm.decimal._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class ShiftOpsSpec extends AsyncWordSpec with Matchers {
  "decimal word shift" should {
    "shift a positive word to the left" in {
      // + 1 2 3 4 5 -> + 2 3 4 5 0
      MixWord(102030405L) << MixIndex(1) mustEqual MixWord(203040500L)
    }

    "shift a negative word to the left" in {
      // - 1 2 3 4 5 -> - 2 3 4 5 0
      MixWord(0x400000000L | 102030405L) << MixIndex(1) mustEqual MixWord(0x400000000L | 203040500L)
    }

    "shift a positive word to the right" in {
      // + 1 2 3 4 5 -> + 0 1 2 3 4
      MixWord(102030405L) >> MixIndex(1) mustEqual MixWord(1020304L)
    }

    "shift a negative word to the right" in {
      // - 1 2 3 4 5 -> - 0 1 2 3 4
      MixWord(0x400000000L | 102030405L) >> MixIndex(1) mustEqual MixWord(0x400000000L | 1020304L)
    }
  }

  "decimal double word shift" should {
    "shift a positive double word to the left" in {
      // + 1 2 3 4 5 6 7 8 9 10 -> + 2 3 4 5 6 7 8 9 10 0
      MixDWord(102030405L, 607080910L) << MixIndex(1) mustEqual MixDWord(203040506L, 708091000L)
    }

    "shift a negative double word to the left" in {
      // - 1 2 3 4 5 6 7 8 9 10 -> - 2 3 4 5 6 7 8 9 10 0
      MixDWord(0x400000000L | 102030405L, 607080910L) << MixIndex(1) mustEqual MixDWord(0x400000000L | 203040506L, 708091000L)
    }

    "shift a positive double word to the right" in {
      // + 1 2 3 4 5 6 7 8 9 10 -> + 0 1 2 3 4 5 6 7 8 9
      MixDWord(102030405L, 607080910L) >> MixIndex(1) mustEqual MixDWord(1020304L, 506070809L)
    }

    "shift a negative double word to the right" in {
      // - 1 2 3 4 5 6 7 8 9 10 -> - 0 1 2 3 4 5 6 7 8 9
      MixDWord(0x400000000L | 102030405L, 607080910L) >> MixIndex(1) mustEqual MixDWord(0x400000000L | 1020304L, 506070809L)
    }
  }

  "decimal circular double word shift" should {
    "shift a positive double word to the left" in {
      // + 1 2 3 4 5 6 7 8 9 10 -> + 2 3 4 5 6 7 8 9 10 1
      MixDWord(102030405L, 607080910L) <<| MixIndex(1) mustEqual MixDWord(203040506L, 708091001L)
    }

    "shift a negative double word to the left" in {
      // - 1 2 3 4 5 6 7 8 9 10 -> - 2 3 4 5 6 7 8 9 10 1
      MixDWord(0x400000000L | 102030405L, 607080910L) <<| MixIndex(1) mustEqual MixDWord(0x400000000L | 203040506L, 708091001L)
    }

    "shift a positive double word to the right" in {
      // + 1 2 3 4 5 6 7 8 9 10 -> + 10 1 2 3 4 5 6 7 8 9
      MixDWord(102030405L, 607080910L) >>| MixIndex(1) mustEqual MixDWord(1001020304L, 506070809L)
    }

    "shift a negative double word to the right" in {
      // - 1 2 3 4 5 6 7 8 9 10 -> - 10 1 2 3 4 5 6 7 8 9
      MixDWord(0x400000000L | 102030405L, 607080910L) >>| MixIndex(1) mustEqual MixDWord(0x400000000L | 1001020304L, 506070809L)
    }
  }
}
