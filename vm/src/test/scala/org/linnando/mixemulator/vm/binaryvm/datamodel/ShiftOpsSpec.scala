package org.linnando.mixemulator.vm.binaryvm.datamodel

import org.linnando.mixemulator.vm.binary._
import org.specs2.mutable.Specification

class ShiftOpsSpec extends Specification {
  "binary word shift" should {
    "shift a positive word to the left" in {
      // + 1 2 3 4 5 -> + 2 3 4 5 0
      MixWord(0x01083105) << MixIndex(1) must be equalTo MixWord(0x020c4140)
    }

    "shift a negative word to the left" in {
      // - 1 2 3 4 5 -> - 2 3 4 5 0
      MixWord(0x41083105) << MixIndex(1) must be equalTo MixWord(0x420c4140)
    }

    "shift a positive word to the right" in {
      // + 1 2 3 4 5 -> + 0 1 2 3 4
      MixWord(0x01083105) >> MixIndex(1) must be equalTo MixWord(0x000420c4)
    }

    "shift a negative word to the right" in {
      // - 1 2 3 4 5 -> - 0 1 2 3 4
      MixWord(0x41083105) >> MixIndex(1) must be equalTo MixWord(0x400420c4)
    }
  }

  "binary double word shift" should {
    "shift a positive double word to the left" in {
      // + 1 2 3 4 5 6 7 8 9 10 -> + 2 3 4 5 6 7 8 9 10 0
      MixDWord(0x00420c41461c824aL) << MixIndex(1) must be equalTo MixDWord(0x0083105187209280L)
    }

    "shift a negative double word to the left" in {
      // - 1 2 3 4 5 6 7 8 9 10 -> - 2 3 4 5 6 7 8 9 10 0
      MixDWord(0x10420c41461c824aL) << MixIndex(1) must be equalTo MixDWord(0x1083105187209280L)
    }

    "shift a positive double word to the right" in {
      // + 1 2 3 4 5 6 7 8 9 10 -> + 0 1 2 3 4 5 6 7 8 9
      MixDWord(0x00420c41461c824aL) >> MixIndex(1) must be equalTo MixDWord(0x0001083105187209L)
    }

    "shift a negative double word to the right" in {
      // - 1 2 3 4 5 6 7 8 9 10 -> - 0 1 2 3 4 5 6 7 8 9
      MixDWord(0x10420c41461c824aL) >> MixIndex(1) must be equalTo MixDWord(0x1001083105187209L)
    }
  }

  "binary circular double word shift" should {
    "shift a positive double word to the left" in {
      // + 1 2 3 4 5 6 7 8 9 10 -> + 2 3 4 5 6 7 8 9 10 1
      MixDWord(0x00420c41461c824aL) <<| MixIndex(1) must be equalTo MixDWord(0x0083105187209281L)
    }

    "shift a negative double word to the left" in {
      // - 1 2 3 4 5 6 7 8 9 10 -> - 2 3 4 5 6 7 8 9 10 1
      MixDWord(0x10420c41461c824aL) <<| MixIndex(1) must be equalTo MixDWord(0x1083105187209281L)
    }

    "shift a positive double word to the right" in {
      // + 1 2 3 4 5 6 7 8 9 10 -> + 10 1 2 3 4 5 6 7 8 9
      MixDWord(0x00420c41461c824aL) >>| MixIndex(1) must be equalTo MixDWord(0x0281083105187209L)
    }

    "shift a negative double word to the right" in {
      // - 1 2 3 4 5 6 7 8 9 10 -> - 10 1 2 3 4 5 6 7 8 9
      MixDWord(0x10420c41461c824aL) >>| MixIndex(1) must be equalTo MixDWord(0x1281083105187209L)
    }
  }
}
