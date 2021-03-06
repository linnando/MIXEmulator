package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.binary
import org.linnando.mixemulator.vm.exceptions.{OverflowException, WrongIndexSpecException}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class AddressingSpec(implicit ee: ExecutionEnv) extends Specification {
  import binary._
  private val initialState = binary.initialState
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(MixWord(0x40000000)) // - 0 0 0 0 0
      .updatedX(MixWord(0x00000001)) // + 0 0 0 0 1
      .updatedI(1, MixIndex(0x0000)) // + 0 0
      .updatedI(2, MixIndex(0x0100)) // + 4 0
      .updatedI(3, MixIndex(0x0200)) // + 8 0
      .updatedI(4, MixIndex(0x0300)) // + 12 0
      .updatedI(5, MixIndex(0x0400)) // + 16 0
      .updatedI(6, MixIndex(0x1000)) // - 0 0
  )

  "binary indexing module" should {
    "perform no indexing if I = 0" in {
      // A = 0x1, I = 0
      getIndexedAddress(state, MixWord(0x40000)) must beEqualTo(MixIndex(0x1))
    }

    "perform indexing if 0 < I <= 6" in {
      // A = 0x1, I = 1
      getIndexedAddress(state, MixWord(0x41000)) must beEqualTo(MixIndex(0x0001))
      // A = 0x1, I = 2
      getIndexedAddress(state, MixWord(0x42000)) must beEqualTo(MixIndex(0x0101))
      // A = 0x1, I = 3
      getIndexedAddress(state, MixWord(0x43000)) must beEqualTo(MixIndex(0x0201))
      // A = 0x1, I = 4
      getIndexedAddress(state, MixWord(0x44000)) must beEqualTo(MixIndex(0x0301))
      // A = 0x1, I = 5
      getIndexedAddress(state, MixWord(0x45000)) must beEqualTo(MixIndex(0x0401))
      // A = 0x1, I = 6
      getIndexedAddress(state, MixWord(0x46000)) must beEqualTo(MixIndex(0x0001))
    }

    "throw an exception if I > 6" in {
      // A = 0x1, I = 7
      getIndexedAddress(state, MixWord(0x47000)) must throwA[WrongIndexSpecException]
      // A = 0x1, I = 0x3f
      getIndexedAddress(state, MixWord(0x7f000)) must throwA[WrongIndexSpecException]
    }

    "throw an exception if the indexed address is too big" in {
      // A = 0xc00, I = 5
      getIndexedAddress(state, MixWord(0x30005000)) must throwAn[OverflowException]
    }

    "not throw an exception if the indexed address fits in two bytes (even if it exceeds memory size)" in {
      // A = 0xbff, I = 5
      getIndexedAddress(state, MixWord(0x2ffc5000)) must beEqualTo(MixIndex(0xfff))
    }
  }

  "binary address loader" should {
    "load a positive number" in {
      // A = 1, I = 0, F = 2, C = 48 ENTA
      execute(state, MixWord(0x000400b0)).map(_.registers.getA) must beEqualTo(MixWord(0x00000001)).await
      // A = 1, I = 0, F = 2, C = 49 ENT1
      execute(state, MixWord(0x000400b1)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x0001)).await
      // A = 1, I = 0, F = 2, C = 50 ENT2
      execute(state, MixWord(0x000400b2)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x0001)).await
      // A = 1, I = 0, F = 2, C = 51 ENT3
      execute(state, MixWord(0x000400b3)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x0001)).await
      // A = 1, I = 0, F = 2, C = 52 ENT4
      execute(state, MixWord(0x000400b4)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x0001)).await
      // A = 1, I = 0, F = 2, C = 53 ENT5
      execute(state, MixWord(0x000400b5)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x0001)).await
      // A = 1, I = 0, F = 2, C = 54 ENT6
      execute(state, MixWord(0x000400b6)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x0001)).await
      // A = 1, I = 0, F = 2, C = 55 ENTX
      execute(state, MixWord(0x000400b7)).map(_.registers.getX) must beEqualTo(MixWord(0x00000001)).await
    }

    "load a negative number" in {
      // A = -1, I = 0, F = 2, C = 48 ENTA
      execute(state, MixWord(0x400400b0)).map(_.registers.getA) must beEqualTo(MixWord(0x40000001)).await
      // A = -1, I = 0, F = 2, C = 49 ENT1
      execute(state, MixWord(0x400400b1)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x1001)).await
      // A = -1, I = 0, F = 2, C = 50 ENT2
      execute(state, MixWord(0x400400b2)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x1001)).await
      // A = -1, I = 0, F = 2, C = 51 ENT3
      execute(state, MixWord(0x400400b3)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x1001)).await
      // A = -1, I = 0, F = 2, C = 52 ENT4
      execute(state, MixWord(0x400400b4)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x1001)).await
      // A = -1, I = 0, F = 2, C = 53 ENT5
      execute(state, MixWord(0x400400b5)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x1001)).await
      // A = -1, I = 0, F = 2, C = 54 ENT6
      execute(state, MixWord(0x400400b6)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x1001)).await
      // A = -1, I = 0, F = 2, C = 55 ENTX
      execute(state, MixWord(0x400400b7)).map(_.registers.getX) must beEqualTo(MixWord(0x40000001)).await
    }

    "load the negative zero" in {
      // A = -0, I = 0, F = 2, C = 48 ENTA
      execute(state, MixWord(0x400000b0)).map(_.registers.getA) must beEqualTo(MixWord(0x40000000)).await
      // A = -0, I = 0, F = 2, C = 49 ENT1
      execute(state, MixWord(0x400000b1)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x1000)).await
      // A = -0, I = 0, F = 2, C = 50 ENT2
      execute(state, MixWord(0x400000b2)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x1000)).await
      // A = -0, I = 0, F = 2, C = 51 ENT3
      execute(state, MixWord(0x400000b3)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x1000)).await
      // A = -0, I = 0, F = 2, C = 52 ENT4
      execute(state, MixWord(0x400000b4)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x1000)).await
      // A = -0, I = 0, F = 2, C = 53 ENT5
      execute(state, MixWord(0x400000b5)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x1000)).await
      // A = -0, I = 0, F = 2, C = 54 ENT6
      execute(state, MixWord(0x400000b6)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x1000)).await
      // A = -0, I = 0, F = 2, C = 55 ENTX
      execute(state, MixWord(0x400000b7)).map(_.registers.getX) must beEqualTo(MixWord(0x40000000)).await
    }

    "load an indexed address" in {
      // A = 3, I = 2, F = 2, C = 48 ENTA
      execute(state, MixWord(0x000c20b0)).map(_.registers.getA) must beEqualTo(MixWord(0x00000103)).await
      // A = 3, I = 2, F = 2, C = 49 ENT1
      execute(state, MixWord(0x000c20b1)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x0103)).await
      // A = 3, I = 2, F = 2, C = 50 ENT2
      execute(state, MixWord(0x000c20b2)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x0103)).await
      // A = 3, I = 2, F = 2, C = 51 ENT3
      execute(state, MixWord(0x000c20b3)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x0103)).await
      // A = 3, I = 2, F = 2, C = 52 ENT4
      execute(state, MixWord(0x000c20b4)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x0103)).await
      // A = 3, I = 2, F = 2, C = 53 ENT5
      execute(state, MixWord(0x000c20b5)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x0103)).await
      // A = 3, I = 2, F = 2, C = 54 ENT6
      execute(state, MixWord(0x000c20b6)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x0103)).await
      // A = 3, I = 2, F = 2, C = 55 ENTX
      execute(state, MixWord(0x000c20b7)).map(_.registers.getX) must beEqualTo(MixWord(0x00000103)).await
    }

    "load the contents of an index register" in {
      // A = 0, I = 2, F = 2, C = 48 ENTA
      execute(state, MixWord(0x000020b0)).map(_.registers.getA) must beEqualTo(MixWord(0x00000100)).await
      // A = 0, I = 2, F = 2, C = 49 ENT1
      execute(state, MixWord(0x000020b1)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x0100)).await
      // A = 0, I = 2, F = 2, C = 50 ENT2
      execute(state, MixWord(0x000020b2)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x0100)).await
      // A = 0, I = 2, F = 2, C = 51 ENT3
      execute(state, MixWord(0x000020b3)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x0100)).await
      // A = 0, I = 2, F = 2, C = 52 ENT4
      execute(state, MixWord(0x000020b4)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x0100)).await
      // A = 0, I = 2, F = 2, C = 53 ENT5
      execute(state, MixWord(0x000020b5)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x0100)).await
      // A = 0, I = 2, F = 2, C = 54 ENT6
      execute(state, MixWord(0x000020b6)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x0100)).await
      // A = 0, I = 2, F = 2, C = 55 ENTX
      execute(state, MixWord(0x000020b7)).map(_.registers.getX) must beEqualTo(MixWord(0x00000100)).await
    }

    "preserve command sign if the indexed address is zero" in {
      // A = -0, I = 1, F = 2, C = 48 ENTA
      execute(state, MixWord(0x400010b0)).map(_.registers.getA) must beEqualTo(MixWord(0x40000000)).await
      // A = -0, I = 1, F = 2, C = 49 ENT1
      execute(state, MixWord(0x400010b1)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x1000)).await
      // A = -0, I = 1, F = 2, C = 50 ENT2
      execute(state, MixWord(0x400010b2)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x1000)).await
      // A = -0, I = 1, F = 2, C = 51 ENT3
      execute(state, MixWord(0x400010b3)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x1000)).await
      // A = -0, I = 1, F = 2, C = 52 ENT4
      execute(state, MixWord(0x400010b4)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x1000)).await
      // A = -0, I = 1, F = 2, C = 53 ENT5
      execute(state, MixWord(0x400010b5)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x1000)).await
      // A = -0, I = 1, F = 2, C = 54 ENT6
      execute(state, MixWord(0x400010b6)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x1000)).await
      // A = -0, I = 1, F = 2, C = 55 ENTX
      execute(state, MixWord(0x400010b7)).map(_.registers.getX) must beEqualTo(MixWord(0x40000000)).await
    }
  }

  "binary address loader with negation" should {
    "load the negation of a positive number" in {
      // A = 1, I = 0, F = 3, C = 48 ENNA
      execute(state, MixWord(0x000400f0)).map(_.registers.getA) must beEqualTo(MixWord(0x40000001)).await
      // A = 1, I = 0, F = 3, C = 49 ENN1
      execute(state, MixWord(0x000400f1)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x1001)).await
      // A = 1, I = 0, F = 3, C = 50 ENN2
      execute(state, MixWord(0x000400f2)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x1001)).await
      // A = 1, I = 0, F = 3, C = 51 ENN3
      execute(state, MixWord(0x000400f3)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x1001)).await
      // A = 1, I = 0, F = 3, C = 52 ENN4
      execute(state, MixWord(0x000400f4)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x1001)).await
      // A = 1, I = 0, F = 3, C = 53 ENN5
      execute(state, MixWord(0x000400f5)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x1001)).await
      // A = 1, I = 0, F = 3, C = 54 ENN6
      execute(state, MixWord(0x000400f6)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x1001)).await
      // A = 1, I = 0, F = 3, C = 55 ENNX
      execute(state, MixWord(0x000400f7)).map(_.registers.getX) must beEqualTo(MixWord(0x40000001)).await
    }

    "load the negation of a negative number (positive)" in {
      // A = -1, I = 0, F = 3, C = 48 ENNA
      execute(state, MixWord(0x400400f0)).map(_.registers.getA) must beEqualTo(MixWord(0x00000001)).await
      // A = -1, I = 0, F = 3, C = 49 ENN1
      execute(state, MixWord(0x400400f1)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x0001)).await
      // A = -1, I = 0, F = 3, C = 50 ENN2
      execute(state, MixWord(0x400400f2)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x0001)).await
      // A = -1, I = 0, F = 3, C = 51 ENN3
      execute(state, MixWord(0x400400f3)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x0001)).await
      // A = -1, I = 0, F = 3, C = 52 ENN4
      execute(state, MixWord(0x400400f4)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x0001)).await
      // A = -1, I = 0, F = 3, C = 53 ENN5
      execute(state, MixWord(0x400400f5)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x0001)).await
      // A = -1, I = 0, F = 3, C = 54 ENN6
      execute(state, MixWord(0x400400f6)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x0001)).await
      // A = -1, I = 0, F = 3, C = 55 ENNX
      execute(state, MixWord(0x400400f7)).map(_.registers.getX) must beEqualTo(MixWord(0x00000001)).await
    }

    "load the negation of the positive zero" in {
      // A = 0, I = 0, F = 3, C = 48 ENNA
      execute(state, MixWord(0x000000f0)).map(_.registers.getA) must beEqualTo(MixWord(0x40000000)).await
      // A = 0, I = 0, F = 3, C = 49 ENN1
      execute(state, MixWord(0x000000f1)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x1000)).await
      // A = 0, I = 0, F = 3, C = 50 ENN2
      execute(state, MixWord(0x000000f2)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x1000)).await
      // A = 0, I = 0, F = 3, C = 51 ENN3
      execute(state, MixWord(0x000000f3)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x1000)).await
      // A = 0, I = 0, F = 3, C = 52 ENN4
      execute(state, MixWord(0x000000f4)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x1000)).await
      // A = 0, I = 0, F = 3, C = 53 ENN5
      execute(state, MixWord(0x000000f5)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x1000)).await
      // A = 0, I = 0, F = 3, C = 54 ENN6
      execute(state, MixWord(0x000000f6)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x1000)).await
      // A = 0, I = 0, F = 3, C = 55 ENNX
      execute(state, MixWord(0x000000f7)).map(_.registers.getX) must beEqualTo(MixWord(0x40000000)).await
    }

    "load the negation of an indexed address" in {
      // A = 3, I = 2, F = 3, C = 48 ENNA
      execute(state, MixWord(0x000c20f0)).map(_.registers.getA) must beEqualTo(MixWord(0x40000103)).await
      // A = 3, I = 2, F = 3, C = 49 ENN1
      execute(state, MixWord(0x000c20f1)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x1103)).await
      // A = 3, I = 2, F = 3, C = 50 ENN2
      execute(state, MixWord(0x000c20f2)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x1103)).await
      // A = 3, I = 2, F = 3, C = 51 ENN3
      execute(state, MixWord(0x000c20f3)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x1103)).await
      // A = 3, I = 2, F = 3, C = 52 ENN4
      execute(state, MixWord(0x000c20f4)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x1103)).await
      // A = 3, I = 2, F = 3, C = 53 ENN5
      execute(state, MixWord(0x000c20f5)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x1103)).await
      // A = 3, I = 2, F = 3, C = 54 ENN6
      execute(state, MixWord(0x000c20f6)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x1103)).await
      // A = 3, I = 2, F = 3, C = 55 ENNX
      execute(state, MixWord(0x000c20f7)).map(_.registers.getX) must beEqualTo(MixWord(0x40000103)).await
    }

    "load the negated contents of an indexed register" in {
      // A = 0, I = 2, F = 3, C = 48 ENNA
      execute(state, MixWord(0x000020f0)).map(_.registers.getA) must beEqualTo(MixWord(0x40000100)).await
      // A = 0, I = 2, F = 3, C = 49 ENN1
      execute(state, MixWord(0x000020f1)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x1100)).await
      // A = 0, I = 2, F = 3, C = 50 ENN2
      execute(state, MixWord(0x000020f2)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x1100)).await
      // A = 0, I = 2, F = 3, C = 51 ENN3
      execute(state, MixWord(0x000020f3)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x1100)).await
      // A = 0, I = 2, F = 3, C = 52 ENN4
      execute(state, MixWord(0x000020f4)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x1100)).await
      // A = 0, I = 2, F = 3, C = 53 ENN5
      execute(state, MixWord(0x000020f5)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x1100)).await
      // A = 0, I = 2, F = 3, C = 54 ENN6
      execute(state, MixWord(0x000020f6)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x1100)).await
      // A = 0, I = 2, F = 3, C = 55 ENNX
      execute(state, MixWord(0x000020f7)).map(_.registers.getX) must beEqualTo(MixWord(0x40000100)).await
    }

    "invert the command sign if the indexed address is zero" in {
      // A = -0, I = 1, F = 3, C = 48 ENNA
      execute(state, MixWord(0x400010f0)).map(_.registers.getA) must beEqualTo(MixWord(0x00000000)).await
      // A = -0, I = 1, F = 3, C = 49 ENN1
      execute(state, MixWord(0x400010f1)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x0000)).await
      // A = -0, I = 1, F = 3, C = 50 ENN2
      execute(state, MixWord(0x400010f2)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x0000)).await
      // A = -0, I = 1, F = 3, C = 51 ENN3
      execute(state, MixWord(0x400010f3)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x0000)).await
      // A = -0, I = 1, F = 3, C = 52 ENN4
      execute(state, MixWord(0x400010f4)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x0000)).await
      // A = -0, I = 1, F = 3, C = 53 ENN5
      execute(state, MixWord(0x400010f5)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x0000)).await
      // A = -0, I = 1, F = 3, C = 54 ENN6
      execute(state, MixWord(0x400010f6)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x0000)).await
      // A = -0, I = 1, F = 3, C = 55 ENNX
      execute(state, MixWord(0x400010f7)).map(_.registers.getX) must beEqualTo(MixWord(0x00000000)).await
    }
  }
}
