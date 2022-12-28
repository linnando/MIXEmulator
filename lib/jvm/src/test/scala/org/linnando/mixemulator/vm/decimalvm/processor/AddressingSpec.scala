package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.exceptions.{OverflowException, WrongIndexSpecException}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class AddressingSpec(implicit ee: ExecutionEnv) extends Specification {

  import decimal._

  private val initialState = decimal.initialState
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(MixWord(0x400000000L)) // - 0 0 0 0 0
      .updatedX(MixWord(1L)) // + 0 0 0 0 1
      .updatedI(1, MixIndex(0)) // + 0 0
      .updatedI(2, MixIndex(400)) // + 4 0
      .updatedI(3, MixIndex(800)) // + 8 0
      .updatedI(4, MixIndex(1200)) // + 12 0
      .updatedI(5, MixIndex(1600)) // + 16 0
      .updatedI(6, MixIndex(0x4000)) // - 0 0
  )

  "decimal indexing module" should {
    "perform no indexing if I = 0" in {
      // A = 1, I = 0
      getIndexedAddress(state, MixWord(1000000L)) must beEqualTo(MixIndex(1))
    }

    "perform indexing if 0 < I <= 6" in {
      // A = 1, I = 1
      getIndexedAddress(state, MixWord(1010000L)) must beEqualTo(MixIndex(1))
      // A = 1, I = 2
      getIndexedAddress(state, MixWord(1020000L)) must beEqualTo(MixIndex(401))
      // A = 1, I = 3
      getIndexedAddress(state, MixWord(1030000L)) must beEqualTo(MixIndex(801))
      // A = 1, I = 4
      getIndexedAddress(state, MixWord(1040000L)) must beEqualTo(MixIndex(1201))
      // A = 1, I = 5
      getIndexedAddress(state, MixWord(1050000L)) must beEqualTo(MixIndex(1601))
      // A = 1, I = 6
      getIndexedAddress(state, MixWord(1060000L)) must beEqualTo(MixIndex(1))
    }

    "throw an exception if I > 6" in {
      // A = 1, I = 7
      getIndexedAddress(state, MixWord(1070000L)) must throwA[WrongIndexSpecException]
      // A = 1, I = 99
      getIndexedAddress(state, MixWord(1990000L)) must throwA[WrongIndexSpecException]
    }

    "throw an exception if the indexed address is too big" in {
      // A = 8400, I = 5
      getIndexedAddress(state, MixWord(8400050000L)) must throwAn[OverflowException]
    }

    "not throw an exception if the indexed address fits in two bytes (even if it exceeds memory size)" in {
      // A = 8399, I = 5
      getIndexedAddress(state, MixWord(8399050000L)) must beEqualTo(MixIndex(9999))
    }
  }

  "decimal address loader" should {
    "load a positive number" in {
      // A = 1, I = 0, F = 2, C = 48 ENTA
      execute(state, MixWord(1000248L)).map(_.registers.getA) must beEqualTo(MixWord(1L)).await
      // A = 1, I = 0, F = 2, C = 49 ENT1
      execute(state, MixWord(1000249L)).map(_.registers.getI(1)) must beEqualTo(MixIndex(1)).await
      // A = 1, I = 0, F = 2, C = 50 ENT2
      execute(state, MixWord(1000250L)).map(_.registers.getI(2)) must beEqualTo(MixIndex(1)).await
      // A = 1, I = 0, F = 2, C = 51 ENT3
      execute(state, MixWord(1000251L)).map(_.registers.getI(3)) must beEqualTo(MixIndex(1)).await
      // A = 1, I = 0, F = 2, C = 52 ENT4
      execute(state, MixWord(1000252L)).map(_.registers.getI(4)) must beEqualTo(MixIndex(1)).await
      // A = 1, I = 0, F = 2, C = 53 ENT5
      execute(state, MixWord(1000253L)).map(_.registers.getI(5)) must beEqualTo(MixIndex(1)).await
      // A = 1, I = 0, F = 2, C = 54 ENT6
      execute(state, MixWord(1000254L)).map(_.registers.getI(6)) must beEqualTo(MixIndex(1)).await
      // A = 1, I = 0, F = 2, C = 55 ENTX
      execute(state, MixWord(1000255L)).map(_.registers.getX) must beEqualTo(MixWord(1L)).await
    }

    "load a negative number" in {
      // A = -1, I = 0, F = 2, C = 48 ENTA
      execute(state, MixWord(0x400000000L | 1000248L)).map(_.registers.getA) must beEqualTo(MixWord(0x400000001L)).await
      // A = -1, I = 0, F = 2, C = 49 ENT1
      execute(state, MixWord(0x400000000L | 1000249L)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x4001)).await
      // A = -1, I = 0, F = 2, C = 50 ENT2
      execute(state, MixWord(0x400000000L | 1000250L)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x4001)).await
      // A = -1, I = 0, F = 2, C = 51 ENT3
      execute(state, MixWord(0x400000000L | 1000251L)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x4001)).await
      // A = -1, I = 0, F = 2, C = 52 ENT4
      execute(state, MixWord(0x400000000L | 1000252L)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x4001)).await
      // A = -1, I = 0, F = 2, C = 53 ENT5
      execute(state, MixWord(0x400000000L | 1000253L)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x4001)).await
      // A = -1, I = 0, F = 2, C = 54 ENT6
      execute(state, MixWord(0x400000000L | 1000254L)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x4001)).await
      // A = -1, I = 0, F = 2, C = 55 ENTX
      execute(state, MixWord(0x400000000L | 1000255L)).map(_.registers.getX) must beEqualTo(MixWord(0x400000001L)).await
    }

    "load the negative zero" in {
      // A = -0, I = 0, F = 2, C = 48 ENTA
      execute(state, MixWord(0x400000000L | 248L)).map(_.registers.getA) must beEqualTo(MixWord(0x400000000L)).await
      // A = -0, I = 0, F = 2, C = 49 ENT1
      execute(state, MixWord(0x400000000L | 249L)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x4000)).await
      // A = -0, I = 0, F = 2, C = 50 ENT2
      execute(state, MixWord(0x400000000L | 250L)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x4000)).await
      // A = -0, I = 0, F = 2, C = 51 ENT3
      execute(state, MixWord(0x400000000L | 251L)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x4000)).await
      // A = -0, I = 0, F = 2, C = 52 ENT4
      execute(state, MixWord(0x400000000L | 252L)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x4000)).await
      // A = -0, I = 0, F = 2, C = 53 ENT5
      execute(state, MixWord(0x400000000L | 253L)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x4000)).await
      // A = -0, I = 0, F = 2, C = 54 ENT6
      execute(state, MixWord(0x400000000L | 254L)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x4000)).await
      // A = -0, I = 0, F = 2, C = 55 ENTX
      execute(state, MixWord(0x400000000L | 255L)).map(_.registers.getX) must beEqualTo(MixWord(0x400000000L)).await
    }

    "load an indexed address" in {
      // A = 3, I = 2, F = 2, C = 48 ENTA
      execute(state, MixWord(3020248L)).map(_.registers.getA) must beEqualTo(MixWord(403L)).await
      // A = 3, I = 2, F = 2, C = 49 ENT1
      execute(state, MixWord(3020249L)).map(_.registers.getI(1)) must beEqualTo(MixIndex(403)).await
      // A = 3, I = 2, F = 2, C = 50 ENT2
      execute(state, MixWord(3020250L)).map(_.registers.getI(2)) must beEqualTo(MixIndex(403)).await
      // A = 3, I = 2, F = 2, C = 51 ENT3
      execute(state, MixWord(3020251L)).map(_.registers.getI(3)) must beEqualTo(MixIndex(403)).await
      // A = 3, I = 2, F = 2, C = 52 ENT4
      execute(state, MixWord(3020252L)).map(_.registers.getI(4)) must beEqualTo(MixIndex(403)).await
      // A = 3, I = 2, F = 2, C = 53 ENT5
      execute(state, MixWord(3020253L)).map(_.registers.getI(5)) must beEqualTo(MixIndex(403)).await
      // A = 3, I = 2, F = 2, C = 54 ENT6
      execute(state, MixWord(3020254L)).map(_.registers.getI(6)) must beEqualTo(MixIndex(403)).await
      // A = 3, I = 2, F = 2, C = 55 ENTX
      execute(state, MixWord(3020255L)).map(_.registers.getX) must beEqualTo(MixWord(403L)).await
    }

    "load the contents of an index register" in {
      // A = 0, I = 2, F = 2, C = 48 ENTA
      execute(state, MixWord(20248L)).map(_.registers.getA) must beEqualTo(MixWord(400L)).await
      // A = 0, I = 2, F = 2, C = 49 ENT1
      execute(state, MixWord(20249L)).map(_.registers.getI(1)) must beEqualTo(MixIndex(400)).await
      // A = 0, I = 2, F = 2, C = 50 ENT2
      execute(state, MixWord(20250L)).map(_.registers.getI(2)) must beEqualTo(MixIndex(400)).await
      // A = 0, I = 2, F = 2, C = 51 ENT3
      execute(state, MixWord(20251L)).map(_.registers.getI(3)) must beEqualTo(MixIndex(400)).await
      // A = 0, I = 2, F = 2, C = 52 ENT4
      execute(state, MixWord(20252L)).map(_.registers.getI(4)) must beEqualTo(MixIndex(400)).await
      // A = 0, I = 2, F = 2, C = 53 ENT5
      execute(state, MixWord(20253L)).map(_.registers.getI(5)) must beEqualTo(MixIndex(400)).await
      // A = 0, I = 2, F = 2, C = 54 ENT6
      execute(state, MixWord(20254L)).map(_.registers.getI(6)) must beEqualTo(MixIndex(400)).await
      // A = 0, I = 2, F = 2, C = 55 ENTX
      execute(state, MixWord(20255L)).map(_.registers.getX) must beEqualTo(MixWord(400L)).await
    }

    "preserve command sign if the indexed address is zero" in {
      // A = -0, I = 1, F = 2, C = 48 ENTA
      execute(state, MixWord(0x400000000L | 10248L)).map(_.registers.getA) must beEqualTo(MixWord(0x400000000L)).await
      // A = -0, I = 1, F = 2, C = 49 ENT1
      execute(state, MixWord(0x400000000L | 10249L)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x4000)).await
      // A = -0, I = 1, F = 2, C = 50 ENT2
      execute(state, MixWord(0x400000000L | 10250L)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x4000)).await
      // A = -0, I = 1, F = 2, C = 51 ENT3
      execute(state, MixWord(0x400000000L | 10251L)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x4000)).await
      // A = -0, I = 1, F = 2, C = 52 ENT4
      execute(state, MixWord(0x400000000L | 10252L)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x4000)).await
      // A = -0, I = 1, F = 2, C = 53 ENT5
      execute(state, MixWord(0x400000000L | 10253L)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x4000)).await
      // A = -0, I = 1, F = 2, C = 54 ENT6
      execute(state, MixWord(0x400000000L | 10254L)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x4000)).await
      // A = -0, I = 1, F = 2, C = 55 ENTX
      execute(state, MixWord(0x400000000L | 10255L)).map(_.registers.getX) must beEqualTo(MixWord(0x400000000L)).await
    }
  }

  "decimal address loader with negation" should {
    "load the negation of a positive number" in {
      // A = 1, I = 0, F = 3, C = 48 ENNA
      execute(state, MixWord(1000348L)).map(_.registers.getA) must beEqualTo(MixWord(0x400000001L)).await
      // A = 1, I = 0, F = 3, C = 49 ENN1
      execute(state, MixWord(1000349L)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x4001)).await
      // A = 1, I = 0, F = 3, C = 50 ENN2
      execute(state, MixWord(1000350L)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x4001)).await
      // A = 1, I = 0, F = 3, C = 51 ENN3
      execute(state, MixWord(1000351L)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x4001)).await
      // A = 1, I = 0, F = 3, C = 52 ENN4
      execute(state, MixWord(1000352L)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x4001)).await
      // A = 1, I = 0, F = 3, C = 53 ENN5
      execute(state, MixWord(1000353L)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x4001)).await
      // A = 1, I = 0, F = 3, C = 54 ENN6
      execute(state, MixWord(1000354L)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x4001)).await
      // A = 1, I = 0, F = 3, C = 55 ENNX
      execute(state, MixWord(1000355L)).map(_.registers.getX) must beEqualTo(MixWord(0x400000001L)).await
    }

    "load the negation of a negative number (positive)" in {
      // A = -1, I = 0, F = 3, C = 48 ENNA
      execute(state, MixWord(0x400000000L | 1000348L)).map(_.registers.getA) must beEqualTo(MixWord(1L)).await
      // A = -1, I = 0, F = 3, C = 49 ENN1
      execute(state, MixWord(0x400000000L | 1000349L)).map(_.registers.getI(1)) must beEqualTo(MixIndex(1)).await
      // A = -1, I = 0, F = 3, C = 50 ENN2
      execute(state, MixWord(0x400000000L | 1000350L)).map(_.registers.getI(2)) must beEqualTo(MixIndex(1)).await
      // A = -1, I = 0, F = 3, C = 51 ENN3
      execute(state, MixWord(0x400000000L | 1000351L)).map(_.registers.getI(3)) must beEqualTo(MixIndex(1)).await
      // A = -1, I = 0, F = 3, C = 52 ENN4
      execute(state, MixWord(0x400000000L | 1000352L)).map(_.registers.getI(4)) must beEqualTo(MixIndex(1)).await
      // A = -1, I = 0, F = 3, C = 53 ENN5
      execute(state, MixWord(0x400000000L | 1000353L)).map(_.registers.getI(5)) must beEqualTo(MixIndex(1)).await
      // A = -1, I = 0, F = 3, C = 54 ENN6
      execute(state, MixWord(0x400000000L | 1000354L)).map(_.registers.getI(6)) must beEqualTo(MixIndex(1)).await
      // A = -1, I = 0, F = 3, C = 55 ENNX
      execute(state, MixWord(0x400000000L | 1000355L)).map(_.registers.getX) must beEqualTo(MixWord(1L)).await
    }

    "load the negation of the positive zero" in {
      // A = 0, I = 0, F = 3, C = 48 ENNA
      execute(state, MixWord(348L)).map(_.registers.getA) must beEqualTo(MixWord(0x400000000L)).await
      // A = 0, I = 0, F = 3, C = 49 ENN1
      execute(state, MixWord(349L)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x4000)).await
      // A = 0, I = 0, F = 3, C = 50 ENN2
      execute(state, MixWord(350L)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x4000)).await
      // A = 0, I = 0, F = 3, C = 51 ENN3
      execute(state, MixWord(351L)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x4000)).await
      // A = 0, I = 0, F = 3, C = 52 ENN4
      execute(state, MixWord(352L)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x4000)).await
      // A = 0, I = 0, F = 3, C = 53 ENN5
      execute(state, MixWord(353L)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x4000)).await
      // A = 0, I = 0, F = 3, C = 54 ENN6
      execute(state, MixWord(354L)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x4000)).await
      // A = 0, I = 0, F = 3, C = 55 ENNX
      execute(state, MixWord(355L)).map(_.registers.getX) must beEqualTo(MixWord(0x400000000L)).await
    }

    "load the negation of an indexed address" in {
      // A = 3, I = 2, F = 3, C = 48 ENNA
      execute(state, MixWord(3020348L)).map(_.registers.getA) must beEqualTo(MixWord(0x400000000L | 403L)).await
      // A = 3, I = 2, F = 3, C = 49 ENN1
      execute(state, MixWord(3020349L)).map(_.registers.getI(1)) must beEqualTo(MixIndex((0x4000 | 403).toShort)).await
      // A = 3, I = 2, F = 3, C = 50 ENN2
      execute(state, MixWord(3020350L)).map(_.registers.getI(2)) must beEqualTo(MixIndex((0x4000 | 403).toShort)).await
      // A = 3, I = 2, F = 3, C = 51 ENN3
      execute(state, MixWord(3020351L)).map(_.registers.getI(3)) must beEqualTo(MixIndex((0x4000 | 403).toShort)).await
      // A = 3, I = 2, F = 3, C = 52 ENN4
      execute(state, MixWord(3020352L)).map(_.registers.getI(4)) must beEqualTo(MixIndex((0x4000 | 403).toShort)).await
      // A = 3, I = 2, F = 3, C = 53 ENN5
      execute(state, MixWord(3020353L)).map(_.registers.getI(5)) must beEqualTo(MixIndex((0x4000 | 403).toShort)).await
      // A = 3, I = 2, F = 3, C = 54 ENN6
      execute(state, MixWord(3020354L)).map(_.registers.getI(6)) must beEqualTo(MixIndex((0x4000 | 403).toShort)).await
      // A = 3, I = 2, F = 3, C = 55 ENNX
      execute(state, MixWord(3020355L)).map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 403L)).await
    }

    "load the negated contents of an indexed register" in {
      // A = 0, I = 2, F = 3, C = 48 ENNA
      execute(state, MixWord(20348L)).map(_.registers.getA) must beEqualTo(MixWord(0x400000000L | 400L)).await
      // A = 0, I = 2, F = 3, C = 49 ENN1
      execute(state, MixWord(20349L)).map(_.registers.getI(1)) must beEqualTo(MixIndex((0x4000 | 400).toShort)).await
      // A = 0, I = 2, F = 3, C = 50 ENN2
      execute(state, MixWord(20350L)).map(_.registers.getI(2)) must beEqualTo(MixIndex((0x4000 | 400).toShort)).await
      // A = 0, I = 2, F = 3, C = 51 ENN3
      execute(state, MixWord(20351L)).map(_.registers.getI(3)) must beEqualTo(MixIndex((0x4000 | 400).toShort)).await
      // A = 0, I = 2, F = 3, C = 52 ENN4
      execute(state, MixWord(20352L)).map(_.registers.getI(4)) must beEqualTo(MixIndex((0x4000 | 400).toShort)).await
      // A = 0, I = 2, F = 3, C = 53 ENN5
      execute(state, MixWord(20353L)).map(_.registers.getI(5)) must beEqualTo(MixIndex((0x4000 | 400).toShort)).await
      // A = 0, I = 2, F = 3, C = 54 ENN6
      execute(state, MixWord(20354L)).map(_.registers.getI(6)) must beEqualTo(MixIndex((0x4000 | 400).toShort)).await
      // A = 0, I = 2, F = 3, C = 55 ENNX
      execute(state, MixWord(20355L)).map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 400L)).await
    }

    "invert the command sign if the indexed address is zero" in {
      // A = -0, I = 1, F = 3, C = 48 ENNA
      execute(state, MixWord(0x400000000L | 10348L)).map(_.registers.getA) must beEqualTo(MixWord(0L)).await
      // A = -0, I = 1, F = 3, C = 49 ENN1
      execute(state, MixWord(0x400000000L | 10349L)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0)).await
      // A = -0, I = 1, F = 3, C = 50 ENN2
      execute(state, MixWord(0x400000000L | 10350L)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0)).await
      // A = -0, I = 1, F = 3, C = 51 ENN3
      execute(state, MixWord(0x400000000L | 10351L)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0)).await
      // A = -0, I = 1, F = 3, C = 52 ENN4
      execute(state, MixWord(0x400000000L | 10352L)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0)).await
      // A = -0, I = 1, F = 3, C = 53 ENN5
      execute(state, MixWord(0x400000000L | 10353L)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0)).await
      // A = -0, I = 1, F = 3, C = 54 ENN6
      execute(state, MixWord(0x400000000L | 10354L)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0)).await
      // A = -0, I = 1, F = 3, C = 55 ENNX
      execute(state, MixWord(0x400000000L | 10355L)).map(_.registers.getX) must beEqualTo(MixWord(0L)).await
    }
  }
}
