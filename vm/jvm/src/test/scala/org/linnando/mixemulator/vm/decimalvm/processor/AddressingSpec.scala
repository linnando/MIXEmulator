package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.exceptions.{OverflowException, WrongIndexSpecException}
import org.specs2.mutable.Specification

class AddressingSpec extends Specification {
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
      getIndexedAddress(state, MixWord(1000000L)) must be equalTo MixIndex(1)
    }

    "perform indexing if 0 < I <= 6" in {
      // A = 1, I = 1
      getIndexedAddress(state, MixWord(1010000L)) must be equalTo MixIndex(1)
      // A = 1, I = 2
      getIndexedAddress(state, MixWord(1020000L)) must be equalTo MixIndex(401)
      // A = 1, I = 3
      getIndexedAddress(state, MixWord(1030000L)) must be equalTo MixIndex(801)
      // A = 1, I = 4
      getIndexedAddress(state, MixWord(1040000L)) must be equalTo MixIndex(1201)
      // A = 1, I = 5
      getIndexedAddress(state, MixWord(1050000L)) must be equalTo MixIndex(1601)
      // A = 1, I = 6
      getIndexedAddress(state, MixWord(1060000L)) must be equalTo MixIndex(1)
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
      getIndexedAddress(state, MixWord(8399050000L)) must be equalTo MixIndex(9999)
    }
  }

  "decimal address loader" should {
    "load a positive number" in {
      // A = 1, I = 0, F = 2, C = 48 ENTA
      execute(state, MixWord(1000248L)).registers.getA must be equalTo MixWord(1L)
      // A = 1, I = 0, F = 2, C = 49 ENT1
      execute(state, MixWord(1000249L)).registers.getI(1) must be equalTo MixIndex(1)
      // A = 1, I = 0, F = 2, C = 50 ENT2
      execute(state, MixWord(1000250L)).registers.getI(2) must be equalTo MixIndex(1)
      // A = 1, I = 0, F = 2, C = 51 ENT3
      execute(state, MixWord(1000251L)).registers.getI(3) must be equalTo MixIndex(1)
      // A = 1, I = 0, F = 2, C = 52 ENT4
      execute(state, MixWord(1000252L)).registers.getI(4) must be equalTo MixIndex(1)
      // A = 1, I = 0, F = 2, C = 53 ENT5
      execute(state, MixWord(1000253L)).registers.getI(5) must be equalTo MixIndex(1)
      // A = 1, I = 0, F = 2, C = 54 ENT6
      execute(state, MixWord(1000254L)).registers.getI(6) must be equalTo MixIndex(1)
      // A = 1, I = 0, F = 2, C = 55 ENTX
      execute(state, MixWord(1000255L)).registers.getX must be equalTo MixWord(1L)
    }

    "load a negative number" in {
      // A = -1, I = 0, F = 2, C = 48 ENTA
      execute(state, MixWord(0x400000000L | 1000248L)).registers.getA must be equalTo MixWord(0x400000001L)
      // A = -1, I = 0, F = 2, C = 49 ENT1
      execute(state, MixWord(0x400000000L | 1000249L)).registers.getI(1) must be equalTo MixIndex(0x4001)
      // A = -1, I = 0, F = 2, C = 50 ENT2
      execute(state, MixWord(0x400000000L | 1000250L)).registers.getI(2) must be equalTo MixIndex(0x4001)
      // A = -1, I = 0, F = 2, C = 51 ENT3
      execute(state, MixWord(0x400000000L | 1000251L)).registers.getI(3) must be equalTo MixIndex(0x4001)
      // A = -1, I = 0, F = 2, C = 52 ENT4
      execute(state, MixWord(0x400000000L | 1000252L)).registers.getI(4) must be equalTo MixIndex(0x4001)
      // A = -1, I = 0, F = 2, C = 53 ENT5
      execute(state, MixWord(0x400000000L | 1000253L)).registers.getI(5) must be equalTo MixIndex(0x4001)
      // A = -1, I = 0, F = 2, C = 54 ENT6
      execute(state, MixWord(0x400000000L | 1000254L)).registers.getI(6) must be equalTo MixIndex(0x4001)
      // A = -1, I = 0, F = 2, C = 55 ENTX
      execute(state, MixWord(0x400000000L | 1000255L)).registers.getX must be equalTo MixWord(0x400000001L)
    }

    "load the negative zero" in {
      // A = -0, I = 0, F = 2, C = 48 ENTA
      execute(state, MixWord(0x400000000L | 248L)).registers.getA must be equalTo MixWord(0x400000000L)
      // A = -0, I = 0, F = 2, C = 49 ENT1
      execute(state, MixWord(0x400000000L | 249L)).registers.getI(1) must be equalTo MixIndex(0x4000)
      // A = -0, I = 0, F = 2, C = 50 ENT2
      execute(state, MixWord(0x400000000L | 250L)).registers.getI(2) must be equalTo MixIndex(0x4000)
      // A = -0, I = 0, F = 2, C = 51 ENT3
      execute(state, MixWord(0x400000000L | 251L)).registers.getI(3) must be equalTo MixIndex(0x4000)
      // A = -0, I = 0, F = 2, C = 52 ENT4
      execute(state, MixWord(0x400000000L | 252L)).registers.getI(4) must be equalTo MixIndex(0x4000)
      // A = -0, I = 0, F = 2, C = 53 ENT5
      execute(state, MixWord(0x400000000L | 253L)).registers.getI(5) must be equalTo MixIndex(0x4000)
      // A = -0, I = 0, F = 2, C = 54 ENT6
      execute(state, MixWord(0x400000000L | 254L)).registers.getI(6) must be equalTo MixIndex(0x4000)
      // A = -0, I = 0, F = 2, C = 55 ENTX
      execute(state, MixWord(0x400000000L | 255L)).registers.getX must be equalTo MixWord(0x400000000L)
    }

    "load an indexed address" in {
      // A = 3, I = 2, F = 2, C = 48 ENTA
      execute(state, MixWord(3020248L)).registers.getA must be equalTo MixWord(403L)
      // A = 3, I = 2, F = 2, C = 49 ENT1
      execute(state, MixWord(3020249L)).registers.getI(1) must be equalTo MixIndex(403)
      // A = 3, I = 2, F = 2, C = 50 ENT2
      execute(state, MixWord(3020250L)).registers.getI(2) must be equalTo MixIndex(403)
      // A = 3, I = 2, F = 2, C = 51 ENT3
      execute(state, MixWord(3020251L)).registers.getI(3) must be equalTo MixIndex(403)
      // A = 3, I = 2, F = 2, C = 52 ENT4
      execute(state, MixWord(3020252L)).registers.getI(4) must be equalTo MixIndex(403)
      // A = 3, I = 2, F = 2, C = 53 ENT5
      execute(state, MixWord(3020253L)).registers.getI(5) must be equalTo MixIndex(403)
      // A = 3, I = 2, F = 2, C = 54 ENT6
      execute(state, MixWord(3020254L)).registers.getI(6) must be equalTo MixIndex(403)
      // A = 3, I = 2, F = 2, C = 55 ENTX
      execute(state, MixWord(3020255L)).registers.getX must be equalTo MixWord(403L)
    }

    "load the contents of an index register" in {
      // A = 0, I = 2, F = 2, C = 48 ENTA
      execute(state, MixWord(20248L)).registers.getA must be equalTo MixWord(400L)
      // A = 0, I = 2, F = 2, C = 49 ENT1
      execute(state, MixWord(20249L)).registers.getI(1) must be equalTo MixIndex(400)
      // A = 0, I = 2, F = 2, C = 50 ENT2
      execute(state, MixWord(20250L)).registers.getI(2) must be equalTo MixIndex(400)
      // A = 0, I = 2, F = 2, C = 51 ENT3
      execute(state, MixWord(20251L)).registers.getI(3) must be equalTo MixIndex(400)
      // A = 0, I = 2, F = 2, C = 52 ENT4
      execute(state, MixWord(20252L)).registers.getI(4) must be equalTo MixIndex(400)
      // A = 0, I = 2, F = 2, C = 53 ENT5
      execute(state, MixWord(20253L)).registers.getI(5) must be equalTo MixIndex(400)
      // A = 0, I = 2, F = 2, C = 54 ENT6
      execute(state, MixWord(20254L)).registers.getI(6) must be equalTo MixIndex(400)
      // A = 0, I = 2, F = 2, C = 55 ENTX
      execute(state, MixWord(20255L)).registers.getX must be equalTo MixWord(400L)
    }

    "preserve command sign if the indexed address is zero" in {
      // A = -0, I = 1, F = 2, C = 48 ENTA
      execute(state, MixWord(0x400000000L | 10248L)).registers.getA must be equalTo MixWord(0x400000000L)
      // A = -0, I = 1, F = 2, C = 49 ENT1
      execute(state, MixWord(0x400000000L | 10249L)).registers.getI(1) must be equalTo MixIndex(0x4000)
      // A = -0, I = 1, F = 2, C = 50 ENT2
      execute(state, MixWord(0x400000000L | 10250L)).registers.getI(2) must be equalTo MixIndex(0x4000)
      // A = -0, I = 1, F = 2, C = 51 ENT3
      execute(state, MixWord(0x400000000L | 10251L)).registers.getI(3) must be equalTo MixIndex(0x4000)
      // A = -0, I = 1, F = 2, C = 52 ENT4
      execute(state, MixWord(0x400000000L | 10252L)).registers.getI(4) must be equalTo MixIndex(0x4000)
      // A = -0, I = 1, F = 2, C = 53 ENT5
      execute(state, MixWord(0x400000000L | 10253L)).registers.getI(5) must be equalTo MixIndex(0x4000)
      // A = -0, I = 1, F = 2, C = 54 ENT6
      execute(state, MixWord(0x400000000L | 10254L)).registers.getI(6) must be equalTo MixIndex(0x4000)
      // A = -0, I = 1, F = 2, C = 55 ENTX
      execute(state, MixWord(0x400000000L | 10255L)).registers.getX must be equalTo MixWord(0x400000000L)
    }
  }

  "decimal address loader with negation" should {
    "load the negation of a positive number" in {
      // A = 1, I = 0, F = 3, C = 48 ENNA
      execute(state, MixWord(1000348L)).registers.getA must be equalTo MixWord(0x400000001L)
      // A = 1, I = 0, F = 3, C = 49 ENN1
      execute(state, MixWord(1000349L)).registers.getI(1) must be equalTo MixIndex(0x4001)
      // A = 1, I = 0, F = 3, C = 50 ENN2
      execute(state, MixWord(1000350L)).registers.getI(2) must be equalTo MixIndex(0x4001)
      // A = 1, I = 0, F = 3, C = 51 ENN3
      execute(state, MixWord(1000351L)).registers.getI(3) must be equalTo MixIndex(0x4001)
      // A = 1, I = 0, F = 3, C = 52 ENN4
      execute(state, MixWord(1000352L)).registers.getI(4) must be equalTo MixIndex(0x4001)
      // A = 1, I = 0, F = 3, C = 53 ENN5
      execute(state, MixWord(1000353L)).registers.getI(5) must be equalTo MixIndex(0x4001)
      // A = 1, I = 0, F = 3, C = 54 ENN6
      execute(state, MixWord(1000354L)).registers.getI(6) must be equalTo MixIndex(0x4001)
      // A = 1, I = 0, F = 3, C = 55 ENNX
      execute(state, MixWord(1000355L)).registers.getX must be equalTo MixWord(0x400000001L)
    }

    "load the negation of a negative number (positive)" in {
      // A = -1, I = 0, F = 3, C = 48 ENNA
      execute(state, MixWord(0x400000000L | 1000348L)).registers.getA must be equalTo MixWord(1L)
      // A = -1, I = 0, F = 3, C = 49 ENN1
      execute(state, MixWord(0x400000000L | 1000349L)).registers.getI(1) must be equalTo MixIndex(1)
      // A = -1, I = 0, F = 3, C = 50 ENN2
      execute(state, MixWord(0x400000000L | 1000350L)).registers.getI(2) must be equalTo MixIndex(1)
      // A = -1, I = 0, F = 3, C = 51 ENN3
      execute(state, MixWord(0x400000000L | 1000351L)).registers.getI(3) must be equalTo MixIndex(1)
      // A = -1, I = 0, F = 3, C = 52 ENN4
      execute(state, MixWord(0x400000000L | 1000352L)).registers.getI(4) must be equalTo MixIndex(1)
      // A = -1, I = 0, F = 3, C = 53 ENN5
      execute(state, MixWord(0x400000000L | 1000353L)).registers.getI(5) must be equalTo MixIndex(1)
      // A = -1, I = 0, F = 3, C = 54 ENN6
      execute(state, MixWord(0x400000000L | 1000354L)).registers.getI(6) must be equalTo MixIndex(1)
      // A = -1, I = 0, F = 3, C = 55 ENNX
      execute(state, MixWord(0x400000000L | 1000355L)).registers.getX must be equalTo MixWord(1L)
    }

    "load the negation of the positive zero" in {
      // A = 0, I = 0, F = 3, C = 48 ENNA
      execute(state, MixWord(348L)).registers.getA must be equalTo MixWord(0x400000000L)
      // A = 0, I = 0, F = 3, C = 49 ENN1
      execute(state, MixWord(349L)).registers.getI(1) must be equalTo MixIndex(0x4000)
      // A = 0, I = 0, F = 3, C = 50 ENN2
      execute(state, MixWord(350L)).registers.getI(2) must be equalTo MixIndex(0x4000)
      // A = 0, I = 0, F = 3, C = 51 ENN3
      execute(state, MixWord(351L)).registers.getI(3) must be equalTo MixIndex(0x4000)
      // A = 0, I = 0, F = 3, C = 52 ENN4
      execute(state, MixWord(352L)).registers.getI(4) must be equalTo MixIndex(0x4000)
      // A = 0, I = 0, F = 3, C = 53 ENN5
      execute(state, MixWord(353L)).registers.getI(5) must be equalTo MixIndex(0x4000)
      // A = 0, I = 0, F = 3, C = 54 ENN6
      execute(state, MixWord(354L)).registers.getI(6) must be equalTo MixIndex(0x4000)
      // A = 0, I = 0, F = 3, C = 55 ENNX
      execute(state, MixWord(355L)).registers.getX must be equalTo MixWord(0x400000000L)
    }

    "load the negation of an indexed address" in {
      // A = 3, I = 2, F = 3, C = 48 ENNA
      execute(state, MixWord(3020348L)).registers.getA must be equalTo MixWord(0x400000000L | 403L)
      // A = 3, I = 2, F = 3, C = 49 ENN1
      execute(state, MixWord(3020349L)).registers.getI(1) must be equalTo MixIndex((0x4000 | 403).toShort)
      // A = 3, I = 2, F = 3, C = 50 ENN2
      execute(state, MixWord(3020350L)).registers.getI(2) must be equalTo MixIndex((0x4000 | 403).toShort)
      // A = 3, I = 2, F = 3, C = 51 ENN3
      execute(state, MixWord(3020351L)).registers.getI(3) must be equalTo MixIndex((0x4000 | 403).toShort)
      // A = 3, I = 2, F = 3, C = 52 ENN4
      execute(state, MixWord(3020352L)).registers.getI(4) must be equalTo MixIndex((0x4000 | 403).toShort)
      // A = 3, I = 2, F = 3, C = 53 ENN5
      execute(state, MixWord(3020353L)).registers.getI(5) must be equalTo MixIndex((0x4000 | 403).toShort)
      // A = 3, I = 2, F = 3, C = 54 ENN6
      execute(state, MixWord(3020354L)).registers.getI(6) must be equalTo MixIndex((0x4000 | 403).toShort)
      // A = 3, I = 2, F = 3, C = 55 ENNX
      execute(state, MixWord(3020355L)).registers.getX must be equalTo MixWord(0x400000000L | 403L)
    }

    "load the negated contents of an indexed register" in {
      // A = 0, I = 2, F = 3, C = 48 ENNA
      execute(state, MixWord(20348L)).registers.getA must be equalTo MixWord(0x400000000L | 400L)
      // A = 0, I = 2, F = 3, C = 49 ENN1
      execute(state, MixWord(20349L)).registers.getI(1) must be equalTo MixIndex((0x4000 | 400).toShort)
      // A = 0, I = 2, F = 3, C = 50 ENN2
      execute(state, MixWord(20350L)).registers.getI(2) must be equalTo MixIndex((0x4000 | 400).toShort)
      // A = 0, I = 2, F = 3, C = 51 ENN3
      execute(state, MixWord(20351L)).registers.getI(3) must be equalTo MixIndex((0x4000 | 400).toShort)
      // A = 0, I = 2, F = 3, C = 52 ENN4
      execute(state, MixWord(20352L)).registers.getI(4) must be equalTo MixIndex((0x4000 | 400).toShort)
      // A = 0, I = 2, F = 3, C = 53 ENN5
      execute(state, MixWord(20353L)).registers.getI(5) must be equalTo MixIndex((0x4000 | 400).toShort)
      // A = 0, I = 2, F = 3, C = 54 ENN6
      execute(state, MixWord(20354L)).registers.getI(6) must be equalTo MixIndex((0x4000 | 400).toShort)
      // A = 0, I = 2, F = 3, C = 55 ENNX
      execute(state, MixWord(20355L)).registers.getX must be equalTo MixWord(0x400000000L | 400L)
    }

    "invert the command sign if the indexed address is zero" in {
      // A = -0, I = 1, F = 3, C = 48 ENNA
      execute(state, MixWord(0x400000000L | 10348L)).registers.getA must be equalTo MixWord(0L)
      // A = -0, I = 1, F = 3, C = 49 ENN1
      execute(state, MixWord(0x400000000L | 10349L)).registers.getI(1) must be equalTo MixIndex(0)
      // A = -0, I = 1, F = 3, C = 50 ENN2
      execute(state, MixWord(0x400000000L | 10350L)).registers.getI(2) must be equalTo MixIndex(0)
      // A = -0, I = 1, F = 3, C = 51 ENN3
      execute(state, MixWord(0x400000000L | 10351L)).registers.getI(3) must be equalTo MixIndex(0)
      // A = -0, I = 1, F = 3, C = 52 ENN4
      execute(state, MixWord(0x400000000L | 10352L)).registers.getI(4) must be equalTo MixIndex(0)
      // A = -0, I = 1, F = 3, C = 53 ENN5
      execute(state, MixWord(0x400000000L | 10353L)).registers.getI(5) must be equalTo MixIndex(0)
      // A = -0, I = 1, F = 3, C = 54 ENN6
      execute(state, MixWord(0x400000000L | 10354L)).registers.getI(6) must be equalTo MixIndex(0)
      // A = -0, I = 1, F = 3, C = 55 ENNX
      execute(state, MixWord(0x400000000L | 10355L)).registers.getX must be equalTo MixWord(0L)
    }
  }
}
