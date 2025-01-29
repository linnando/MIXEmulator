package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.exceptions.{OverflowException, WrongIndexSpecException}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class AddressingSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

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
      getIndexedAddress(state, MixWord(1000000L)) mustEqual MixIndex(1)
    }

    "perform indexing if 0 < I <= 6" in {
      // A = 1, I = 1
      getIndexedAddress(state, MixWord(1010000L)) mustEqual MixIndex(1)
      // A = 1, I = 2
      getIndexedAddress(state, MixWord(1020000L)) mustEqual MixIndex(401)
      // A = 1, I = 3
      getIndexedAddress(state, MixWord(1030000L)) mustEqual MixIndex(801)
      // A = 1, I = 4
      getIndexedAddress(state, MixWord(1040000L)) mustEqual MixIndex(1201)
      // A = 1, I = 5
      getIndexedAddress(state, MixWord(1050000L)) mustEqual MixIndex(1601)
      // A = 1, I = 6
      getIndexedAddress(state, MixWord(1060000L)) mustEqual MixIndex(1)
    }

    "throw an exception if I > 6" in {
      // A = 1, I = 7
      a[WrongIndexSpecException] must be thrownBy getIndexedAddress(state, MixWord(1070000L))
      // A = 1, I = 99
      a[WrongIndexSpecException] must be thrownBy getIndexedAddress(state, MixWord(1990000L))
    }

    "throw an exception if the indexed address is too big" in {
      // A = 8400, I = 5
      an[OverflowException] must be thrownBy getIndexedAddress(state, MixWord(8400050000L))
    }

    "not throw an exception if the indexed address fits in two bytes (even if it exceeds memory size)" in {
      // A = 8399, I = 5
      getIndexedAddress(state, MixWord(8399050000L)) mustEqual MixIndex(9999)
    }
  }

  "decimal address loader" when {
    "loading to register A" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 48 ENTA
        execute(state, MixWord(1000248L)) map {
          _.registers.getA mustEqual MixWord(1L)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 48 ENTA
        execute(state, MixWord(0x400000000L | 1000248L)) map {
          _.registers.getA mustEqual MixWord(0x400000001L)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 48 ENTA
        execute(state, MixWord(0x400000000L | 248L)) map {
          _.registers.getA mustEqual MixWord(0x400000000L)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 48 ENTA
        execute(state, MixWord(3020248L)) map {
          _.registers.getA mustEqual MixWord(403L)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 48 ENTA
        execute(state, MixWord(20248L)) map {
          _.registers.getA mustEqual MixWord(400L)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 48 ENTA
        execute(state, MixWord(0x400000000L | 10248L)) map {
          _.registers.getA mustEqual MixWord(0x400000000L)
        }
      }
    }

    "loading to register I1" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 49 ENT1
        execute(state, MixWord(1000249L)) map {
          _.registers.getI(1) mustEqual MixIndex(1)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 49 ENT1
        execute(state, MixWord(0x400000000L | 1000249L)) map {
          _.registers.getI(1) mustEqual MixIndex(0x4001)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 49 ENT1
        execute(state, MixWord(0x400000000L | 249L)) map {
          _.registers.getI(1) mustEqual MixIndex(0x4000)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 49 ENT1
        execute(state, MixWord(3020249L)) map {
          _.registers.getI(1) mustEqual MixIndex(403)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 49 ENT1
        execute(state, MixWord(20249L)) map {
          _.registers.getI(1) mustEqual MixIndex(400)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 49 ENT1
        execute(state, MixWord(0x400000000L | 10249L)) map {
          _.registers.getI(1) mustEqual MixIndex(0x4000)
        }
      }
    }

    "loading to register I2" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 50 ENT2
        execute(state, MixWord(1000250L)) map {
          _.registers.getI(2) mustEqual MixIndex(1)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 50 ENT2
        execute(state, MixWord(0x400000000L | 1000250L)) map {
          _.registers.getI(2) mustEqual MixIndex(0x4001)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 50 ENT2
        execute(state, MixWord(0x400000000L | 250L)) map {
          _.registers.getI(2) mustEqual MixIndex(0x4000)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 50 ENT2
        execute(state, MixWord(3020250L)) map {
          _.registers.getI(2) mustEqual MixIndex(403)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 50 ENT2
        execute(state, MixWord(20250L)) map {
          _.registers.getI(2) mustEqual MixIndex(400)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 50 ENT2
        execute(state, MixWord(0x400000000L | 10250L)) map {
          _.registers.getI(2) mustEqual MixIndex(0x4000)
        }
      }
    }

    "loading to register I3" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 51 ENT3
        execute(state, MixWord(1000251L)) map {
          _.registers.getI(3) mustEqual MixIndex(1)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 51 ENT3
        execute(state, MixWord(0x400000000L | 1000251L)) map {
          _.registers.getI(3) mustEqual MixIndex(0x4001)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 51 ENT3
        execute(state, MixWord(0x400000000L | 251L)) map {
          _.registers.getI(3) mustEqual MixIndex(0x4000)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 51 ENT3
        execute(state, MixWord(3020251L)) map {
          _.registers.getI(3) mustEqual MixIndex(403)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 51 ENT3
        execute(state, MixWord(20251L)) map {
          _.registers.getI(3) mustEqual MixIndex(400)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 51 ENT3
        execute(state, MixWord(0x400000000L | 10251L)) map {
          _.registers.getI(3) mustEqual MixIndex(0x4000)
        }
      }
    }

    "loading to register I4" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 52 ENT4
        execute(state, MixWord(1000252L)) map {
          _.registers.getI(4) mustEqual MixIndex(1)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 52 ENT4
        execute(state, MixWord(0x400000000L | 1000252L)) map {
          _.registers.getI(4) mustEqual MixIndex(0x4001)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 52 ENT4
        execute(state, MixWord(0x400000000L | 252L)) map {
          _.registers.getI(4) mustEqual MixIndex(0x4000)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 52 ENT4
        execute(state, MixWord(3020252L)) map {
          _.registers.getI(4) mustEqual MixIndex(403)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 52 ENT4
        execute(state, MixWord(20252L)) map {
          _.registers.getI(4) mustEqual MixIndex(400)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 52 ENT4
        execute(state, MixWord(0x400000000L | 10252L)) map {
          _.registers.getI(4) mustEqual MixIndex(0x4000)
        }
      }
    }

    "loading to register I5" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 53 ENT5
        execute(state, MixWord(1000253L)) map {
          _.registers.getI(5) mustEqual MixIndex(1)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 53 ENT5
        execute(state, MixWord(0x400000000L | 1000253L)) map {
          _.registers.getI(5) mustEqual MixIndex(0x4001)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 53 ENT5
        execute(state, MixWord(0x400000000L | 253L)) map {
          _.registers.getI(5) mustEqual MixIndex(0x4000)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 53 ENT5
        execute(state, MixWord(3020253L)) map {
          _.registers.getI(5) mustEqual MixIndex(403)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 53 ENT5
        execute(state, MixWord(20253L)) map {
          _.registers.getI(5) mustEqual MixIndex(400)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 53 ENT5
        execute(state, MixWord(0x400000000L | 10253L)) map {
          _.registers.getI(5) mustEqual MixIndex(0x4000)
        }
      }
    }

    "loading to register I6" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 54 ENT6
        execute(state, MixWord(1000254L)) map {
          _.registers.getI(6) mustEqual MixIndex(1)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 54 ENT6
        execute(state, MixWord(0x400000000L | 1000254L)) map {
          _.registers.getI(6) mustEqual MixIndex(0x4001)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 54 ENT6
        execute(state, MixWord(0x400000000L | 254L)) map {
          _.registers.getI(6) mustEqual MixIndex(0x4000)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 54 ENT6
        execute(state, MixWord(3020254L)) map {
          _.registers.getI(6) mustEqual MixIndex(403)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 54 ENT6
        execute(state, MixWord(20254L)) map {
          _.registers.getI(6) mustEqual MixIndex(400)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 54 ENT6
        execute(state, MixWord(0x400000000L | 10254L)) map {
          _.registers.getI(6) mustEqual MixIndex(0x4000)
        }
      }
    }

    "loading to register X" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 55 ENTX
        execute(state, MixWord(1000255L)) map {
          _.registers.getX mustEqual MixWord(1L)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 55 ENTX
        execute(state, MixWord(0x400000000L | 1000255L)) map {
          _.registers.getX mustEqual MixWord(0x400000001L)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 55 ENTX
        execute(state, MixWord(0x400000000L | 255L)) map {
          _.registers.getX mustEqual MixWord(0x400000000L)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 55 ENTX
        execute(state, MixWord(3020255L)) map {
          _.registers.getX mustEqual MixWord(403L)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 55 ENTX
        execute(state, MixWord(20255L)) map {
          _.registers.getX mustEqual MixWord(400L)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 55 ENTX
        execute(state, MixWord(0x400000000L | 10255L)) map {
          _.registers.getX mustEqual MixWord(0x400000000L)
        }
      }
    }
  }

  "decimal address loader with negation" when {
    "loading to register A" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 48 ENNA
        execute(state, MixWord(1000348L)) map {
          _.registers.getA mustEqual MixWord(0x400000001L)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 48 ENNA
        execute(state, MixWord(0x400000000L | 1000348L)) map {
          _.registers.getA mustEqual MixWord(1L)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 48 ENNA
        execute(state, MixWord(348L)) map {
          _.registers.getA mustEqual MixWord(0x400000000L)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 48 ENNA
        execute(state, MixWord(3020348L)) map {
          _.registers.getA mustEqual MixWord(0x400000000L | 403L)
        }
      }

      "load the negated contents of an indexed register" in {
        // A = 0, I = 2, F = 3, C = 48 ENNA
        execute(state, MixWord(20348L)) map {
          _.registers.getA mustEqual MixWord(0x400000000L | 400L)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 48 ENNA
        execute(state, MixWord(0x400000000L | 10348L)) map {
          _.registers.getA mustEqual MixWord(0L)
        }
      }
    }

    "loading to register I1" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 49 ENN1
        execute(state, MixWord(1000349L)) map {
          _.registers.getI(1) mustEqual MixIndex(0x4001)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 49 ENN1
        execute(state, MixWord(0x400000000L | 1000349L)) map {
          _.registers.getI(1) mustEqual MixIndex(1)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 49 ENN1
        execute(state, MixWord(349L)) map {
          _.registers.getI(1) mustEqual MixIndex(0x4000)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 49 ENN1
        execute(state, MixWord(3020349L)) map {
          _.registers.getI(1) mustEqual MixIndex((0x4000 | 403).toShort)
        }
      }

      "load the negated contents of an indexed register" in {
        // A = 0, I = 2, F = 3, C = 49 ENN1
        execute(state, MixWord(20349L)) map {
          _.registers.getI(1) mustEqual MixIndex((0x4000 | 400).toShort)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 49 ENN1
        execute(state, MixWord(0x400000000L | 10349L)) map {
          _.registers.getI(1) mustEqual MixIndex(0)
        }
      }
    }

    "loading to register I2" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 50 ENN2
        execute(state, MixWord(1000350L)) map {
          _.registers.getI(2) mustEqual MixIndex(0x4001)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 50 ENN2
        execute(state, MixWord(0x400000000L | 1000350L)) map {
          _.registers.getI(2) mustEqual MixIndex(1)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 50 ENN2
        execute(state, MixWord(350L)) map {
          _.registers.getI(2) mustEqual MixIndex(0x4000)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 50 ENN2
        execute(state, MixWord(3020350L)) map {
          _.registers.getI(2) mustEqual MixIndex((0x4000 | 403).toShort)
        }
      }

      "load the negated contents of an indexed register" in {
        // A = 0, I = 2, F = 3, C = 50 ENN2
        execute(state, MixWord(20350L)) map {
          _.registers.getI(2) mustEqual MixIndex((0x4000 | 400).toShort)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 50 ENN2
        execute(state, MixWord(0x400000000L | 10350L)) map {
          _.registers.getI(2) mustEqual MixIndex(0)
        }
      }
    }

    "loading to register I3" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 51 ENN3
        execute(state, MixWord(1000351L)) map {
          _.registers.getI(3) mustEqual MixIndex(0x4001)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 51 ENN3
        execute(state, MixWord(0x400000000L | 1000351L)) map {
          _.registers.getI(3) mustEqual MixIndex(1)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 51 ENN3
        execute(state, MixWord(351L)) map {
          _.registers.getI(3) mustEqual MixIndex(0x4000)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 51 ENN3
        execute(state, MixWord(3020351L)) map {
          _.registers.getI(3) mustEqual MixIndex((0x4000 | 403).toShort)
        }
      }

      "load the negated contents of an indexed register" in {
        // A = 0, I = 2, F = 3, C = 51 ENN3
        execute(state, MixWord(20351L)) map {
          _.registers.getI(3) mustEqual MixIndex((0x4000 | 400).toShort)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 51 ENN3
        execute(state, MixWord(0x400000000L | 10351L)) map {
          _.registers.getI(3) mustEqual MixIndex(0)
        }
      }
    }

    "loading to register I4" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 52 ENN4
        execute(state, MixWord(1000352L)) map {
          _.registers.getI(4) mustEqual MixIndex(0x4001)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 52 ENN4
        execute(state, MixWord(0x400000000L | 1000352L)) map {
          _.registers.getI(4) mustEqual MixIndex(1)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 52 ENN4
        execute(state, MixWord(352L)) map {
          _.registers.getI(4) mustEqual MixIndex(0x4000)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 52 ENN4
        execute(state, MixWord(3020352L)) map {
          _.registers.getI(4) mustEqual MixIndex((0x4000 | 403).toShort)
        }
      }

      "load the negated contents of an indexed register" in {
        // A = 0, I = 2, F = 3, C = 52 ENN4
        execute(state, MixWord(20352L)) map {
          _.registers.getI(4) mustEqual MixIndex((0x4000 | 400).toShort)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 52 ENN4
        execute(state, MixWord(0x400000000L | 10352L)) map {
          _.registers.getI(4) mustEqual MixIndex(0)
        }
      }
    }

    "loading to register I5" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 53 ENN5
        execute(state, MixWord(1000353L)) map {
          _.registers.getI(5) mustEqual MixIndex(0x4001)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 53 ENN5
        execute(state, MixWord(0x400000000L | 1000353L)) map {
          _.registers.getI(5) mustEqual MixIndex(1)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 53 ENN5
        execute(state, MixWord(353L)) map {
          _.registers.getI(5) mustEqual MixIndex(0x4000)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 53 ENN5
        execute(state, MixWord(3020353L)) map {
          _.registers.getI(5) mustEqual MixIndex((0x4000 | 403).toShort)
        }
      }

      "load the negated contents of an indexed register" in {
        // A = 0, I = 2, F = 3, C = 53 ENN5
        execute(state, MixWord(20353L)) map {
          _.registers.getI(5) mustEqual MixIndex((0x4000 | 400).toShort)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 53 ENN5
        execute(state, MixWord(0x400000000L | 10353L)) map {
          _.registers.getI(5) mustEqual MixIndex(0)
        }
      }
    }

    "loading to register I6" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 54 ENN6
        execute(state, MixWord(1000354L)) map {
          _.registers.getI(6) mustEqual MixIndex(0x4001)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 54 ENN6
        execute(state, MixWord(0x400000000L | 1000354L)) map {
          _.registers.getI(6) mustEqual MixIndex(1)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 54 ENN6
        execute(state, MixWord(354L)) map {
          _.registers.getI(6) mustEqual MixIndex(0x4000)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 54 ENN6
        execute(state, MixWord(3020354L)) map {
          _.registers.getI(6) mustEqual MixIndex((0x4000 | 403).toShort)
        }
      }

      "load the negated contents of an indexed register" in {
        // A = 0, I = 2, F = 3, C = 54 ENN6
        execute(state, MixWord(20354L)) map {
          _.registers.getI(6) mustEqual MixIndex((0x4000 | 400).toShort)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 54 ENN6
        execute(state, MixWord(0x400000000L | 10354L)) map {
          _.registers.getI(6) mustEqual MixIndex(0)
        }
      }
    }

    "loading to register X" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 55 ENNX
        execute(state, MixWord(1000355L)) map {
          _.registers.getX mustEqual MixWord(0x400000001L)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 55 ENNX
        execute(state, MixWord(0x400000000L | 1000355L)) map {
          _.registers.getX mustEqual MixWord(1L)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 55 ENNX
        execute(state, MixWord(355L)) map {
          _.registers.getX mustEqual MixWord(0x400000000L)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 55 ENNX
        execute(state, MixWord(3020355L)) map {
          _.registers.getX mustEqual MixWord(0x400000000L | 403L)
        }
      }

      "load the negated contents of an indexed register" in {
        // A = 0, I = 2, F = 3, C = 55 ENNX
        execute(state, MixWord(20355L)) map {
          _.registers.getX mustEqual MixWord(0x400000000L | 400L)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 55 ENNX
        execute(state, MixWord(0x400000000L | 10355L)) map {
          _.registers.getX mustEqual MixWord(0L)
        }
      }
    }
  }
}
