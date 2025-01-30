package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.binary
import org.linnando.mixemulator.vm.exceptions.{OverflowException, WrongIndexSpecException}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class AddressingSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

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
      getIndexedAddress(state, MixWord(0x40000)) mustEqual MixIndex(0x1)
    }

    "perform indexing if 0 < I <= 6" in {
      // A = 0x1, I = 1
      getIndexedAddress(state, MixWord(0x41000)) mustEqual MixIndex(0x0001)
      // A = 0x1, I = 2
      getIndexedAddress(state, MixWord(0x42000)) mustEqual MixIndex(0x0101)
      // A = 0x1, I = 3
      getIndexedAddress(state, MixWord(0x43000)) mustEqual MixIndex(0x0201)
      // A = 0x1, I = 4
      getIndexedAddress(state, MixWord(0x44000)) mustEqual MixIndex(0x0301)
      // A = 0x1, I = 5
      getIndexedAddress(state, MixWord(0x45000)) mustEqual MixIndex(0x0401)
      // A = 0x1, I = 6
      getIndexedAddress(state, MixWord(0x46000)) mustEqual MixIndex(0x0001)
    }

    "throw an exception if I > 6" in {
      // A = 0x1, I = 7
      val exception7 = the[WrongIndexSpecException] thrownBy getIndexedAddress(state, MixWord(0x47000))
      exception7.indexSpec mustEqual 7
      // A = 0x1, I = 0x3f
      val exception3f = the[WrongIndexSpecException] thrownBy getIndexedAddress(state, MixWord(0x7f000))
      exception3f.indexSpec mustEqual 0x3f
    }

    "throw an exception if the indexed address is too big" in {
      // A = 0xc00, I = 5
      an[OverflowException] must be thrownBy getIndexedAddress(state, MixWord(0x30005000))
    }

    "not throw an exception if the indexed address fits in two bytes (even if it exceeds memory size)" in {
      // A = 0xbff, I = 5
      getIndexedAddress(state, MixWord(0x2ffc5000)) mustEqual MixIndex(0xfff)
    }
  }

  "binary address loader" when {
    "loading to register A" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 48 ENTA
        binary.execute(state, MixWord(0x000400b0)) map {
          _.registers.getA mustEqual MixWord(0x00000001)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 48 ENTA
        binary.execute(state, MixWord(0x400400b0)) map {
          _.registers.getA mustEqual MixWord(0x40000001)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 48 ENTA
        binary.execute(state, MixWord(0x400000b0)) map {
          _.registers.getA mustEqual MixWord(0x40000000)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 48 ENTA
        binary.execute(state, MixWord(0x000020b0)) map {
          _.registers.getA mustEqual MixWord(0x00000100)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 48 ENTA
        binary.execute(state, MixWord(0x000c20b0)) map {
          _.registers.getA mustEqual MixWord(0x00000103)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 48 ENTA
        binary.execute(state, MixWord(0x400010b0)) map {
          _.registers.getA mustEqual MixWord(0x40000000)
        }
      }
    }

    "loading to register I1" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 49 ENT1
        binary.execute(state, MixWord(0x000400b1)) map {
          _.registers.getI(1) mustEqual MixIndex(0x0001)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 49 ENT1
        binary.execute(state, MixWord(0x400400b1)) map {
          _.registers.getI(1) mustEqual MixIndex(0x1001)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 49 ENT1
        binary.execute(state, MixWord(0x400000b1)) map {
          _.registers.getI(1) mustEqual MixIndex(0x1000)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 49 ENT1
        binary.execute(state, MixWord(0x000020b1)) map {
          _.registers.getI(1) mustEqual MixIndex(0x0100)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 49 ENT1
        binary.execute(state, MixWord(0x000c20b1)) map {
          _.registers.getI(1) mustEqual MixIndex(0x0103)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 49 ENT1
        binary.execute(state, MixWord(0x400010b1)) map {
          _.registers.getI(1) mustEqual MixIndex(0x1000)
        }
      }
    }

    "loading to register I2" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 50 ENT2
        binary.execute(state, MixWord(0x000400b2)) map {
          _.registers.getI(2) mustEqual MixIndex(0x0001)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 50 ENT2
        binary.execute(state, MixWord(0x400400b2)) map {
          _.registers.getI(2) mustEqual MixIndex(0x1001)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 50 ENT2
        binary.execute(state, MixWord(0x400000b2)) map {
          _.registers.getI(2) mustEqual MixIndex(0x1000)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 50 ENT2
        binary.execute(state, MixWord(0x000020b2)) map {
          _.registers.getI(2) mustEqual MixIndex(0x0100)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 50 ENT2
        binary.execute(state, MixWord(0x000c20b2)) map {
          _.registers.getI(2) mustEqual MixIndex(0x0103)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 50 ENT2
        binary.execute(state, MixWord(0x400010b2)) map {
          _.registers.getI(2) mustEqual MixIndex(0x1000)
        }
      }
    }

    "loading to register I3" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 51 ENT3
        binary.execute(state, MixWord(0x000400b3)) map {
          _.registers.getI(3) mustEqual MixIndex(0x0001)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 51 ENT3
        binary.execute(state, MixWord(0x400400b3)) map {
          _.registers.getI(3) mustEqual MixIndex(0x1001)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 51 ENT3
        binary.execute(state, MixWord(0x400000b3)) map {
          _.registers.getI(3) mustEqual MixIndex(0x1000)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 51 ENT3
        binary.execute(state, MixWord(0x000020b3)) map {
          _.registers.getI(3) mustEqual MixIndex(0x0100)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 51 ENT3
        binary.execute(state, MixWord(0x000c20b3)) map {
          _.registers.getI(3) mustEqual MixIndex(0x0103)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 51 ENT3
        binary.execute(state, MixWord(0x400010b3)) map {
          _.registers.getI(3) mustEqual MixIndex(0x1000)
        }
      }
    }

    "loading to register I4" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 52 ENT4
        binary.execute(state, MixWord(0x000400b4)) map {
          _.registers.getI(4) mustEqual MixIndex(0x0001)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 52 ENT4
        binary.execute(state, MixWord(0x400400b4)) map {
          _.registers.getI(4) mustEqual MixIndex(0x1001)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 52 ENT4
        binary.execute(state, MixWord(0x400000b4)) map {
          _.registers.getI(4) mustEqual MixIndex(0x1000)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 52 ENT4
        binary.execute(state, MixWord(0x000020b4)) map {
          _.registers.getI(4) mustEqual MixIndex(0x0100)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 52 ENT4
        binary.execute(state, MixWord(0x000c20b4)) map {
          _.registers.getI(4) mustEqual MixIndex(0x0103)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 52 ENT4
        binary.execute(state, MixWord(0x400010b4)) map {
          _.registers.getI(4) mustEqual MixIndex(0x1000)
        }
      }
    }

    "loading to register I5" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 53 ENT5
        binary.execute(state, MixWord(0x000400b5)) map {
          _.registers.getI(5) mustEqual MixIndex(0x0001)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 53 ENT5
        binary.execute(state, MixWord(0x400400b5)) map {
          _.registers.getI(5) mustEqual MixIndex(0x1001)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 53 ENT5
        binary.execute(state, MixWord(0x400000b5)) map {
          _.registers.getI(5) mustEqual MixIndex(0x1000)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 53 ENT5
        binary.execute(state, MixWord(0x000020b5)) map {
          _.registers.getI(5) mustEqual MixIndex(0x0100)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 53 ENT5
        binary.execute(state, MixWord(0x000c20b5)) map {
          _.registers.getI(5) mustEqual MixIndex(0x0103)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 53 ENT5
        binary.execute(state, MixWord(0x400010b5)) map {
          _.registers.getI(5) mustEqual MixIndex(0x1000)
        }
      }
    }

    "loading to register I6" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 54 ENT6
        binary.execute(state, MixWord(0x000400b6)) map {
          _.registers.getI(6) mustEqual MixIndex(0x0001)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 54 ENT6
        binary.execute(state, MixWord(0x400400b6)) map {
          _.registers.getI(6) mustEqual MixIndex(0x1001)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 54 ENT6
        binary.execute(state, MixWord(0x400000b6)) map {
          _.registers.getI(6) mustEqual MixIndex(0x1000)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 54 ENT6
        binary.execute(state, MixWord(0x000020b6)) map {
          _.registers.getI(6) mustEqual MixIndex(0x0100)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 54 ENT6
        binary.execute(state, MixWord(0x000c20b6)) map {
          _.registers.getI(6) mustEqual MixIndex(0x0103)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 54 ENT6
        binary.execute(state, MixWord(0x400010b6)) map {
          _.registers.getI(6) mustEqual MixIndex(0x1000)
        }
      }
    }

    "loading to register X" should {
      "load a positive number" in {
        // A = 1, I = 0, F = 2, C = 55 ENTX
        binary.execute(state, MixWord(0x000400b7)) map {
          _.registers.getX mustEqual MixWord(0x00000001)
        }
      }

      "load a negative number" in {
        // A = -1, I = 0, F = 2, C = 55 ENTX
        binary.execute(state, MixWord(0x400400b7)) map {
          _.registers.getX mustEqual MixWord(0x40000001)
        }
      }

      "load the negative zero" in {
        // A = -0, I = 0, F = 2, C = 55 ENTX
        binary.execute(state, MixWord(0x400000b7)) map {
          _.registers.getX mustEqual MixWord(0x40000000)
        }
      }

      "load the contents of an index register" in {
        // A = 0, I = 2, F = 2, C = 55 ENTX
        binary.execute(state, MixWord(0x000020b7)) map {
          _.registers.getX mustEqual MixWord(0x00000100)
        }
      }

      "load an indexed address" in {
        // A = 3, I = 2, F = 2, C = 55 ENTX
        binary.execute(state, MixWord(0x000c20b7)) map {
          _.registers.getX mustEqual MixWord(0x00000103)
        }
      }

      "preserve command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 2, C = 55 ENTX
        binary.execute(state, MixWord(0x400010b7)) map {
          _.registers.getX mustEqual MixWord(0x40000000)
        }
      }
    }
  }

  "binary address loader with negation" when {
    "loading to register A" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 48 ENNA
        binary.execute(state, MixWord(0x000400f0)) map {
          _.registers.getA mustEqual MixWord(0x40000001)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 48 ENNA
        binary.execute(state, MixWord(0x400400f0)) map {
          _.registers.getA mustEqual MixWord(0x00000001)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 48 ENNA
        binary.execute(state, MixWord(0x000000f0)) map {
          _.registers.getA mustEqual MixWord(0x40000000)
        }
      }

      "load the negated contents of an index register" in {
        // A = 0, I = 2, F = 3, C = 48 ENNA
        binary.execute(state, MixWord(0x000020f0)) map {
          _.registers.getA mustEqual MixWord(0x40000100)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 48 ENNA
        binary.execute(state, MixWord(0x000c20f0)) map {
          _.registers.getA mustEqual MixWord(0x40000103)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 48 ENNA
        binary.execute(state, MixWord(0x400010f0)) map {
          _.registers.getA mustEqual MixWord(0x00000000)
        }
      }
    }

    "loading to register I1" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 49 ENN1
        binary.execute(state, MixWord(0x000400f1)) map {
          _.registers.getI(1) mustEqual MixIndex(0x1001)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 49 ENN1
        binary.execute(state, MixWord(0x400400f1)) map {
          _.registers.getI(1) mustEqual MixIndex(0x0001)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 49 ENN1
        binary.execute(state, MixWord(0x000000f1)) map {
          _.registers.getI(1) mustEqual MixIndex(0x1000)
        }
      }

      "load the negated contents of an index register" in {
        // A = 0, I = 2, F = 3, C = 49 ENN1
        binary.execute(state, MixWord(0x000020f1)) map {
          _.registers.getI(1) mustEqual MixIndex(0x1100)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 49 ENN1
        binary.execute(state, MixWord(0x000c20f1)) map {
          _.registers.getI(1) mustEqual MixIndex(0x1103)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 49 ENN1
        binary.execute(state, MixWord(0x400010f1)) map {
          _.registers.getI(1) mustEqual MixIndex(0x0000)
        }
      }
    }

    "loading to register I2" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 50 ENN2
        binary.execute(state, MixWord(0x000400f2)) map {
          _.registers.getI(2) mustEqual MixIndex(0x1001)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 50 ENN2
        binary.execute(state, MixWord(0x400400f2)) map {
          _.registers.getI(2) mustEqual MixIndex(0x0001)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 50 ENN2
        binary.execute(state, MixWord(0x000000f2)) map {
          _.registers.getI(2) mustEqual MixIndex(0x1000)
        }
      }

      "load the negated contents of an index register" in {
        // A = 0, I = 2, F = 3, C = 50 ENN2
        binary.execute(state, MixWord(0x000020f2)) map {
          _.registers.getI(2) mustEqual MixIndex(0x1100)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 50 ENN2
        binary.execute(state, MixWord(0x000c20f2)) map {
          _.registers.getI(2) mustEqual MixIndex(0x1103)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 50 ENN2
        binary.execute(state, MixWord(0x400010f2)) map {
          _.registers.getI(2) mustEqual MixIndex(0x0000)
        }
      }
    }

    "loading to register I3" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 51 ENN3
        binary.execute(state, MixWord(0x000400f3)) map {
          _.registers.getI(3) mustEqual MixIndex(0x1001)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 51 ENN3
        binary.execute(state, MixWord(0x400400f3)) map {
          _.registers.getI(3) mustEqual MixIndex(0x0001)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 51 ENN3
        binary.execute(state, MixWord(0x000000f3)) map {
          _.registers.getI(3) mustEqual MixIndex(0x1000)
        }
      }

      "load the negated contents of an index register" in {
        // A = 0, I = 2, F = 3, C = 51 ENN3
        binary.execute(state, MixWord(0x000020f3)) map {
          _.registers.getI(3) mustEqual MixIndex(0x1100)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 51 ENN3
        binary.execute(state, MixWord(0x000c20f3)) map {
          _.registers.getI(3) mustEqual MixIndex(0x1103)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 51 ENN3
        binary.execute(state, MixWord(0x400010f3)) map {
          _.registers.getI(3) mustEqual MixIndex(0x0000)
        }
      }
    }

    "loading to register I4" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 52 ENN4
        binary.execute(state, MixWord(0x000400f4)) map {
          _.registers.getI(4) mustEqual MixIndex(0x1001)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 52 ENN4
        binary.execute(state, MixWord(0x400400f4)) map {
          _.registers.getI(4) mustEqual MixIndex(0x0001)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 52 ENN4
        binary.execute(state, MixWord(0x000000f4)) map {
          _.registers.getI(4) mustEqual MixIndex(0x1000)
        }
      }

      "load the negated contents of an index register" in {
        // A = 0, I = 2, F = 3, C = 52 ENN4
        binary.execute(state, MixWord(0x000020f4)) map {
          _.registers.getI(4) mustEqual MixIndex(0x1100)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 52 ENN4
        binary.execute(state, MixWord(0x000c20f4)) map {
          _.registers.getI(4) mustEqual MixIndex(0x1103)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 52 ENN4
        binary.execute(state, MixWord(0x400010f4)) map {
          _.registers.getI(4) mustEqual MixIndex(0x0000)
        }
      }
    }

    "loading to register I5" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 53 ENN5
        binary.execute(state, MixWord(0x000400f5)) map {
          _.registers.getI(5) mustEqual MixIndex(0x1001)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 53 ENN5
        binary.execute(state, MixWord(0x400400f5)) map {
          _.registers.getI(5) mustEqual MixIndex(0x0001)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 53 ENN5
        binary.execute(state, MixWord(0x000000f5)) map {
          _.registers.getI(5) mustEqual MixIndex(0x1000)
        }
      }

      "load the negated contents of an index register" in {
        // A = 0, I = 2, F = 3, C = 53 ENN5
        binary.execute(state, MixWord(0x000020f5)) map {
          _.registers.getI(5) mustEqual MixIndex(0x1100)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 53 ENN5
        binary.execute(state, MixWord(0x000c20f5)) map {
          _.registers.getI(5) mustEqual MixIndex(0x1103)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 53 ENN5
        binary.execute(state, MixWord(0x400010f5)) map {
          _.registers.getI(5) mustEqual MixIndex(0x0000)
        }
      }
    }

    "loading to register I6" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 54 ENN6
        binary.execute(state, MixWord(0x000400f6)) map {
          _.registers.getI(6) mustEqual MixIndex(0x1001)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 54 ENN6
        binary.execute(state, MixWord(0x400400f6)) map {
          _.registers.getI(6) mustEqual MixIndex(0x0001)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 54 ENN6
        binary.execute(state, MixWord(0x000000f6)) map {
          _.registers.getI(6) mustEqual MixIndex(0x1000)
        }
      }

      "load the negated contents of an index register" in {
        // A = 0, I = 2, F = 3, C = 54 ENN6
        binary.execute(state, MixWord(0x000020f6)) map {
          _.registers.getI(6) mustEqual MixIndex(0x1100)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 54 ENN6
        binary.execute(state, MixWord(0x000c20f6)) map {
          _.registers.getI(6) mustEqual MixIndex(0x1103)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 54 ENN6
        binary.execute(state, MixWord(0x400010f6)) map {
          _.registers.getI(6) mustEqual MixIndex(0x0000)
        }
      }
    }

    "loading to register X" should {
      "load the negation of a positive number" in {
        // A = 1, I = 0, F = 3, C = 55 ENNX
        binary.execute(state, MixWord(0x000400f7)) map {
          _.registers.getX mustEqual MixWord(0x40000001)
        }
      }

      "load the negation of a negative number (positive)" in {
        // A = -1, I = 0, F = 3, C = 55 ENNX
        binary.execute(state, MixWord(0x400400f7)) map {
          _.registers.getX mustEqual MixWord(0x00000001)
        }
      }

      "load the negation of the positive zero" in {
        // A = 0, I = 0, F = 3, C = 55 ENNX
        binary.execute(state, MixWord(0x000000f7)) map {
          _.registers.getX mustEqual MixWord(0x40000000)
        }
      }

      "load the negated contents of an index register" in {
        // A = 0, I = 2, F = 3, C = 55 ENNX
        binary.execute(state, MixWord(0x000020f7)) map {
          _.registers.getX mustEqual MixWord(0x40000100)
        }
      }

      "load the negation of an indexed address" in {
        // A = 3, I = 2, F = 3, C = 55 ENNX
        binary.execute(state, MixWord(0x000c20f7)) map {
          _.registers.getX mustEqual MixWord(0x40000103)
        }
      }

      "invert the command sign if the indexed address is zero" in {
        // A = -0, I = 1, F = 3, C = 55 ENNX
        binary.execute(state, MixWord(0x400010f7)) map {
          _.registers.getX mustEqual MixWord(0x00000000)
        }
      }
    }
  }
}
