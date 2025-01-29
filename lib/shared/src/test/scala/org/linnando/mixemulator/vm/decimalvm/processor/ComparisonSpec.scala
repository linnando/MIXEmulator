package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.{Comparison, decimal}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class ComparisonSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  import decimal._

  private val initialState = decimal.initialState
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(MixWord(0x400000000L | 1918000009L)) // - 19 18 0 0 9
      .updatedX(MixWord(23032L)) // + 0 0 2 30 32
      .updatedI(1, MixIndex((0x4000 | 5937).toShort)) // - 59 37
      .updatedI(2, MixIndex(319)) // + 3 19
      .updatedI(3, MixIndex(3042)) // + 30 42
      .updatedI(4, MixIndex((0x4000 | 5926).toShort)) // - 59 26
      .updatedI(5, MixIndex(857)) // + 8 57
      .updatedI(6, MixIndex((0x4000 | 1246).toShort)), // - 12 46
    memory = initialState.memory
      .updated(MixIndex(1000), MixWord(0x400000000L | 22200L)) // - 0 0 2 22 0
  )

  "decimal comparison module" when {
    "comparing register A with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 56 CMPA
        execute(state, MixWord(1000000556L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare the whole word except for the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 56 CMPA
        execute(state, MixWord(1000001356L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 56 CMPA
        execute(state, MixWord(1000000356L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a left-side field not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 56 CMPA
        execute(state, MixWord(1000001156L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 56 CMPA
        execute(state, MixWord(1000003756L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 56 CMPA
        execute(state, MixWord(1000000256L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 56 CMPA
        execute(state, MixWord(1000001056L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }
    }

    "comparing register I1 with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 57 CMP1
        execute(state, MixWord(1000000557L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare the whole word except for the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 57 CMP1
        execute(state, MixWord(1000001357L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 57 CMP1
        execute(state, MixWord(1000000357L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a left-side field not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 57 CMP1
        execute(state, MixWord(1000001157L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 57 CMP1
        execute(state, MixWord(1000003757L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 57 CMP1
        execute(state, MixWord(1000000257L)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 57 CMP1
        execute(state, MixWord(1000001057L)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }
    }

    "comparing register I2 with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 58 CMP2
        execute(state, MixWord(1000000558L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare the whole word except for the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 58 CMP2
        execute(state, MixWord(1000001358L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 58 CMP2
        execute(state, MixWord(1000000358L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a left-side field not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 58 CMP2
        execute(state, MixWord(1000001158L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 58 CMP2
        execute(state, MixWord(1000003758L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 58 CMP2
        execute(state, MixWord(1000000258L)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 58 CMP2
        execute(state, MixWord(1000001058L)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }
    }

    "comparing register I3 with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 59 CMP3
        execute(state, MixWord(1000000559L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare the whole word except for the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 59 CMP3
        execute(state, MixWord(1000001359L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 59 CMP3
        execute(state, MixWord(1000000359L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a left-side field not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 59 CMP3
        execute(state, MixWord(1000001159L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 59 CMP3
        execute(state, MixWord(1000003759L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 59 CMP3
        execute(state, MixWord(1000000259L)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 59 CMP3
        execute(state, MixWord(1000001059L)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }
    }

    "comparing register I4 with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 60 CMP4
        execute(state, MixWord(1000000560L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare the whole word except for the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 60 CMP4
        execute(state, MixWord(1000001360L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 60 CMP4
        execute(state, MixWord(1000000360L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a left-side field not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 60 CMP4
        execute(state, MixWord(1000001160L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 60 CMP4
        execute(state, MixWord(1000003760L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 60 CMP4
        execute(state, MixWord(1000000260L)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 60 CMP4
        execute(state, MixWord(1000001060L)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }
    }

    "comparing register I5 with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 61 CMP5
        execute(state, MixWord(1000000561L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare the whole word except for the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 61 CMP5
        execute(state, MixWord(1000001361L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 61 CMP5
        execute(state, MixWord(1000000361L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a left-side field not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 61 CMP5
        execute(state, MixWord(1000001161L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 61 CMP5
        execute(state, MixWord(1000003761L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 61 CMP5
        execute(state, MixWord(1000000261L)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 61 CMP5
        execute(state, MixWord(1000001061L)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }
    }

    "comparing register I6 with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 62 CMP6
        execute(state, MixWord(1000000562L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare the whole word except for the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 62 CMP6
        execute(state, MixWord(1000001362L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 62 CMP6
        execute(state, MixWord(1000000362L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a left-side field not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 62 CMP6
        execute(state, MixWord(1000001162L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 62 CMP6
        execute(state, MixWord(1000003762L)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 62 CMP6
        execute(state, MixWord(1000000262L)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 62 CMP6
        execute(state, MixWord(1000001062L)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }
    }

    "comparing register X with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 63 CMPX
        execute(state, MixWord(1000000563L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare the whole word except for the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 63 CMPX
        execute(state, MixWord(1000001363L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 63 CMPX
        execute(state, MixWord(1000000363L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a left-side field not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 63 CMPX
        execute(state, MixWord(1000001163L)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 63 CMPX
        execute(state, MixWord(1000003763L)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 63 CMPX
        execute(state, MixWord(1000000263L)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 63 CMPX
        execute(state, MixWord(1000001063L)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }
    }
  }
}
