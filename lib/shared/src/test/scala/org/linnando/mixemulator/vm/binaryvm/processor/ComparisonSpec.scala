package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.{Comparison, binary}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class ComparisonSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  import binary._

  private val initialState = binary.initialState
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(MixWord(0x53480009)) // - 19 18 0 0 9
      .updatedX(MixWord(0x000027a0)) // + 0 0 2 30 32
      .updatedI(1, MixIndex(0x1ee5)) // - 59 37
      .updatedI(2, MixIndex(0x00d3)) // + 3 19
      .updatedI(3, MixIndex(0x07aa)) // + 30 42
      .updatedI(4, MixIndex(0x1eda)) // - 59 26
      .updatedI(5, MixIndex(0x0239)) // + 8 57
      .updatedI(6, MixIndex(0x132e)), // - 12 46
    memory = initialState.memory
      .updated(MixIndex(1000), MixWord(0x40002580)) // - 0 0 2 22 0
  )

  "binary comparison module" when {
    "comparing register A with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 56 CMPA
        binary.execute(state, MixWord(0x0fa00178)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare the whole word except for the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 56 CMPA
        binary.execute(state, MixWord(0x0fa00378)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 56 CMPA
        binary.execute(state, MixWord(0x0fa000f8)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a left-side field not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 56 CMPA
        binary.execute(state, MixWord(0x0fa002f8)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 56 CMPA
        binary.execute(state, MixWord(0x0fa00978)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 56 CMPA
        binary.execute(state, MixWord(0x0fa000b8)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 56 CMPA
        binary.execute(state, MixWord(0x0fa002b8)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }
    }

    "comparing register I1 with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 57 CMP1
        binary.execute(state, MixWord(0x0fa00179)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare the whole word without the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 57 CMP1
        binary.execute(state, MixWord(0x0fa00379)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 57 CMP1
        binary.execute(state, MixWord(0x0fa000f9)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a left-side field not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 57 CMP1
        binary.execute(state, MixWord(0x0fa002f9)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 57 CMP1
        binary.execute(state, MixWord(0x0fa00979)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 57 CMP1
        binary.execute(state, MixWord(0x0fa000b9)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 57 CMP1
        binary.execute(state, MixWord(0x0fa002b9)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }
    }

    "comparing register I2 with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 58 CMP2
        binary.execute(state, MixWord(0x0fa0017a)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare the whole word except for the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 58 CMP2
        binary.execute(state, MixWord(0x0fa0037a)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 58 CMP2
        binary.execute(state, MixWord(0x0fa000fa)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a left-side field not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 58 CMP2
        binary.execute(state, MixWord(0x0fa002fa)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 58 CMP2
        binary.execute(state, MixWord(0x0fa0097a)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 58 CMP2
        binary.execute(state, MixWord(0x0fa000ba)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 58 CMP2
        binary.execute(state, MixWord(0x0fa002ba)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }
    }

    "comparing register I3 with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 59 CMP3
        binary.execute(state, MixWord(0x0fa0017b)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare the whole word except for the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 59 CMP3
        binary.execute(state, MixWord(0x0fa0037b)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 59 CMP3
        binary.execute(state, MixWord(0x0fa000fb)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a left-side field not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 59 CMP3
        binary.execute(state, MixWord(0x0fa002fb)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 59 CMP3
        binary.execute(state, MixWord(0x0fa0097b)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 59 CMP3
        binary.execute(state, MixWord(0x0fa000bb)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 59 CMP3
        binary.execute(state, MixWord(0x0fa002bb)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }
    }

    "comparing register I4 with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 60 CMP4
        binary.execute(state, MixWord(0x0fa0017c)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare the whole word except for the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 60 CMP4
        binary.execute(state, MixWord(0x0fa0037c)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 60 CMP4
        binary.execute(state, MixWord(0x0fa000fc)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a left-side field not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 60 CMP4
        binary.execute(state, MixWord(0x0fa002fc)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 60 CMP4
        binary.execute(state, MixWord(0x0fa0097c)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 60 CMP4
        binary.execute(state, MixWord(0x0fa000bc)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 60 CMP4
        binary.execute(state, MixWord(0x0fa002bc)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }
    }

    "comparing register I5 with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 61 CMP5
        binary.execute(state, MixWord(0x0fa0017d)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare the whole word except for the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 61 CMP5
        binary.execute(state, MixWord(0x0fa0037d)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 61 CMP5
        binary.execute(state, MixWord(0x0fa000fd)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a left-side fields not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 61 CMP5
        binary.execute(state, MixWord(0x0fa002fd)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 61 CMP5
        binary.execute(state, MixWord(0x0fa0097d)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 61 CMP5
        binary.execute(state, MixWord(0x0fa000bd)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 61 CMP5
        binary.execute(state, MixWord(0x0fa002bd)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }
    }

    "comparing register I6 with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 62 CMP6
        binary.execute(state, MixWord(0x0fa0017e)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare the whole word except for the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 62 CMP6
        binary.execute(state, MixWord(0x0fa0037e)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 62 CMP6
        binary.execute(state, MixWord(0x0fa000fe)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a left-side field not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 62 CMP6
        binary.execute(state, MixWord(0x0fa002fe)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 62 CMP6
        binary.execute(state, MixWord(0x0fa0097e)) map {
          _.registers.getCMP mustEqual Comparison.LESS
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 62 CMP6
        binary.execute(state, MixWord(0x0fa000be)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 62 CMP6
        binary.execute(state, MixWord(0x0fa002be)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }
    }

    "comparing register X with memory" should {
      "compare the whole word" in {
        // A = 1000, I = 0, F = 0:5, C = 63 CMPX
        binary.execute(state, MixWord(0x0fa0017f)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare the whole word except for the sign" in {
        // A = 1000, I = 0, F = 1:5, C = 63 CMPX
        binary.execute(state, MixWord(0x0fa0037f)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a field including the sign" in {
        // A = 1000, I = 0, F = 0:3, C = 63 CMPX
        binary.execute(state, MixWord(0x0fa000ff)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a left-side field not including the sign" in {
        // A = 1000, I = 0, F = 1:3, C = 63 CMPX
        binary.execute(state, MixWord(0x0fa002ff)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a right-side field not including the sign" in {
        // A = 1000, I = 0, F = 4:5, C = 63 CMPX
        binary.execute(state, MixWord(0x0fa0097f)) map {
          _.registers.getCMP mustEqual Comparison.GREATER
        }
      }

      "compare a field including the sign to zero" in {
        // A = 1000, I = 0, F = 0:2, C = 63 CMPX
        binary.execute(state, MixWord(0x0fa000bf)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }

      "compare a field not including the sign to zero" in {
        // A = 1000, I = 0, F = 1:2, C = 63 CMPX
        binary.execute(state, MixWord(0x0fa002bf)) map {
          _.registers.getCMP mustEqual Comparison.EQUAL
        }
      }
    }
  }
}
