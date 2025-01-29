package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.binary
import org.linnando.mixemulator.vm.exceptions.OverflowException
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class MemorySpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  import binary._

  private val initialState = binary.initialState
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(MixWord(0x061c8240)) // + 6 7 8 9 0
      .updatedX(MixWord(0x461c8240)) // - 6 7 8 9 0
      .updatedI(1, MixIndex(0x0240)) // + 9 0
      .updatedI(2, MixIndex(0x1240)) // - 9 0
      .updatedI(3, MixIndex(0x0240)) // + 9 0
      .updatedI(4, MixIndex(0x0240)) // + 9 0
      .updatedI(5, MixIndex(0x0240)) // + 9 0
      .updatedI(6, MixIndex(0x0240)) // + 9 0
      .updatedJ(MixIndex(0x0bb8)),
    memory = initialState.memory
      .updated(MixIndex(2000), MixWord(0x41403144)) // - 1 16 3 5 4
      .updated(MixIndex(2001), MixWord(0x41083105)) // - 1 2 3 4 5
  )

  "binary loading from memory" when {
    "loading to register A" should {
      "load the whole word" in {
        // A = 2000, I = 0, F = 0:5, C = 8 LDA
        execute(state, MixWord(0x1f400148)) map {
          _.registers.getA mustEqual MixWord(0x41403144)
        }
      }

      "load the whole word except for the sign as a positive number" in {
        // A = 2000, I = 0, F = 1:5, C = 8 LDA
        execute(state, MixWord(0x1f400348)) map {
          _.registers.getA mustEqual MixWord(0x01403144)
        }
      }

      "load a right-hand part as a positive number" in {
        // A = 2000, I = 0, F = 3:5, C = 8 LDA
        execute(state, MixWord(0x1f400748)) map {
          _.registers.getA mustEqual MixWord(0x00003144)
        }
      }

      "load a left-hand part with the sign and shift it right" in {
        // A = 2000, I = 0, F = 0:3, C = 8 LDA
        execute(state, MixWord(0x1f4000c8)).map {
          _.registers.getA mustEqual MixWord(0x40001403)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 8 LDA
        execute(state, MixWord(0x1f400248)) map {
          _.registers.getA mustEqual MixWord(0x00000001)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 8 LDA
        execute(state, MixWord(0x1f400908)) map {
          _.registers.getA mustEqual MixWord(0x00000005)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 8 LDA
        execute(state, MixWord(0x1f400008)) map {
          _.registers.getA mustEqual MixWord(0x40000000)
        }
      }

      "load with negation the whole word" in {
        // A = 2000, I = 0, F = 0:5, C = 16 LDAN
        execute(state, MixWord(0x1f400150)) map {
          _.registers.getA mustEqual MixWord(0x01403144)
        }
      }

      "load with negation the whole word except for the sign as a negative number" in {
        // A = 2000, I = 0, F = 1:5, C = 16 LDAN
        execute(state, MixWord(0x1f400350)) map {
          _.registers.getA mustEqual MixWord(0x41403144)
        }
      }

      "load with negation a right-hand part as a negative number" in {
        // A = 2000, I = 0, F = 3:5, C = 16 LDAN
        execute(state, MixWord(0x1f400750)) map {
          _.registers.getA mustEqual MixWord(0x40003144)
        }
      }

      "load with negation a left-hand part with the sign, shift it right, and invert the sign" in {
        // A = 2000, I = 0, F = 0:3, C = 16 LDAN
        execute(state, MixWord(0x1f4000d0)) map {
          _.registers.getA mustEqual MixWord(0x00001403)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 16 LDAN
        execute(state, MixWord(0x1f400250)) map {
          _.registers.getA mustEqual MixWord(0x40000001)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 16 LDAN
        execute(state, MixWord(0x1f400910)) map {
          _.registers.getA mustEqual MixWord(0x40000005)
        }
      }

      "load with negation the sign only and invert the sign" in {
        // A = 2000, I = 0, F = 0:0, C = 16 LDAN
        execute(state, MixWord(0x1f400010)) map {
          _.registers.getA mustEqual MixWord(0x00000000)
        }
      }
    }

    "loading to register I1" should {
      "load the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 9 LD1
        execute(state, MixWord(0x1f400089)) map {
          _.registers.getI(1) mustEqual MixIndex(0x1050)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 9 LD1
        execute(state, MixWord(0x1f400909)) map {
          _.registers.getI(1) mustEqual MixIndex(0x0005)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 9 LD1
        execute(state, MixWord(0x1f400009)) map {
          _.registers.getI(1) mustEqual MixIndex(0x1000)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 9 LD1
        execute(state, MixWord(0x1f400249)) map {
          _.registers.getI(1) mustEqual MixIndex(0x0001)
        }
      }

      "throw an exception on an attempt to load a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 9 LD1
          execute(state, MixWord(0x1f400149))
        }
      }

      "load with negation the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 17 LD1N
        execute(state, MixWord(0x1f400091)) map {
          _.registers.getI(1) mustEqual MixIndex(0x0050)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 17 LD1N
        execute(state, MixWord(0x1f400911)) map {
          _.registers.getI(1) mustEqual MixIndex(0x1005)
        }
      }

      "load with negation the sign only and invert it" in {
        // A = 2000, I = 0, F = 0:0, C = 17 LD1N
        execute(state, MixWord(0x1f400011)) map {
          _.registers.getI(1) mustEqual MixIndex(0x0000)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 17 LD1N
        execute(state, MixWord(0x1f400251)) map {
          _.registers.getI(1) mustEqual MixIndex(0x1001)
        }
      }

      "throw an exception on an attempt to load with negation a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 17 LD1N
          execute(state, MixWord(0x1f400151))
        }
      }
    }

    "loading to register I2" should {
      "load the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 10 LD2
        execute(state, MixWord(0x1f40008a)) map {
          _.registers.getI(2) mustEqual MixIndex(0x1050)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 10 LD2
        execute(state, MixWord(0x1f40090a)) map {
          _.registers.getI(2) mustEqual MixIndex(0x0005)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 10 LD2
        execute(state, MixWord(0x1f40000a)) map {
          _.registers.getI(2) mustEqual MixIndex(0x1000)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 10 LD2
        execute(state, MixWord(0x1f40024a)) map {
          _.registers.getI(2) mustEqual MixIndex(0x0001)
        }
      }

      "throw an exception on an attempt to load a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 10 LD2
          execute(state, MixWord(0x1f40014a))
        }
      }

      "load with negation the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 18 LD2N
        execute(state, MixWord(0x1f400092)) map {
          _.registers.getI(2) mustEqual MixIndex(0x0050)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 18 LD2N
        execute(state, MixWord(0x1f400912)) map {
          _.registers.getI(2) mustEqual MixIndex(0x1005)
        }
      }

      "load with negation the sign only and invert it" in {
        // A = 2000, I = 0, F = 0:0, C = 18 LD2N
        execute(state, MixWord(0x1f400012)) map {
          _.registers.getI(2) mustEqual MixIndex(0x0000)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 18 LD2N
        execute(state, MixWord(0x1f400252)) map {
          _.registers.getI(2) mustEqual MixIndex(0x1001)
        }
      }

      "throw an exception on an attempt to load with negation a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 18 LD2N
          execute(state, MixWord(0x1f400152))
        }
      }
    }

    "loading to register I3" should {
      "load the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 11 LD3
        execute(state, MixWord(0x1f40008b)) map {
          _.registers.getI(3) mustEqual MixIndex(0x1050)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 11 LD3
        execute(state, MixWord(0x1f40090b)) map {
          _.registers.getI(3) mustEqual MixIndex(0x0005)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 11 LD3
        execute(state, MixWord(0x1f40000b)) map {
          _.registers.getI(3) mustEqual MixIndex(0x1000)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 11 LD3
        execute(state, MixWord(0x1f40024b)) map {
          _.registers.getI(3) mustEqual MixIndex(0x0001)
        }
      }

      "throw an exception on an attempt to load a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 11 LD3
          execute(state, MixWord(0x1f40014b))
        }
      }

      "load with negation the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 19 LD3N
        execute(state, MixWord(0x1f400093)) map {
          _.registers.getI(3) mustEqual MixIndex(0x0050)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 19 LD3N
        execute(state, MixWord(0x1f400913)) map {
          _.registers.getI(3) mustEqual MixIndex(0x1005)
        }
      }

      "load with negation the sign only and invert it" in {
        // A = 2000, I = 0, F = 0:0, C = 19 LD3N
        execute(state, MixWord(0x1f400013)) map {
          _.registers.getI(3) mustEqual MixIndex(0x0000)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 19 LD3N
        execute(state, MixWord(0x1f400253)) map {
          _.registers.getI(3) mustEqual MixIndex(0x1001)
        }
      }

      "throw an exception on an attempt to load with negation a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 19 LD3N
          execute(state, MixWord(0x1f400153))
        }
      }
    }

    "loading to register I4" should {
      "load the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 12 LD4
        execute(state, MixWord(0x1f40008c)) map {
          _.registers.getI(4) mustEqual MixIndex(0x1050)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 12 LD4
        execute(state, MixWord(0x1f40090c)) map {
          _.registers.getI(4) mustEqual MixIndex(0x0005)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 12 LD4
        execute(state, MixWord(0x1f40000c)) map {
          _.registers.getI(4) mustEqual MixIndex(0x1000)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 12 LD4
        execute(state, MixWord(0x1f40024c)) map {
          _.registers.getI(4) mustEqual MixIndex(0x0001)
        }
      }

      "throw an exception on an attempt to load a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 12 LD4
          execute(state, MixWord(0x1f40014c))
        }
      }

      "load with negation the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 20 LD4N
        execute(state, MixWord(0x1f400094)) map {
          _.registers.getI(4) mustEqual MixIndex(0x0050)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 20 LD4N
        execute(state, MixWord(0x1f400914)) map {
          _.registers.getI(4) mustEqual MixIndex(0x1005)
        }
      }

      "load with negation the sign only and invert it" in {
        // A = 2000, I = 0, F = 0:0, C = 20 LD4N
        execute(state, MixWord(0x1f400014)) map {
          _.registers.getI(4) mustEqual MixIndex(0x0000)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 20 LD4N
        execute(state, MixWord(0x1f400254)) map {
          _.registers.getI(4) mustEqual MixIndex(0x1001)
        }
      }

      "throw an exception on an attempt to load with negation a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 20 LD4N
          execute(state, MixWord(0x1f400154))
        }
      }
    }

    "loading to register I5" should {
      "load the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 13 LD5
        execute(state, MixWord(0x1f40008d)) map {
          _.registers.getI(5) mustEqual MixIndex(0x1050)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 13 LD5
        execute(state, MixWord(0x1f40090d)) map {
          _.registers.getI(5) mustEqual MixIndex(0x0005)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 13 LD5
        execute(state, MixWord(0x1f40000d)) map {
          _.registers.getI(5) mustEqual MixIndex(0x1000)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 13 LD5
        execute(state, MixWord(0x1f40024d)) map {
          _.registers.getI(5) mustEqual MixIndex(0x0001)
        }
      }

      "throw an exception on an attempt to load a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 13 LD5
          execute(state, MixWord(0x1f40014d))
        }
      }

      "load with negation the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 21 LD5N
        execute(state, MixWord(0x1f400095)) map {
          _.registers.getI(5) mustEqual MixIndex(0x0050)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 21 LD5N
        execute(state, MixWord(0x1f400915)) map {
          _.registers.getI(5) mustEqual MixIndex(0x1005)
        }
      }

      "load with negation the sign only and invert it" in {
        // A = 2000, I = 0, F = 0:0, C = 21 LD5N
        execute(state, MixWord(0x1f400015)) map {
          _.registers.getI(5) mustEqual MixIndex(0x0000)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 21 LD5N
        execute(state, MixWord(0x1f400255)) map {
          _.registers.getI(5) mustEqual MixIndex(0x1001)
        }
      }

      "throw an exception on an attempt to load with negation a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 21 LD5N
          execute(state, MixWord(0x1f400155))
        }
      }
    }

    "loading to register I6" should {
      "load the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 14 LD6
        execute(state, MixWord(0x1f40008e)) map {
          _.registers.getI(6) mustEqual MixIndex(0x1050)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 14 LD6
        execute(state, MixWord(0x1f40090e)) map {
          _.registers.getI(6) mustEqual MixIndex(0x0005)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 14 LD6
        execute(state, MixWord(0x1f40000e)) map {
          _.registers.getI(6) mustEqual MixIndex(0x1000)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 14 LD6
        execute(state, MixWord(0x1f40024e)) map {
          _.registers.getI(6) mustEqual MixIndex(0x0001)
        }
      }

      "throw an exception on an attempt to load a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 14 LD6
          execute(state, MixWord(0x1f40014e))
        }
      }

      "load with negation the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 22 LD6N
        execute(state, MixWord(0x1f400096)) map {
          _.registers.getI(6) mustEqual MixIndex(0x0050)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 22 LD6N
        execute(state, MixWord(0x1f400916)) map {
          _.registers.getI(6) mustEqual MixIndex(0x1005)
        }
      }

      "load with negation the sign only and invert it" in {
        // A = 2000, I = 0, F = 0:0, C = 22 LD6N
        execute(state, MixWord(0x1f400016)) map {
          _.registers.getI(6) mustEqual MixIndex(0x0000)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 22 LD6N
        execute(state, MixWord(0x1f400256)) map {
          _.registers.getI(6) mustEqual MixIndex(0x1001)
        }
      }

      "throw an exception on an attempt to load with negation a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 22 LD6N
          execute(state, MixWord(0x1f400156))
        }
      }
    }

    "loading to register X" should {
      "load the whole word" in {
        // A = 2000, I = 0, F = 0:5, C = 15 LDX
        execute(state, MixWord(0x1f40014f)) map {
          _.registers.getX mustEqual MixWord(0x41403144)
        }
      }

      "load the whole word except for the sign as a positive number" in {
        // A = 2000, I = 0, F = 1:5, C = 15 LDX
        execute(state, MixWord(0x1f40034f)) map {
          _.registers.getX mustEqual MixWord(0x01403144)
        }
      }

      "load a right-hand part as a positive number" in {
        // A = 2000, I = 0, F = 3:5, C = 15 LDX
        execute(state, MixWord(0x1f40074f)) map {
          _.registers.getX mustEqual MixWord(0x00003144)
        }
      }

      "load a left-hand part with a sign and shift it right" in {
        // A = 2000, I = 0, F = 0:3, C = 15 LDX
        execute(state, MixWord(0x1f4000cf)) map {
          _.registers.getX mustEqual MixWord(0x40001403)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 15 LDX
        execute(state, MixWord(0x1f40024f)) map {
          _.registers.getX mustEqual MixWord(0x00000001)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 15 LDX
        execute(state, MixWord(0x1f40090f)) map {
          _.registers.getX mustEqual MixWord(0x00000005)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 15 LDX
        execute(state, MixWord(0x1f40000f)) map {
          _.registers.getX mustEqual MixWord(0x40000000)
        }
      }

      "load with negation the whole word" in {
        // A = 2000, I = 0, F = 0:5, C = 23 LDXN
        execute(state, MixWord(0x1f400157)) map {
          _.registers.getX mustEqual MixWord(0x01403144)
        }
      }

      "load with negation the whole word except for the sign as a negative number" in {
        // A = 2000, I = 0, F = 1:5, C = 23 LDXN
        execute(state, MixWord(0x1f400357)) map {
          _.registers.getX mustEqual MixWord(0x41403144)
        }
      }

      "load with negation a right-hand part as a negative number" in {
        // A = 2000, I = 0, F = 3:5, C = 23 LDXN
        execute(state, MixWord(0x1f400757)) map {
          _.registers.getX mustEqual MixWord(0x40003144)
        }
      }

      "load with negation a left-hand part with the sign, shift it right, and invert the sign" in {
        // A = 2000, I = 0, F = 0:3, C = 23 LDXN
        execute(state, MixWord(0x1f4000d7)) map {
          _.registers.getX mustEqual MixWord(0x00001403)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 23 LDXN
        execute(state, MixWord(0x1f400257)) map {
          _.registers.getX mustEqual MixWord(0x40000001)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 23 LDXN
        execute(state, MixWord(0x1f400917)) map {
          _.registers.getX mustEqual MixWord(0x40000005)
        }
      }

      "load with negation the sign only and invert it" in {
        // A = 2000, I = 0, F = 0:0, C = 23 LDXN
        execute(state, MixWord(0x1f400017)) map {
          _.registers.getX mustEqual MixWord(0x00000000)
        }
      }
    }
  }

  "binary storing to memory" when {
    "storing from register A" should {
      "replace the whole content of a memory cell" in {
        // A = 2001, I = 0, F = 0:5, C = 24 STA
        execute(state, MixWord(0x1f440158)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x061c8240)
        }
      }

      "replace the whole content of a memory cell except for the sign" in {
        // A = 2001, I = 0, F = 1:5, C = 24 STA
        execute(state, MixWord(0x1f440358)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x461c8240)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 24 STA
        execute(state, MixWord(0x1f440b58)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41083100)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 24 STA
        execute(state, MixWord(0x1f440498)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41003105)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 24 STA
        execute(state, MixWord(0x1f4404d8)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41240105)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 24 STA
        execute(state, MixWord(0x1f440058)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x00083105)
        }
      }
    }

    "storing from register I1" should {
      "replace the whole content of a memory cell, filling lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 0:5, C = 25 ST1
        execute(state, MixWord(0x1f440159)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x00000240)
        }
      }

      "replace the whole content of a memory cell except for the sign, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 1:5, C = 25 ST1
        execute(state, MixWord(0x1f440359)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x40000240)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 25 ST1
        execute(state, MixWord(0x1f440b59)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41083100)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 25 ST1
        execute(state, MixWord(0x1f440499)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41003105)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 25 ST1
        execute(state, MixWord(0x1f4404d9)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41240105)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 25 ST1
        execute(state, MixWord(0x1f440059)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x00083105)
        }
      }
    }

    "storing from register I2" should {
      "replace the whole content of a memory cell, filling lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 0:5, C = 26 ST2
        execute(state, MixWord(0x1f44015a)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x40000240)
        }
      }

      "replace the whole content of a memory cell except for the sign, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 1:5, C = 26 ST2
        execute(state, MixWord(0x1f44035a)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x40000240)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 26 ST2
        execute(state, MixWord(0x1f440b5a)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41083100)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 26 ST2
        execute(state, MixWord(0x1f44049a)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41003105)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 26 ST2
        execute(state, MixWord(0x1f4404da)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41240105)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 26 ST2
        execute(state, MixWord(0x1f44005a)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x40083105)
        }
      }
    }

    "storing from register I3" should {
      "replace the whole content of a memory cell, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 0:5, C = 27 ST3
        execute(state, MixWord(0x1f44015b)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x00000240)
        }
      }

      "replace the whole content of a memory cell except for the sign, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 1:5, C = 27 ST3
        execute(state, MixWord(0x1f44035b)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x40000240)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 27 ST3
        execute(state, MixWord(0x1f440b5b)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41083100)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 27 ST3
        execute(state, MixWord(0x1f44049b)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41003105)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 27 ST3
        execute(state, MixWord(0x1f4404db)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41240105)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 27 ST3
        execute(state, MixWord(0x1f44005b)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x00083105)
        }
      }
    }

    "storing from register I4" should {
      "replace the whole content of a memory cell, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 0:5, C = 28 ST4
        execute(state, MixWord(0x1f44015c)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x00000240)
        }
      }

      "replace the whole content of a memory cell except for the sign, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 1:5, C = 28 ST4
        execute(state, MixWord(0x1f44035c)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x40000240)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 28 ST4
        execute(state, MixWord(0x1f440b5c)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41083100)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 28 ST4
        execute(state, MixWord(0x1f44049c)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41003105)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 28 ST4
        execute(state, MixWord(0x1f4404dc)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41240105)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 28 ST4
        execute(state, MixWord(0x1f44005c)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x00083105)
        }
      }
    }

    "storing from register I5" should {
      "replace the whole content of a memory cell, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 0:5, C = 29 ST5
        execute(state, MixWord(0x1f44015d)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x00000240)
        }
      }

      "replace the whole content of a memory cell except for the sign, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 1:5, C = 29 ST5
        execute(state, MixWord(0x1f44035d)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x40000240)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 29 ST5
        execute(state, MixWord(0x1f440b5d)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41083100)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 29 ST5
        execute(state, MixWord(0x1f44049d)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41003105)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 29 ST5
        execute(state, MixWord(0x1f4404dd)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41240105)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 29 ST5
        execute(state, MixWord(0x1f44005d)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x00083105)
        }
      }
    }

    "storing from register I6" should {
      "replace the whole content of a memory cell, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 0:5, C = 30 ST6
        execute(state, MixWord(0x1f44015e)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x00000240)
        }
      }

      "replace the whole content of a memory cell except for the sign, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 1:5, C = 30 ST6
        execute(state, MixWord(0x1f44035e)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x40000240)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 30 ST6
        execute(state, MixWord(0x1f440b5e)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41083100)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 30 ST6
        execute(state, MixWord(0x1f44049e)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41003105)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 30 ST6
        execute(state, MixWord(0x1f4404de)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41240105)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 30 ST6
        execute(state, MixWord(0x1f44005e)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x00083105)
        }
      }
    }

    "storing from register X" should {
      "replace the whole content of a memory cell" in {
        // A = 2001, I = 0, F = 0:5, C = 31 STX
        execute(state, MixWord(0x1f44015f)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x461c8240)
        }
      }

      "replace the whole content of a memory cell except for the sign" in {
        // A = 2001, I = 0, F = 1:5, C = 31 STX
        execute(state, MixWord(0x1f44035f)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x461c8240)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 31 STX
        execute(state, MixWord(0x1f440b5f)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41083100)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 31 STX
        execute(state, MixWord(0x1f44049f)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41003105)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 31 STX
        execute(state, MixWord(0x1f4404df)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41240105)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 31 STX
        execute(state, MixWord(0x1f44005f)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x40083105)
        }
      }
    }

    "storing from register J" should {
      "replace the whole content of a memory cell, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 0:5, C = 32 STJ
        execute(state, MixWord(0x1f440160)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x00000bb8)
        }
      }

      "replace the whole content of a memory cell except for the sign, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 1:5, C = 32 STJ
        execute(state, MixWord(0x1f440360)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x40000bb8)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 32 STJ
        execute(state, MixWord(0x1f440b60)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41083138)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 32 STJ
        execute(state, MixWord(0x1f4404a0)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41e03105)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 32 STJ
        execute(state, MixWord(0x1f4404e0)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41bb8105)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 32 STJ
        execute(state, MixWord(0x1f440060)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x38083105)
        }
      }
    }

    "storing zero" should {
      "clear the whole content of a memory cell" in {
        // A = 2001, I = 0, F = 0:5, C = 33 STZ
        execute(state, MixWord(0x1f440161)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x00000000)
        }
      }

      "clear the whole content of a memory cell except for the sign" in {
        // A = 2001, I = 0, F = 1:5, C = 33 STZ
        execute(state, MixWord(0x1f440361)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x40000000)
        }
      }

      "clear only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 33 STZ
        execute(state, MixWord(0x1f440b61)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41083100)
        }
      }

      "clear only a middle part of a memory cell" in {
        // A = 2001, I = 0, F = 2:3, C = 33 STZ
        execute(state, MixWord(0x1f4404e1)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x41000105)
        }
      }

      "clear only a left-hand part with the sign of a memory cell" in {
        // A = 2001, I = 0, F = 0:1, C = 33 STZ
        execute(state, MixWord(0x1f440061)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x00083105)
        }
      }
    }
  }

  "binary moving in memory" should {
    "move memory words" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, MixIndex(3000)),
        memory = initialState.memory
          .updated(MixIndex(2000), MixWord(0x00000001))
          .updated(MixIndex(2001), MixWord(0x00000002))
          .updated(MixIndex(2002), MixWord(0x00000003))
      )
      // A = 2000, I = 0, F = 3, C = 7 MOVE
      execute(prevState, MixWord(0x1f4000c7)).map(s => {
        s.registers.getI(1) mustEqual MixIndex(3003)
        s.memory.get(3000.toShort) mustEqual MixWord(0x00000001)
        s.memory.get(3001.toShort) mustEqual MixWord(0x00000002)
        s.memory.get(3002.toShort) mustEqual MixWord(0x00000003)
      })
    }

    "do nothing when the number of words to move is zero" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, MixIndex(3000)),
        memory = initialState.memory
          .updated(MixIndex(2000), MixWord(0x00000001))
          .updated(MixIndex(2001), MixWord(0x00000002))
          .updated(MixIndex(2002), MixWord(0x00000003))
      )
      // A = 2000, I = 0, F = 0, C = 7 MOVE
      execute(prevState, MixWord(0x1f400007)).map(s => {
        s.registers.getI(1) mustEqual MixIndex(3000)
        s.memory.get(3000.toShort) mustEqual MixWord(0x00000000)
        s.memory.get(3001.toShort) mustEqual MixWord(0x00000000)
        s.memory.get(3002.toShort) mustEqual MixWord(0x00000000)
      })
    }

    "move overlapping ranges in the downward direction" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, MixIndex(1999)),
        memory = initialState.memory
          .updated(MixIndex(2000), MixWord(0x00000001))
          .updated(MixIndex(2001), MixWord(0x00000002))
          .updated(MixIndex(2002), MixWord(0x00000003))
      )
      // A = 2000, I = 0, F = 3, C = 7 MOVE
      execute(prevState, MixWord(0x1f4000c7)).map(s => {
        s.registers.getI(1) mustEqual MixIndex(2002)
        s.memory.get(1999.toShort) mustEqual MixWord(0x00000001)
        s.memory.get(2000.toShort) mustEqual MixWord(0x00000002)
        s.memory.get(2001.toShort) mustEqual MixWord(0x00000003)
        s.memory.get(2002.toShort) mustEqual MixWord(0x00000003)
      })
    }

    "move overlapping ranges in the upward direction" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, MixIndex(2001)),
        memory = initialState.memory
          .updated(MixIndex(2000), MixWord(0x00000001))
          .updated(MixIndex(2001), MixWord(0x00000002))
          .updated(MixIndex(2002), MixWord(0x00000003))
      )
      // A = 2000, I = 0, F = 3, C = 7 MOVE
      execute(prevState, MixWord(0x1f4000c7)).map(s => {
        s.registers.getI(1) mustEqual MixIndex(2004)
        s.memory.get(2000.toShort) mustEqual MixWord(0x00000001)
        s.memory.get(2001.toShort) mustEqual MixWord(0x00000001)
        s.memory.get(2002.toShort) mustEqual MixWord(0x00000001)
        s.memory.get(2003.toShort) mustEqual MixWord(0x00000001)
      })
    }
  }
}
