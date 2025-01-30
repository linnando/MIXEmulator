package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.exceptions.OverflowException
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class MemorySpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  import decimal._

  private val initialState = decimal.initialState
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(MixWord(607080900L)) // + 6 7 8 9 0
      .updatedX(MixWord(0x400000000L | 607080900L)) // - 6 7 8 9 0
      .updatedI(1, MixIndex(900)) // + 9 0
      .updatedI(2, MixIndex((0x4000 | 900).toShort)) // - 9 0
      .updatedI(3, MixIndex(900)) // + 9 0
      .updatedI(4, MixIndex(900)) // + 9 0
      .updatedI(5, MixIndex(900)) // + 9 0
      .updatedI(6, MixIndex(900)) // + 9 0
      .updatedJ(MixIndex(3001)),
    memory = initialState.memory
      .updated(MixIndex(2000), MixWord(0x400000000L | 116030504L)) // - 1 16 3 5 4
      .updated(MixIndex(2001), MixWord(0x400000000L | 102030405L)) // - 1 2 3 4 5
  )

  "decimal loading from memory" when {
    "loading to register A" should {
      "load the whole word" in {
        // A = 2000, I = 0, F = 0:5, C = 8 LDA
        decimal.execute(state, MixWord(2000000508L)) map {
          _.registers.getA mustEqual MixWord(0x400000000L | 116030504L)
        }
      }

      "load the whole word except for the sign as a positive number" in {
        // A = 2000, I = 0, F = 1:5, C = 8 LDA
        decimal.execute(state, MixWord(2000001308L)) map {
          _.registers.getA mustEqual MixWord(116030504L)
        }
      }

      "load a right-hand part as a positive number" in {
        // A = 2000, I = 0, F = 3:5, C = 8 LDA
        decimal.execute(state, MixWord(2000002908L)) map {
          _.registers.getA mustEqual MixWord(30504L)
        }
      }

      "load a left-hand part with the sign and shift it right" in {
        // A = 2000, I = 0, F = 0:3, C = 8 LDA
        decimal.execute(state, MixWord(2000000308L)) map {
          _.registers.getA mustEqual MixWord(0x400000000L | 11603L)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 8 LDA
        decimal.execute(state, MixWord(2000000908L)) map {
          _.registers.getA mustEqual MixWord(1L)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 8 LDA
        decimal.execute(state, MixWord(2000003608L)) map {
          _.registers.getA mustEqual MixWord(5L)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 8 LDA
        decimal.execute(state, MixWord(2000000008L)) map {
          _.registers.getA mustEqual MixWord(0x400000000L)
        }
      }

      "load with negation the whole word" in {
        // A = 2000, I = 0, F = 0:5, C = 16 LDAN
        decimal.execute(state, MixWord(2000000516L)) map {
          _.registers.getA mustEqual MixWord(116030504L)
        }
      }

      "load with negation the whole word except for the sign as a negative number" in {
        // A = 2000, I = 0, F = 1:5, C = 16 LDAN
        decimal.execute(state, MixWord(2000001316L)) map {
          _.registers.getA mustEqual MixWord(0x400000000L | 116030504L)
        }
      }

      "load with negation a right-hand part as a negative number" in {
        // A = 2000, I = 0, F = 3:5, C = 16 LDAN
        decimal.execute(state, MixWord(2000002916L)) map {
          _.registers.getA mustEqual MixWord(0x400000000L | 30504L)
        }
      }

      "load with negation a left-hand part with the sign, shift it right, and invert the sign" in {
        // A = 2000, I = 0, F = 0:3, C = 16 LDAN
        decimal.execute(state, MixWord(2000000316L)) map {
          _.registers.getA mustEqual MixWord(11603L)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 16 LDAN
        decimal.execute(state, MixWord(2000000916L)) map {
          _.registers.getA mustEqual MixWord(0x400000001L)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 16 LDAN
        decimal.execute(state, MixWord(2000003616L)) map {
          _.registers.getA mustEqual MixWord(0x400000005L)
        }
      }

      "load with negation the sign only and invert the sign" in {
        // A = 2000, I = 0, F = 0:0, C = 16 LDAN
        decimal.execute(state, MixWord(2000000016L)) map {
          _.registers.getA mustEqual MixWord(0L)
        }
      }
    }

    "loading to register I1" should {
      "load the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 9 LD1
        decimal.execute(state, MixWord(2000000209L)) map {
          _.registers.getI(1) mustEqual MixIndex((0x4000 | 116).toShort)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 9 LD1
        decimal.execute(state, MixWord(2000003609L)) map {
          _.registers.getI(1) mustEqual MixIndex(5)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 9 LD1
        decimal.execute(state, MixWord(2000000009L)) map {
          _.registers.getI(1) mustEqual MixIndex(0x4000)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 9 LD1
        decimal.execute(state, MixWord(2000000909L)) map {
          _.registers.getI(1) mustEqual MixIndex(1)
        }
      }

      "throw an exception on an attempt to load a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 9 LD1
          decimal.execute(state, MixWord(2000000509L))
        }
      }

      "load with negation the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 17 LD1N
        decimal.execute(state, MixWord(2000000217L)) map {
          _.registers.getI(1) mustEqual MixIndex(116)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 17 LD1N
        decimal.execute(state, MixWord(2000003617L)) map {
          _.registers.getI(1) mustEqual MixIndex(0x4005)
        }
      }

      "load with negation the sign only and invert it" in {
        // A = 2000, I = 0, F = 0:0, C = 17 LD1N
        decimal.execute(state, MixWord(2000000017L)) map {
          _.registers.getI(1) mustEqual MixIndex(0)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 17 LD1N
        decimal.execute(state, MixWord(2000000917L)) map {
          _.registers.getI(1) mustEqual MixIndex(0x4001)
        }
      }

      "throw an exception on an attempt to load with negation a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 17 LD1N
          decimal.execute(state, MixWord(2000000517L))
        }
      }
    }

    "loading to register I2" should {
      "load the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 10 LD2
        decimal.execute(state, MixWord(2000000210L)) map {
          _.registers.getI(2) mustEqual MixIndex((0x4000 | 116).toShort)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 10 LD2
        decimal.execute(state, MixWord(2000003610L)) map {
          _.registers.getI(2) mustEqual MixIndex(5)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 10 LD2
        decimal.execute(state, MixWord(2000000010L)) map {
          _.registers.getI(2) mustEqual MixIndex(0x4000)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 10 LD2
        decimal.execute(state, MixWord(2000000910L)) map {
          _.registers.getI(2) mustEqual MixIndex(1)
        }
      }

      "throw an exception on an attempt to load a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 10 LD2
          decimal.execute(state, MixWord(2000000510L))
        }
      }

      "load with negation the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 18 LD2N
        decimal.execute(state, MixWord(2000000218L)) map {
          _.registers.getI(2) mustEqual MixIndex(116)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 18 LD2N
        decimal.execute(state, MixWord(2000003618L)) map {
          _.registers.getI(2) mustEqual MixIndex(0x4005)
        }
      }

      "load with negation the sign only and invert it" in {
        // A = 2000, I = 0, F = 0:0, C = 18 LD2N
        decimal.execute(state, MixWord(2000000018L)) map {
          _.registers.getI(2) mustEqual MixIndex(0)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 18 LD2N
        decimal.execute(state, MixWord(2000000918L)) map {
          _.registers.getI(2) mustEqual MixIndex(0x4001)
        }
      }

      "throw an exception on an attempt to load with negation a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 18 LD2N
          decimal.execute(state, MixWord(2000000518L))
        }
      }
    }

    "loading to register I3" should {
      "load the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 11 LD3
        decimal.execute(state, MixWord(2000000211L)) map {
          _.registers.getI(3) mustEqual MixIndex((0x4000 | 116).toShort)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 11 LD3
        decimal.execute(state, MixWord(2000003611L)) map {
          _.registers.getI(3) mustEqual MixIndex(5)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 11 LD3
        decimal.execute(state, MixWord(2000000011L)) map {
          _.registers.getI(3) mustEqual MixIndex(0x4000)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 11 LD3
        decimal.execute(state, MixWord(2000000911L)) map {
          _.registers.getI(3) mustEqual MixIndex(1)
        }
      }

      "throw an exception on an attempt to load a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 11 LD3
          decimal.execute(state, MixWord(2000000511L))
        }
      }

      "load with negation the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 19 LD3N
        decimal.execute(state, MixWord(2000000219L)) map {
          _.registers.getI(3) mustEqual MixIndex(116)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 19 LD3N
        decimal.execute(state, MixWord(2000003619L)) map {
          _.registers.getI(3) mustEqual MixIndex(0x4005)
        }
      }

      "load with negation the sign only and invert it" in {
        // A = 2000, I = 0, F = 0:0, C = 19 LD3N
        decimal.execute(state, MixWord(2000000019L)) map {
          _.registers.getI(3) mustEqual MixIndex(0)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 19 LD3N
        decimal.execute(state, MixWord(2000000919L)) map {
          _.registers.getI(3) mustEqual MixIndex(0x4001)
        }
      }

      "throw an exception on an attempt to load with negation a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 19 LD3N
          decimal.execute(state, MixWord(2000000519L))
        }
      }
    }

    "loading to register I4" should {
      "load the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 12 LD4
        decimal.execute(state, MixWord(2000000212L)) map {
          _.registers.getI(4) mustEqual MixIndex((0x4000 | 116).toShort)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 12 LD4
        decimal.execute(state, MixWord(2000003612L)) map {
          _.registers.getI(4) mustEqual MixIndex(5)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 12 LD4
        decimal.execute(state, MixWord(2000000012L)) map {
          _.registers.getI(4) mustEqual MixIndex(0x4000)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 12 LD4
        decimal.execute(state, MixWord(2000000912L)) map {
          _.registers.getI(4) mustEqual MixIndex(1)
        }
      }

      "throw an exception on an attempt to load a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 12 LD4
          decimal.execute(state, MixWord(2000000512L))
        }
      }

      "load with negation the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 20 LD4N
        decimal.execute(state, MixWord(2000000220L)) map {
          _.registers.getI(4) mustEqual MixIndex(116)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 20 LD4N
        decimal.execute(state, MixWord(2000003620L)) map {
          _.registers.getI(4) mustEqual MixIndex(0x4005)
        }
      }

      "load with negation the sign only and invert it" in {
        // A = 2000, I = 0, F = 0:0, C = 20 LD4N
        decimal.execute(state, MixWord(2000000020L)) map {
          _.registers.getI(4) mustEqual MixIndex(0)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 20 LD4N
        decimal.execute(state, MixWord(2000000920L)) map {
          _.registers.getI(4) mustEqual MixIndex(0x4001)
        }
      }

      "throw an exception on an attempt to load with negation a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 20 LD4N
          decimal.execute(state, MixWord(2000000520L))
        }
      }
    }

    "loading to register I5" should {
      "load the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 13 LD5
        decimal.execute(state, MixWord(2000000213L)) map {
          _.registers.getI(5) mustEqual MixIndex((0x4000 | 116).toShort)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 13 LD5
        decimal.execute(state, MixWord(2000003613L)) map {
          _.registers.getI(5) mustEqual MixIndex(5)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 13 LD5
        decimal.execute(state, MixWord(2000000013L)) map {
          _.registers.getI(5) mustEqual MixIndex(0x4000)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 13 LD5
        decimal.execute(state, MixWord(2000000913L)) map {
          _.registers.getI(5) mustEqual MixIndex(1)
        }
      }

      "throw an exception on an attempt to load a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 13 LD5
          decimal.execute(state, MixWord(2000000513L))
        }
      }

      "load with negation the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 21 LD5N
        decimal.execute(state, MixWord(2000000221L)) map {
          _.registers.getI(5) mustEqual MixIndex(116)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 21 LD5N
        decimal.execute(state, MixWord(2000003621L)) map {
          _.registers.getI(5) mustEqual MixIndex(0x4005)
        }
      }

      "load with negation the sign only and invert it" in {
        // A = 2000, I = 0, F = 0:0, C = 21 LD5N
        decimal.execute(state, MixWord(2000000021L)) map {
          _.registers.getI(5) mustEqual MixIndex(0)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 21 LD5N
        decimal.execute(state, MixWord(2000000921L)) map {
          _.registers.getI(5) mustEqual MixIndex(0x4001)
        }
      }

      "throw an exception on an attempt to load with negation a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 21 LD5N
          decimal.execute(state, MixWord(2000000521L))
        }
      }
    }

    "loading to register I6" should {
      "load the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 14 LD6
        decimal.execute(state, MixWord(2000000214L)) map {
          _.registers.getI(6) mustEqual MixIndex((0x4000 | 116).toShort)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 14 LD6
        decimal.execute(state, MixWord(2000003614L)) map {
          _.registers.getI(6) mustEqual MixIndex(5)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 14 LD6
        decimal.execute(state, MixWord(2000000014L)) map {
          _.registers.getI(6) mustEqual MixIndex(0x4000)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 14 LD6
        decimal.execute(state, MixWord(2000000914L)) map {
          _.registers.getI(6) mustEqual MixIndex(1)
        }
      }

      "throw an exception on an attempt to load a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 14 LD6
          decimal.execute(state, MixWord(2000000514L))
        }
      }

      "load with negation the left-hand part of an index register size" in {
        // A = 2000, I = 0, F = 0:2, C = 22 LD6N
        decimal.execute(state, MixWord(2000000222L)) map {
          _.registers.getI(6) mustEqual MixIndex(116)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 22 LD6N
        decimal.execute(state, MixWord(2000003622L)) map {
          _.registers.getI(6) mustEqual MixIndex(0x4005)
        }
      }

      "load with negation the sign only and invert it" in {
        // A = 2000, I = 0, F = 0:0, C = 22 LD6N
        decimal.execute(state, MixWord(2000000022L)) map {
          _.registers.getI(6) mustEqual MixIndex(0)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 22 LD6N
        decimal.execute(state, MixWord(2000000922L)) map {
          _.registers.getI(6) mustEqual MixIndex(0x4001)
        }
      }

      "throw an exception on an attempt to load with negation a field that does not fit into an index register" in {
        recoverToSucceededIf[OverflowException] {
          // A = 2000, I = 0, F = 0:5, C = 22 LD6N
          decimal.execute(state, MixWord(2000000522L))
        }
      }
    }

    "loading to register X" should {
      "load the whole word" in {
        // A = 2000, I = 0, F = 0:5, C = 15 LDX
        decimal.execute(state, MixWord(2000000515L)) map {
          _.registers.getX mustEqual MixWord(0x400000000L | 116030504L)
        }
      }

      "load the whole word except for the sign as a positive number" in {
        // A = 2000, I = 0, F = 1:5, C = 15 LDX
        decimal.execute(state, MixWord(2000001315L)) map {
          _.registers.getX mustEqual MixWord(116030504L)
        }
      }

      "load a right-hand part as a positive number" in {
        // A = 2000, I = 0, F = 3:5, C = 15 LDX
        decimal.execute(state, MixWord(2000002915L)) map {
          _.registers.getX mustEqual MixWord(30504L)
        }
      }

      "load a left-hand part with a sign and shift it right" in {
        // A = 2000, I = 0, F = 0:3, C = 15 LDX
        decimal.execute(state, MixWord(2000000315L)) map {
          _.registers.getX mustEqual MixWord(0x400000000L | 11603L)
        }
      }

      "load a left-hand part without the sign as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 15 LDX
        decimal.execute(state, MixWord(2000000915L)) map {
          _.registers.getX mustEqual MixWord(1)
        }
      }

      "load a middle part as a positive number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 15 LDX
        decimal.execute(state, MixWord(2000003615L)) map {
          _.registers.getX mustEqual MixWord(5L)
        }
      }

      "load the sign only" in {
        // A = 2000, I = 0, F = 0:0, C = 15 LDX
        decimal.execute(state, MixWord(2000000015L)) map {
          _.registers.getX mustEqual MixWord(0x400000000L)
        }
      }

      "load with negation the whole word" in {
        // A = 2000, I = 0, F = 0:5, C = 23 LDXN
        decimal.execute(state, MixWord(2000000523L)) map {
          _.registers.getX mustEqual MixWord(116030504L)
        }
      }

      "load with negation the whole word except for the sign as a negative number" in {
        // A = 2000, I = 0, F = 1:5, C = 23 LDXN
        decimal.execute(state, MixWord(2000001323L)) map {
          _.registers.getX mustEqual MixWord(0x400000000L | 116030504L)
        }
      }

      "load with negation a right-hand part as a negative number" in {
        // A = 2000, I = 0, F = 3:5, C = 23 LDXN
        decimal.execute(state, MixWord(2000002923L)) map {
          _.registers.getX mustEqual MixWord(0x400000000L | 30504L)
        }
      }

      "load with negation a left-hand part with the sign, shift it right, and invert the sign" in {
        // A = 2000, I = 0, F = 0:3, C = 23 LDXN
        decimal.execute(state, MixWord(2000000323L)) map {
          _.registers.getX mustEqual MixWord(11603L)
        }
      }

      "load with negation a left-hand part without the sign as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 1:1, C = 23 LDXN
        decimal.execute(state, MixWord(2000000923L)) map {
          _.registers.getX mustEqual MixWord(0x400000001L)
        }
      }

      "load with negation a middle part as a negative number and shift it right" in {
        // A = 2000, I = 0, F = 4:4, C = 23 LDXN
        decimal.execute(state, MixWord(2000003623L)) map {
          _.registers.getX mustEqual MixWord(0x400000005L)
        }
      }

      "load with negation the sign only and invert it" in {
        // A = 2000, I = 0, F = 0:0, C = 23 LDXN
        decimal.execute(state, MixWord(2000000023L)) map {
          _.registers.getX mustEqual MixWord(0L)
        }
      }
    }
  }

  "decimal storing to memory" when {
    "storing from register A" should {
      "replace the whole content of a memory cell" in {
        // A = 2001, I = 0, F = 0:5, C = 24 STA
        decimal.execute(state, MixWord(2001000524L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(607080900L)
        }
      }

      "replace the whole content of a memory cell except for the sign" in {
        // A = 2001, I = 0, F = 1:5, C = 24 STA
        decimal.execute(state, MixWord(2001001324L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 607080900L)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 24 STA
        decimal.execute(state, MixWord(2001004524L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 102030400L)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 24 STA
        decimal.execute(state, MixWord(2001001824L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 100030405L)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 24 STA
        decimal.execute(state, MixWord(2001001924L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 109000405L)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 24 STA
        decimal.execute(state, MixWord(2001000124L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(2030405L)
        }
      }
    }

    "storing from register I1" should {
      "replace the whole content of a memory cell, filling lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 0:5, C = 25 ST1
        decimal.execute(state, MixWord(2001000525L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(900L)
        }
      }

      "replace the whole content of a memory cell except for the sign, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 1:5, C = 25 ST1
        decimal.execute(state, MixWord(2001001325L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 900L)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 25 ST1
        decimal.execute(state, MixWord(2001004525L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 102030400L)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 25 ST1
        decimal.execute(state, MixWord(2001001825L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 100030405L)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 25 ST1
        decimal.execute(state, MixWord(2001001925L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 109000405L)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 25 ST1
        decimal.execute(state, MixWord(2001000125L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(2030405L)
        }
      }
    }

    "storing from register I2" should {
      "replace the whole content of a memory cell, filling lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 0:5, C = 26 ST2
        decimal.execute(state, MixWord(2001000526L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 900L)
        }
      }

      "replace the whole content of a memory cell except for the sign, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 1:5, C = 26 ST2
        decimal.execute(state, MixWord(2001001326L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 900L)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 26 ST2
        decimal.execute(state, MixWord(2001004526L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 102030400L)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 26 ST2
        decimal.execute(state, MixWord(2001001826L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 100030405L)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 26 ST2
        decimal.execute(state, MixWord(2001001926L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 109000405L)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 26 ST2
        decimal.execute(state, MixWord(2001000126L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 2030405L)
        }
      }
    }

    "storing from register I3" should {
      "replace the whole content of a memory cell, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 0:5, C = 27 ST3
        decimal.execute(state, MixWord(2001000527L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(900L)
        }
      }

      "replace the whole content of a memory cell except for the sign, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 1:5, C = 27 ST3
        decimal.execute(state, MixWord(2001001327L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 900L)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 27 ST3
        decimal.execute(state, MixWord(2001004527L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 102030400L)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 27 ST3
        decimal.execute(state, MixWord(2001001827L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 100030405L)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 27 ST3
        decimal.execute(state, MixWord(2001001927L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 109000405L)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 27 ST3
        decimal.execute(state, MixWord(2001000127L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(2030405L)
        }
      }
    }

    "storing from register I4" should {
      "replace the whole content of a memory cell, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 0:5, C = 28 ST4
        decimal.execute(state, MixWord(2001000527L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(900L)
        }
      }

      "replace the whole content of a memory cell except for the sign, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 1:5, C = 28 ST4
        decimal.execute(state, MixWord(2001001328L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 900L)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 28 ST4
        decimal.execute(state, MixWord(2001004528L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 102030400L)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 28 ST4
        decimal.execute(state, MixWord(2001001828L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 100030405L)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 28 ST4
        decimal.execute(state, MixWord(2001001928L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 109000405L)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 28 ST4
        decimal.execute(state, MixWord(2001000128L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(2030405L)
        }
      }
    }

    "storing from register I5" should {
      "replace the whole content of a memory cell, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 0:5, C = 29 ST5
        decimal.execute(state, MixWord(2001000529L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(900L)
        }
      }

      "replace the whole content of a memory cell except for the sign, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 1:5, C = 29 ST5
        decimal.execute(state, MixWord(2001001329L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 900L)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 29 ST5
        decimal.execute(state, MixWord(2001004529L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 102030400L)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 29 ST5
        decimal.execute(state, MixWord(2001001829L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 100030405L)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 29 ST5
        decimal.execute(state, MixWord(2001001929L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 109000405L)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 29 ST5
        decimal.execute(state, MixWord(2001000129L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(2030405L)
        }
      }
    }

    "storing from register I6" should {
      "replace the whole content of a memory cell, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 0:5, C = 30 ST6
        decimal.execute(state, MixWord(2001000530L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(900L)
        }
      }

      "replace the whole content of a memory cell except for the sign, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 1:5, C = 30 ST6
        decimal.execute(state, MixWord(2001001330L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 900L)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 30 ST6
        decimal.execute(state, MixWord(2001004530L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 102030400L)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 30 ST6
        decimal.execute(state, MixWord(2001001830L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 100030405L)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 30 ST6
        decimal.execute(state, MixWord(2001001930L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 109000405L)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 30 ST6
        decimal.execute(state, MixWord(2001000130L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(2030405L)
        }
      }
    }

    "storing from register X" should {
      "replace the whole content of a memory cell" in {
        // A = 2001, I = 0, F = 0:5, C = 31 STX
        decimal.execute(state, MixWord(2001000531L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 607080900L)
        }
      }

      "replace the whole content of a memory cell except for the sign" in {
        // A = 2001, I = 0, F = 1:5, C = 31 STX
        decimal.execute(state, MixWord(2001001331L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 607080900L)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 31 STX
        decimal.execute(state, MixWord(2001004531L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 102030400L)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 31 STX
        decimal.execute(state, MixWord(2001001831L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 100030405L)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 31 STX
        decimal.execute(state, MixWord(2001001931L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 109000405L)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 31 STX
        decimal.execute(state, MixWord(2001000131L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 2030405L)
        }
      }
    }

    "storing from register J" should {
      "replace the whole content of a memory cell, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 0:5, C = 32 STJ
        decimal.execute(state, MixWord(2001000532L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(3001L)
        }
      }

      "replace the whole content of a memory cell except for the sign, filling the lacking bytes with zeros" in {
        // A = 2001, I = 0, F = 1:5, C = 32 STJ
        decimal.execute(state, MixWord(2001001332L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 3001L)
        }
      }

      "replace only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 32 STJ
        decimal.execute(state, MixWord(2001004532L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 102030401L)
        }
      }

      "replace only a middle part of a memory cell with a right-hand part of the register" in {
        // A = 2001, I = 0, F = 2:2, C = 32 STJ
        decimal.execute(state, MixWord(2001001832L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 101030405L)
        }
      }

      "replace only a middle part of a memory cell with the whole content of the register except for the sign" in {
        // A = 2001, I = 0, F = 2:3, C = 32 STJ
        decimal.execute(state, MixWord(2001001932L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 130010405L)
        }
      }

      "replace only a left-hand part with the sign of a memory cell with a right-hand part and the sign of the register" in {
        // A = 2001, I = 0, F = 0:1, C = 32 STJ
        decimal.execute(state, MixWord(2001000132L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(102030405L)
        }
      }
    }

    "storing zero" should {
      "clear the whole content of a memory cell" in {
        // A = 2001, I = 0, F = 0:5, C = 33 STZ
        decimal.execute(state, MixWord(2001000533L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0L)
        }
      }

      "clear the whole content of a memory cell except for the sign" in {
        // A = 2001, I = 0, F = 1:5, C = 33 STZ
        decimal.execute(state, MixWord(2001001333L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L)
        }
      }

      "clear only a right-hand part of a memory cell" in {
        // A = 2001, I = 0, F = 5:5, C = 33 STZ
        decimal.execute(state, MixWord(2001004533L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 102030400L)
        }
      }

      "clear only a middle part of a memory cell" in {
        // A = 2001, I = 0, F = 2:3, C = 33 STZ
        decimal.execute(state, MixWord(2001001933L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(0x400000000L | 100000405L)
        }
      }

      "clear only a left-hand part with the sign of a memory cell" in {
        // A = 2001, I = 0, F = 0:1, C = 33 STZ
        decimal.execute(state, MixWord(2001000133L)) map {
          _.memory.get(2001.toShort) mustEqual MixWord(2030405L)
        }
      }
    }
  }

  "decimal moving in memory" should {
    "move memory words" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, MixIndex(3000)),
        memory = initialState.memory
          .updated(MixIndex(2000), MixWord(1L))
          .updated(MixIndex(2001), MixWord(2L))
          .updated(MixIndex(2002), MixWord(3L))
      )
      // A = 2000, I = 0, F = 3, C = 7 MOVE
      decimal.execute(prevState, MixWord(2000000307L)).map(s => {
        s.registers.getI(1) mustEqual MixIndex(3003)
        s.memory.get(3000.toShort) mustEqual MixWord(1L)
        s.memory.get(3001.toShort) mustEqual MixWord(2L)
        s.memory.get(3002.toShort) mustEqual MixWord(3L)
      })
    }

    "do nothing when the number of words to move is zero" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, MixIndex(3000)),
        memory = initialState.memory
          .updated(MixIndex(2000), MixWord(1L))
          .updated(MixIndex(2001), MixWord(2L))
          .updated(MixIndex(2002), MixWord(3L))
      )
      // A = 2000, I = 0, F = 0, C = 7 MOVE
      decimal.execute(prevState, MixWord(2000000007L)).map(s => {
        s.registers.getI(1) mustEqual MixIndex(3000)
        s.memory.get(3000.toShort) mustEqual MixWord(0L)
        s.memory.get(3001.toShort) mustEqual MixWord(0L)
        s.memory.get(3002.toShort) mustEqual MixWord(0L)
      })
    }

    "move overlapping ranges in the downward direction" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, MixIndex(1999)),
        memory = initialState.memory
          .updated(MixIndex(2000), MixWord(1L))
          .updated(MixIndex(2001), MixWord(2L))
          .updated(MixIndex(2002), MixWord(3L))
      )
      // A = 2000, I = 0, F = 3, C = 7 MOVE
      decimal.execute(prevState, MixWord(2000000307L)).map(s => {
        s.registers.getI(1) mustEqual MixIndex(2002)
        s.memory.get(1999.toShort) mustEqual MixWord(1L)
        s.memory.get(2000.toShort) mustEqual MixWord(2L)
        s.memory.get(2001.toShort) mustEqual MixWord(3L)
        s.memory.get(2002.toShort) mustEqual MixWord(3L)
      })
    }

    "move overlapping ranges in the upward direction" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, MixIndex(2001)),
        memory = initialState.memory
          .updated(MixIndex(2000), MixWord(1L))
          .updated(MixIndex(2001), MixWord(2L))
          .updated(MixIndex(2002), MixWord(3L))
      )
      // A = 2000, I = 0, F = 3, C = 7 MOVE
      decimal.execute(prevState, MixWord(2000000307L)).map(s => {
        s.registers.getI(1) mustEqual MixIndex(2004)
        s.memory.get(2000.toShort) mustEqual MixWord(1L)
        s.memory.get(2001.toShort) mustEqual MixWord(1L)
        s.memory.get(2002.toShort) mustEqual MixWord(1L)
        s.memory.get(2003.toShort) mustEqual MixWord(1L)
      })
    }
  }
}
