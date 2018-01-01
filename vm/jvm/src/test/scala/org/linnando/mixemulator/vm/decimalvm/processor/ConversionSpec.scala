package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class ConversionSpec(implicit ee: ExecutionEnv) extends Specification {

  import decimal._

  private val initialState = decimal.initialState

  "decimal conversion module" should {
    "convert characters to numeric" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(MixWord(0x400000000L | 313239L)) // - 0 0 31 32 39
          .updatedX(MixWord(3757473030L)) // + 37 57 47 30 30
      )
      // A = 0, I = 0, F = 0, C = 5 NUM
      val nextState = execute(state, MixWord(5L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x400000000L | 12977700L)).await // - 0 12 97 77 0
      nextState.map(_.registers.getX) must beEqualTo(MixWord(3757473030L)).await // + 37 57 47 30 30
      nextState.map(_.registers.getOV) must beFalse.await
    }

    "convert number to characters" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(MixWord(0x400000000L | 12977699L)) // - 0 12 97 76 99
          .updatedX(MixWord(3757473030L)) // + 37 57 47 30 30
      )
      // A = 0, I = 0, F = 1, C = 5 CHAR
      val nextState = execute(state, MixWord(105L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x400000000L | 3030313239L)).await // - 30 30 31 32 39
      nextState.map(_.registers.getX) must beEqualTo(MixWord(3737363939L)).await // + 37 37 36 39 39
      nextState.map(_.registers.getOV) must beFalse.await
    }
  }
}
