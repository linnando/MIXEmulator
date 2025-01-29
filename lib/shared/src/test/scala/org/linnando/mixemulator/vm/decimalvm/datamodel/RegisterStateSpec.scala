package org.linnando.mixemulator.vm.decimalvm.datamodel

import org.linnando.mixemulator.vm.Comparison
import org.linnando.mixemulator.vm.decimal._
import org.linnando.mixemulator.vm.exceptions.WrongIndexSpecException
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class RegisterStateSpec extends AsyncWordSpec with Matchers {
  private val initialState = RegisterState.initialState

  "decimal register state" should {
    "get A contents" in {
      val state = initialState.copy(a = 1L)
      state.getA mustEqual MixWord(1L)
    }

    "get X contents" in {
      val state = initialState.copy(x = 1L)
      state.getX mustEqual MixWord(1L)
    }

    "get A and X contents" in {
      val state = initialState.copy(a = 3L, x = 0x400000000L | 1L)
      state.getAX mustEqual MixDWord(3L, 1L)
    }

    "get Ix contents by byte index" in {
      val indices = Vector[Short](1, 2, 3, 4, 5, 6)
      val state = initialState.copy(i = indices)
      state.getI(MixByte(1)) mustEqual MixIndex(indices(0))
    }

    "get Ix contents by Int index" in {
      val indices = Vector[Short](1, 2, 3, 4, 5, 6)
      val state = initialState.copy(i = indices)
      state.getI(2) mustEqual MixIndex(indices(1))
    }

    "get J contents" in {
      val state = initialState.copy(j = 1)
      state.getJ mustEqual MixIndex(1)
    }

    "get OV contents" in {
      val state = initialState.copy(ov = true)
      state.getOV mustEqual true
    }

    "get CMP contents" in {
      val state = initialState.copy(cmp = Comparison.GREATER)
      state.getCMP mustEqual Comparison.GREATER
    }

    "update A contents" in {
      val state = initialState.updatedA(MixWord(1L))
      state.a mustEqual 1L
    }

    "update X contents" in {
      val state = initialState.updatedX(MixWord(1L))
      state.x mustEqual 1L
    }

    "update A and X contents" in {
      val state = initialState.updatedAX(MixDWord(3L, 1L), xIsNegative = true)
      state.a mustEqual 3L
      state.x mustEqual 0x400000000L | 1L
    }

    "update Ix contents by byte index" in {
      val state = initialState.updatedI(MixByte(1), MixIndex(3))
      state.i mustEqual Vector(3, 0, 0, 0, 0, 0)
    }

    "update Ix contents by Int index" in {
      val state = initialState.updatedI(1, MixIndex(3))
      state.i mustEqual Vector(3, 0, 0, 0, 0, 0)
    }

    "update J contents" in {
      val state = initialState.updatedJ(MixIndex(1))
      state.j mustEqual 1
    }

    "update OV contents" in {
      val state = initialState.updatedOV(true)
      state.ov mustEqual true
    }

    "update CMP contents" in {
      val state = initialState.updatedCMP(Comparison.GREATER)
      state.cmp mustEqual Comparison.GREATER
    }

    "throw an exception if index spec is negative" in {
      a[WrongIndexSpecException] must be thrownBy initialState.getI(-1)
      a[WrongIndexSpecException] must be thrownBy initialState.updatedI(-1, MixIndex(1))
    }

    "throw an exception if byte index spec is too big" in {
      a[WrongIndexSpecException] must be thrownBy initialState.getI(MixByte(7))
      a[WrongIndexSpecException] must be thrownBy initialState.updatedI(MixByte(7), MixIndex(1))
    }

    "throw an exception if index spec is too big" in {
      a[WrongIndexSpecException] must be thrownBy initialState.getI(7)
      a[WrongIndexSpecException] must be thrownBy initialState.updatedI(7, MixIndex(1))
    }
  }
}
