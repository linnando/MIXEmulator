package org.linnando.mixemulator.vm.binaryvm.datamodel

import org.linnando.mixemulator.vm.Comparison
import org.linnando.mixemulator.vm.binary._
import org.linnando.mixemulator.vm.exceptions.WrongIndexSpecException
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RegisterStateSpec extends AnyWordSpec with Matchers {
  private val initialState = RegisterState.initialState

  "binary register state" should {
    "get A contents" in {
      val state = initialState.copy(a = 0x1)
      state.getA mustEqual MixWord(0x1)
    }

    "get X contents" in {
      val state = initialState.copy(x = 0x1)
      state.getX mustEqual MixWord(0x1)
    }

    "get A and X contents" in {
      val state = initialState.copy(a = 0x3, x = 0x40000001)
      state.getAX mustEqual MixDWord(0xc0000001L)
    }

    "get Ix contents by byte index" in {
      val indices = Vector[Short](0x1, 0x2, 0x3, 0x4, 0x5, 0x6)
      val state = initialState.copy(i = indices)
      state.getI(MixByte(1)) mustEqual MixIndex(indices(0))
    }

    "get Ix contents by Int index" in {
      val indices = Vector[Short](0x1, 0x2, 0x3, 0x4, 0x5, 0x6)
      val state = initialState.copy(i = indices)
      state.getI(2) mustEqual MixIndex(indices(1))
    }

    "get J contents" in {
      val state = initialState.copy(j = 0x1)
      state.getJ mustEqual MixIndex(0x1)
    }

    "get OV contents" in {
      val state = initialState.copy(ov = true)
      state.getOV mustBe true
    }

    "get CMP contents" in {
      val state = initialState.copy(cmp = Comparison.GREATER)
      state.getCMP mustEqual Comparison.GREATER
    }

    "update A contents" in {
      val state = initialState.updatedA(MixWord(0x1))
      state.a mustEqual 0x1
    }

    "update X contents" in {
      val state = initialState.updatedX(MixWord(0x1))
      state.x mustEqual 0x1
    }

    "update A and X contents" in {
      val state = initialState.updatedAX(MixDWord(0xc0000001L), true)
      state.a mustEqual 0x3
      state.x mustEqual 0x40000001
    }

    "update Ix contents by byte index" in {
      val state = initialState.updatedI(MixByte(1), MixIndex(0x3))
      state.i mustEqual Vector(3, 0, 0, 0, 0, 0)
    }

    "update Ix contents by Int index" in {
      val state = initialState.updatedI(1, MixIndex(0x3))
      state.i mustEqual Vector(3, 0, 0, 0, 0, 0)
    }

    "update J contents" in {
      val state = initialState.updatedJ(MixIndex(0x1))
      state.j mustEqual 0x1
    }

    "update OV contents" in {
      val state = initialState.updatedOV(true)
      state.ov mustBe true
    }

    "update CMP contents" in {
      val state = initialState.updatedCMP(Comparison.GREATER)
      state.cmp mustEqual Comparison.GREATER
    }

    "throw an exception if index spec is negative" in {
      val getException = the[WrongIndexSpecException] thrownBy initialState.getI(-1)
      getException.indexSpec mustEqual -1
      val updateException = the[WrongIndexSpecException] thrownBy initialState.updatedI(-1, MixIndex(0x1))
      updateException.indexSpec mustEqual -1
    }

    "throw an exception if byte index spec is too big" in {
      val getException = the[WrongIndexSpecException] thrownBy initialState.getI(MixByte(7))
      getException.indexSpec mustEqual 7
      val updateException = the[WrongIndexSpecException] thrownBy initialState.updatedI(MixByte(7), MixIndex(0x1))
      updateException.indexSpec mustEqual 7
    }

    "throw an exception if index spec is too big" in {
      val getException = the[WrongIndexSpecException] thrownBy initialState.getI(7)
      getException.indexSpec mustEqual 7
      val updateException = the[WrongIndexSpecException] thrownBy initialState.updatedI(7, MixIndex(0x1))
      updateException.indexSpec mustEqual 7
    }
  }
}
