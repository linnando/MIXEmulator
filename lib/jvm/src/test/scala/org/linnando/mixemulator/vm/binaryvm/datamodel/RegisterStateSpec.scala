package org.linnando.mixemulator.vm.binaryvm.datamodel

import org.linnando.mixemulator.vm.Comparison
import org.linnando.mixemulator.vm.binary._
import org.linnando.mixemulator.vm.exceptions.WrongIndexSpecException
import org.specs2.mutable.Specification

class RegisterStateSpec extends Specification {
  private val initialState = RegisterState.initialState

  "binary register state" should {
    "get A contents" in {
      val state = initialState.copy(a = 0x1)
      state.getA must be equalTo MixWord(0x1)
    }

    "get X contents" in {
      val state = initialState.copy(x = 0x1)
      state.getX must be equalTo MixWord(0x1)
    }

    "get A and X contents" in {
      val state = initialState.copy(a = 0x3, x = 0x40000001)
      state.getAX must be equalTo MixDWord(0xc0000001L)
    }

    "get Ix contents by byte index" in {
      val indices = Vector[Short](0x1, 0x2, 0x3, 0x4, 0x5, 0x6)
      val state = initialState.copy(i = indices)
      state.getI(MixByte(1)) must be equalTo MixIndex(indices(0))
    }

    "get Ix contents by Int index" in {
      val indices = Vector[Short](0x1, 0x2, 0x3, 0x4, 0x5, 0x6)
      val state = initialState.copy(i = indices)
      state.getI(2) must be equalTo MixIndex(indices(1))
    }

    "get J contents" in {
      val state = initialState.copy(j = 0x1)
      state.getJ must be equalTo MixIndex(0x1)
    }

    "get OV contents" in {
      val state = initialState.copy(ov = true)
      state.getOV must beTrue
    }

    "get CMP contents" in {
      val state = initialState.copy(cmp = Comparison.GREATER)
      state.getCMP must be equalTo Comparison.GREATER
    }

    "update A contents" in {
      val state = initialState.updatedA(MixWord(0x1))
      state.a must be equalTo 0x1
    }

    "update X contents" in {
      val state = initialState.updatedX(MixWord(0x1))
      state.x must be equalTo 0x1
    }

    "update A and X contents" in {
      val state = initialState.updatedAX(MixDWord(0xc0000001L), true)
      state.a must be equalTo 0x3
      state.x must be equalTo 0x40000001
    }

    "update Ix contents by byte index" in {
      val state = initialState.updatedI(MixByte(1), MixIndex(0x3))
      state.i must be equalTo Vector(3, 0, 0, 0, 0, 0)
    }

    "update Ix contents by Int index" in {
      val state = initialState.updatedI(1, MixIndex(0x3))
      state.i must be equalTo Vector(3, 0, 0, 0, 0, 0)
    }

    "update J contents" in {
      val state = initialState.updatedJ(MixIndex(0x1))
      state.j must be equalTo 0x1
    }

    "update OV contents" in {
      val state = initialState.updatedOV(true)
      state.ov must beTrue
    }

    "update CMP contents" in {
      val state = initialState.updatedCMP(Comparison.GREATER)
      state.cmp must be equalTo Comparison.GREATER
    }

    "throw an exception if index spec is negative" in {
      initialState.getI(-1) must throwA[WrongIndexSpecException]
      initialState.updatedI(-1, MixIndex(0x1)) must throwA[WrongIndexSpecException]
    }

    "throw an exception if byte index spec is too big" in {
      initialState.getI(MixByte(7)) must throwA[WrongIndexSpecException]
      initialState.updatedI(MixByte(7), MixIndex(0x1)) must throwA[WrongIndexSpecException]
    }

    "throw an exception if index spec is too big" in {
      initialState.getI(7) must throwA[WrongIndexSpecException]
      initialState.updatedI(7, MixIndex(0x1)) must throwA[WrongIndexSpecException]
    }
  }
}
