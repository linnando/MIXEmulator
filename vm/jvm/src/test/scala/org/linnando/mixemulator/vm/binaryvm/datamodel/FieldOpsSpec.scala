package org.linnando.mixemulator.vm.binaryvm.datamodel

import org.linnando.mixemulator.vm.binary._
import org.linnando.mixemulator.vm.exceptions.WrongFieldSpecException
import org.specs2.mutable.Specification

class FieldOpsSpec extends Specification {
  "binary index field selection" should {
    "consider a positive number to be positive and not negative" in {
      // + 1 2
      MixIndex(0x0042).isPositive must beTrue
      MixIndex(0x0042).isNegative must beFalse
    }

    "consider a negative number to be negative and not positive" in {
      // - 1 2
      MixIndex(0x1042).isPositive must beFalse
      MixIndex(0x1042).isNegative must beTrue
    }

    "consider a positive zero to be positive and not negative" in {
      // + 0 0
      MixIndex(0x0000).isPositive must beTrue
      MixIndex(0x0000).isNegative must beFalse
    }

    "consider a negative zero to be negative and not positive" in {
      // - 0 0
      MixIndex(0x1000).isPositive must beFalse
      MixIndex(0x1000).isNegative must beTrue
    }
  }

  "binary word field selection" should {
    "consider a positive number to be positive and not negative" in {
      // + 1 2 3 4 5
      MixWord(0x01083105).isPositive must beTrue
      MixWord(0x01083105).isNegative must beFalse
    }

    "consider a negative number to be negative and not positive" in {
      // - 1 2 3 4 5
      MixWord(0x41083105).isPositive must beFalse
      MixWord(0x41083105).isNegative must beTrue
    }

    "consider a positive zero to be positive and not negative" in {
      // + 0 0 0 0 0
      MixWord(0x00000000).isPositive must beTrue
      MixWord(0x00000000).isNegative must beFalse
    }

    "consider a negative zero to be negative and not positive" in {
      // - 0 0 0 0 0
      MixWord(0x40000000).isPositive must beFalse
      MixWord(0x40000000).isNegative must beTrue
    }

    "select address from a positive word" in {
      // + 1 2 3 4 5 -> + 1 2
      MixWord(0x01083105).getAddress must be equalTo MixIndex(0x0042)
    }

    "select address from a negative word" in {
      // - 1 2 3 4 5 -> - 1 2
      MixWord(0x41083105).getAddress must be equalTo MixIndex(0x1042)
    }

    "select index spec from a word" in {
      // + 1 2 3 4 5 -> 3
      MixWord(0x01083105).getIndexSpec must be equalTo MixByte(0x03)
    }

    "select operation modifier from a word" in {
      // + 1 2 3 4 5 -> 4
      MixWord(0x01083105).getFieldSpec must be equalTo MixByte(0x04)
    }

    "select operation code from a word" in {
      // + 1 2 3 4 5 -> 5
      MixWord(0x01083105).getOpCode must be equalTo MixByte(0x05)
    }

    "select arbitrary field from a positive word" in {
      // + 1 2 3 4 5
      val word = MixWord(0x01083105)
      word.getField(MixByte(0x00)) must be equalTo MixWord(0x00000000) // + 0 0 0 0 0
      word.getField(MixByte(0x01)) must be equalTo MixWord(0x00000001) // + 0 0 0 0 1
      word.getField(MixByte(0x02)) must be equalTo MixWord(0x00000042) // + 0 0 0 1 2
      word.getField(MixByte(0x03)) must be equalTo MixWord(0x00001083) // + 0 0 1 2 3
      word.getField(MixByte(0x04)) must be equalTo MixWord(0x000420c4) // + 0 1 2 3 4
      word.getField(MixByte(0x05)) must be equalTo MixWord(0x01083105) // + 1 2 3 4 5
      word.getField(MixByte(0x09)) must be equalTo MixWord(0x00000001) // + 0 0 0 0 1
      word.getField(MixByte(0x0a)) must be equalTo MixWord(0x00000042) // + 0 0 0 1 2
      word.getField(MixByte(0x0b)) must be equalTo MixWord(0x00001083) // + 0 0 1 2 3
      word.getField(MixByte(0x0c)) must be equalTo MixWord(0x000420c4) // + 0 1 2 3 4
      word.getField(MixByte(0x0d)) must be equalTo MixWord(0x01083105) // + 1 2 3 4 5
      word.getField(MixByte(0x12)) must be equalTo MixWord(0x00000002) // + 0 0 0 0 2
      word.getField(MixByte(0x13)) must be equalTo MixWord(0x00000083) // + 0 0 0 2 3
      word.getField(MixByte(0x14)) must be equalTo MixWord(0x000020c4) // + 0 0 2 3 4
      word.getField(MixByte(0x15)) must be equalTo MixWord(0x00083105) // + 0 2 3 4 5
      word.getField(MixByte(0x1b)) must be equalTo MixWord(0x00000003) // + 0 0 0 0 3
      word.getField(MixByte(0x1c)) must be equalTo MixWord(0x000000c4) // + 0 0 0 3 4
      word.getField(MixByte(0x1d)) must be equalTo MixWord(0x00003105) // + 0 0 3 4 5
      word.getField(MixByte(0x24)) must be equalTo MixWord(0x00000004) // + 0 0 0 0 4
      word.getField(MixByte(0x25)) must be equalTo MixWord(0x00000105) // + 0 0 0 4 5
      word.getField(MixByte(0x2d)) must be equalTo MixWord(0x00000005) // + 0 0 0 0 5
    }

    "select arbitrary field from a negative word" in {
      // - 1 2 3 4 5
      val word = MixWord(0x41083105)
      word.getField(MixByte(0x00)) must be equalTo MixWord(0x40000000) // - 0 0 0 0 0
      word.getField(MixByte(0x01)) must be equalTo MixWord(0x40000001) // - 0 0 0 0 1
      word.getField(MixByte(0x02)) must be equalTo MixWord(0x40000042) // - 0 0 0 1 2
      word.getField(MixByte(0x03)) must be equalTo MixWord(0x40001083) // - 0 0 1 2 3
      word.getField(MixByte(0x04)) must be equalTo MixWord(0x400420c4) // - 0 1 2 3 4
      word.getField(MixByte(0x05)) must be equalTo MixWord(0x41083105) // - 1 2 3 4 5
      word.getField(MixByte(0x09)) must be equalTo MixWord(0x00000001) // + 0 0 0 0 1
      word.getField(MixByte(0x0a)) must be equalTo MixWord(0x00000042) // + 0 0 0 1 2
      word.getField(MixByte(0x0b)) must be equalTo MixWord(0x00001083) // + 0 0 1 2 3
      word.getField(MixByte(0x0c)) must be equalTo MixWord(0x000420c4) // + 0 1 2 3 4
      word.getField(MixByte(0x0d)) must be equalTo MixWord(0x01083105) // + 1 2 3 4 5
      word.getField(MixByte(0x12)) must be equalTo MixWord(0x00000002) // + 0 0 0 0 2
      word.getField(MixByte(0x13)) must be equalTo MixWord(0x00000083) // + 0 0 0 2 3
      word.getField(MixByte(0x14)) must be equalTo MixWord(0x000020c4) // + 0 0 2 3 4
      word.getField(MixByte(0x15)) must be equalTo MixWord(0x00083105) // + 0 2 3 4 5
      word.getField(MixByte(0x1b)) must be equalTo MixWord(0x00000003) // + 0 0 0 0 3
      word.getField(MixByte(0x1c)) must be equalTo MixWord(0x000000c4) // + 0 0 0 3 4
      word.getField(MixByte(0x1d)) must be equalTo MixWord(0x00003105) // + 0 0 3 4 5
      word.getField(MixByte(0x24)) must be equalTo MixWord(0x00000004) // + 0 0 0 0 4
      word.getField(MixByte(0x25)) must be equalTo MixWord(0x00000105) // + 0 0 0 4 5
      word.getField(MixByte(0x2d)) must be equalTo MixWord(0x00000005) // + 0 0 0 0 5
    }

    "throw an exception if field number is wrong" in {
      MixWord(0x0).getField(MixByte(0x06)) must throwA[WrongFieldSpecException]
    }

    "throw an exception if l > r in a field spec" in {
      MixWord(0x0).getField(MixByte(0x08)) must throwA[WrongFieldSpecException]
    }
  }

  "binary double word field selection" should {
    "consider a positive number to be positive and not negative" in {
      // + 1 2 3 4 5 6 7 8 9 0
      MixDWord(0x00420c41461c8240L).isPositive must beTrue
      MixDWord(0x00420c41461c8240L).isNegative must beFalse
    }

    "consider a negative number to be negative and not positive" in {
      // - 1 2 3 4 5 6 7 8 9 0
      MixDWord(0x10420c41461c8240L).isPositive must beFalse
      MixDWord(0x10420c41461c8240L).isNegative must beTrue
    }

    "consider a positive zero to be positive and not negative" in {
      // + 0 0 0 0 0 0 0 0 0 0
      MixDWord(0x0000000000000000L).isPositive must beTrue
      MixDWord(0x0000000000000000L).isNegative must beFalse
    }

    "consider a negative zero to be negative and not positive" in {
      // - 0 0 0 0 0 0 0 0 0 0
      MixDWord(0x1000000000000000L).isPositive must beFalse
      MixDWord(0x1000000000000000L).isNegative must beTrue
    }
  }
}
