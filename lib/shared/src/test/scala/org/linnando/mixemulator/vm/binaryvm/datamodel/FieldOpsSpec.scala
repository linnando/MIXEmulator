package org.linnando.mixemulator.vm.binaryvm.datamodel

import org.linnando.mixemulator.vm.binary._
import org.linnando.mixemulator.vm.exceptions.WrongFieldSpecException
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FieldOpsSpec extends AnyWordSpec with Matchers {
  "binary index field selection" should {
    "consider a positive number to be positive and not negative" in {
      // + 1 2
      MixIndex(0x0042).isPositive mustBe true
      MixIndex(0x0042).isNegative mustBe false
    }

    "consider a negative number to be negative and not positive" in {
      // - 1 2
      MixIndex(0x1042).isPositive mustBe false
      MixIndex(0x1042).isNegative mustBe true
    }

    "consider a positive zero to be positive and not negative" in {
      // + 0 0
      MixIndex(0x0000).isPositive mustBe true
      MixIndex(0x0000).isNegative mustBe false
    }

    "consider a negative zero to be negative and not positive" in {
      // - 0 0
      MixIndex(0x1000).isPositive mustBe false
      MixIndex(0x1000).isNegative mustBe true
    }
  }

  "binary word building" should {
    "collect a word from fields" in {
      getWord(getIndex(-1000), getByte(1), getByte(5), getByte(8)) mustEqual MixWord(0x4fa01148)
    }
  }

  "binary word field selection" should {
    "consider a positive number to be positive and not negative" in {
      // + 1 2 3 4 5
      MixWord(0x01083105).isPositive mustBe true
      MixWord(0x01083105).isNegative mustBe false
    }

    "consider a negative number to be negative and not positive" in {
      // - 1 2 3 4 5
      MixWord(0x41083105).isPositive mustBe false
      MixWord(0x41083105).isNegative mustBe true
    }

    "consider a positive zero to be positive and not negative" in {
      // + 0 0 0 0 0
      MixWord(0x00000000).isPositive mustBe true
      MixWord(0x00000000).isNegative mustBe false
    }

    "consider a negative zero to be negative and not positive" in {
      // - 0 0 0 0 0
      MixWord(0x40000000).isPositive mustBe false
      MixWord(0x40000000).isNegative mustBe true
    }

    "select address from a positive word" in {
      // + 1 2 3 4 5 -> + 1 2
      MixWord(0x01083105).getAddress mustEqual MixIndex(0x0042)
    }

    "select address from a negative word" in {
      // - 1 2 3 4 5 -> - 1 2
      MixWord(0x41083105).getAddress mustEqual MixIndex(0x1042)
    }

    "select index spec from a word" in {
      // + 1 2 3 4 5 -> 3
      MixWord(0x01083105).getIndexSpec mustEqual MixByte(0x03)
    }

    "select operation modifier from a word" in {
      // + 1 2 3 4 5 -> 4
      MixWord(0x01083105).getFieldSpec mustEqual MixByte(0x04)
    }

    "select operation code from a word" in {
      // + 1 2 3 4 5 -> 5
      MixWord(0x01083105).getOpCode mustEqual MixByte(0x05)
    }

    "select arbitrary field from a positive word" in {
      // + 1 2 3 4 5
      val word = MixWord(0x01083105)
      word.getField(MixByte(0x00)) mustEqual MixWord(0x00000000) // + 0 0 0 0 0
      word.getField(MixByte(0x01)) mustEqual MixWord(0x00000001) // + 0 0 0 0 1
      word.getField(MixByte(0x02)) mustEqual MixWord(0x00000042) // + 0 0 0 1 2
      word.getField(MixByte(0x03)) mustEqual MixWord(0x00001083) // + 0 0 1 2 3
      word.getField(MixByte(0x04)) mustEqual MixWord(0x000420c4) // + 0 1 2 3 4
      word.getField(MixByte(0x05)) mustEqual MixWord(0x01083105) // + 1 2 3 4 5
      word.getField(MixByte(0x09)) mustEqual MixWord(0x00000001) // + 0 0 0 0 1
      word.getField(MixByte(0x0a)) mustEqual MixWord(0x00000042) // + 0 0 0 1 2
      word.getField(MixByte(0x0b)) mustEqual MixWord(0x00001083) // + 0 0 1 2 3
      word.getField(MixByte(0x0c)) mustEqual MixWord(0x000420c4) // + 0 1 2 3 4
      word.getField(MixByte(0x0d)) mustEqual MixWord(0x01083105) // + 1 2 3 4 5
      word.getField(MixByte(0x12)) mustEqual MixWord(0x00000002) // + 0 0 0 0 2
      word.getField(MixByte(0x13)) mustEqual MixWord(0x00000083) // + 0 0 0 2 3
      word.getField(MixByte(0x14)) mustEqual MixWord(0x000020c4) // + 0 0 2 3 4
      word.getField(MixByte(0x15)) mustEqual MixWord(0x00083105) // + 0 2 3 4 5
      word.getField(MixByte(0x1b)) mustEqual MixWord(0x00000003) // + 0 0 0 0 3
      word.getField(MixByte(0x1c)) mustEqual MixWord(0x000000c4) // + 0 0 0 3 4
      word.getField(MixByte(0x1d)) mustEqual MixWord(0x00003105) // + 0 0 3 4 5
      word.getField(MixByte(0x24)) mustEqual MixWord(0x00000004) // + 0 0 0 0 4
      word.getField(MixByte(0x25)) mustEqual MixWord(0x00000105) // + 0 0 0 4 5
      word.getField(MixByte(0x2d)) mustEqual MixWord(0x00000005) // + 0 0 0 0 5
    }

    "select arbitrary field from a negative word" in {
      // - 1 2 3 4 5
      val word = MixWord(0x41083105)
      word.getField(MixByte(0x00)) mustEqual MixWord(0x40000000) // - 0 0 0 0 0
      word.getField(MixByte(0x01)) mustEqual MixWord(0x40000001) // - 0 0 0 0 1
      word.getField(MixByte(0x02)) mustEqual MixWord(0x40000042) // - 0 0 0 1 2
      word.getField(MixByte(0x03)) mustEqual MixWord(0x40001083) // - 0 0 1 2 3
      word.getField(MixByte(0x04)) mustEqual MixWord(0x400420c4) // - 0 1 2 3 4
      word.getField(MixByte(0x05)) mustEqual MixWord(0x41083105) // - 1 2 3 4 5
      word.getField(MixByte(0x09)) mustEqual MixWord(0x00000001) // + 0 0 0 0 1
      word.getField(MixByte(0x0a)) mustEqual MixWord(0x00000042) // + 0 0 0 1 2
      word.getField(MixByte(0x0b)) mustEqual MixWord(0x00001083) // + 0 0 1 2 3
      word.getField(MixByte(0x0c)) mustEqual MixWord(0x000420c4) // + 0 1 2 3 4
      word.getField(MixByte(0x0d)) mustEqual MixWord(0x01083105) // + 1 2 3 4 5
      word.getField(MixByte(0x12)) mustEqual MixWord(0x00000002) // + 0 0 0 0 2
      word.getField(MixByte(0x13)) mustEqual MixWord(0x00000083) // + 0 0 0 2 3
      word.getField(MixByte(0x14)) mustEqual MixWord(0x000020c4) // + 0 0 2 3 4
      word.getField(MixByte(0x15)) mustEqual MixWord(0x00083105) // + 0 2 3 4 5
      word.getField(MixByte(0x1b)) mustEqual MixWord(0x00000003) // + 0 0 0 0 3
      word.getField(MixByte(0x1c)) mustEqual MixWord(0x000000c4) // + 0 0 0 3 4
      word.getField(MixByte(0x1d)) mustEqual MixWord(0x00003105) // + 0 0 3 4 5
      word.getField(MixByte(0x24)) mustEqual MixWord(0x00000004) // + 0 0 0 0 4
      word.getField(MixByte(0x25)) mustEqual MixWord(0x00000105) // + 0 0 0 4 5
      word.getField(MixByte(0x2d)) mustEqual MixWord(0x00000005) // + 0 0 0 0 5
    }

    "throw an exception if field number is wrong" in {
      val exception = the[WrongFieldSpecException] thrownBy MixWord(0x0).getField(MixByte(0x06))
      // TODO Fix the bug in the code, it should be 0x06
      exception.fieldSpec mustEqual 0x0e
    }

    "throw an exception if l > r in a field spec" in {
      val exception = the[WrongFieldSpecException] thrownBy MixWord(0x0).getField(MixByte(0x08))
      exception.fieldSpec mustEqual 0x08
    }
  }

  "binary word field update" should {
    "update a field of a positive word" in {
      val word = MixWord(0x01083105) // + 1 2 3 4 5
      val value = MixWord(0x061c8240) // + 6 7 8 9 0
      word.updated(MixByte(0x00), value) mustEqual MixWord(0x01083105) // + 1 2 3 4 5
      word.updated(MixByte(0x01), value) mustEqual MixWord(0x00083105) // + 0 2 3 4 5
      word.updated(MixByte(0x02), value) mustEqual MixWord(0x09003105) // + 9 0 3 4 5
      word.updated(MixByte(0x03), value) mustEqual MixWord(0x08240105) // + 8 9 0 4 5
      word.updated(MixByte(0x04), value) mustEqual MixWord(0x07209005) // + 7 8 9 0 5
      word.updated(MixByte(0x05), value) mustEqual MixWord(0x061c8240) // + 6 7 8 9 0
      word.updated(MixByte(0x09), value) mustEqual MixWord(0x00083105) // + 0 2 3 4 5
      word.updated(MixByte(0x0a), value) mustEqual MixWord(0x09003105) // + 9 0 3 4 5
      word.updated(MixByte(0x0b), value) mustEqual MixWord(0x08240105) // + 8 9 0 4 5
      word.updated(MixByte(0x0c), value) mustEqual MixWord(0x07209005) // + 7 8 9 0 5
      word.updated(MixByte(0x0d), value) mustEqual MixWord(0x061c8240) // + 6 7 8 9 0
      word.updated(MixByte(0x12), value) mustEqual MixWord(0x01003105) // + 1 0 3 4 5
      word.updated(MixByte(0x13), value) mustEqual MixWord(0x01240105) // + 1 9 0 4 5
      word.updated(MixByte(0x14), value) mustEqual MixWord(0x01209005) // + 1 8 9 0 5
      word.updated(MixByte(0x15), value) mustEqual MixWord(0x011c8240) // + 1 7 8 9 0
      word.updated(MixByte(0x1b), value) mustEqual MixWord(0x01080105) // + 1 2 0 4 5
      word.updated(MixByte(0x1c), value) mustEqual MixWord(0x01089005) // + 1 2 9 0 5
      word.updated(MixByte(0x1d), value) mustEqual MixWord(0x01088240) // + 1 2 8 9 0
      word.updated(MixByte(0x24), value) mustEqual MixWord(0x01083005) // + 1 2 3 0 5
      word.updated(MixByte(0x25), value) mustEqual MixWord(0x01083240) // + 1 2 3 9 0
      word.updated(MixByte(0x2d), value) mustEqual MixWord(0x01083100) // + 1 2 3 4 0
    }

    "update a field of a negative word" in {
      val word = MixWord(0x41083105) // - 1 2 3 4 5
      val value = MixWord(0x061c8240) // + 6 7 8 9 0
      word.updated(MixByte(0x00), value) mustEqual MixWord(0x01083105) // + 1 2 3 4 5
      word.updated(MixByte(0x01), value) mustEqual MixWord(0x00083105) // + 0 2 3 4 5
      word.updated(MixByte(0x02), value) mustEqual MixWord(0x09003105) // + 9 0 3 4 5
      word.updated(MixByte(0x03), value) mustEqual MixWord(0x08240105) // + 8 9 0 4 5
      word.updated(MixByte(0x04), value) mustEqual MixWord(0x07209005) // + 7 8 9 0 5
      word.updated(MixByte(0x05), value) mustEqual MixWord(0x061c8240) // + 6 7 8 9 0
      word.updated(MixByte(0x09), value) mustEqual MixWord(0x40083105) // - 0 2 3 4 5
      word.updated(MixByte(0x0a), value) mustEqual MixWord(0x49003105) // - 9 0 3 4 5
      word.updated(MixByte(0x0b), value) mustEqual MixWord(0x48240105) // - 8 9 0 4 5
      word.updated(MixByte(0x0c), value) mustEqual MixWord(0x47209005) // - 7 8 9 0 5
      word.updated(MixByte(0x0d), value) mustEqual MixWord(0x461c8240) // - 6 7 8 9 0
      word.updated(MixByte(0x12), value) mustEqual MixWord(0x41003105) // - 1 0 3 4 5
      word.updated(MixByte(0x13), value) mustEqual MixWord(0x41240105) // - 1 9 0 4 5
      word.updated(MixByte(0x14), value) mustEqual MixWord(0x41209005) // - 1 8 9 0 5
      word.updated(MixByte(0x15), value) mustEqual MixWord(0x411c8240) // - 1 7 8 9 0
      word.updated(MixByte(0x1b), value) mustEqual MixWord(0x41080105) // - 1 2 0 4 5
      word.updated(MixByte(0x1c), value) mustEqual MixWord(0x41089005) // - 1 2 9 0 5
      word.updated(MixByte(0x1d), value) mustEqual MixWord(0x41088240) // - 1 2 8 9 0
      word.updated(MixByte(0x24), value) mustEqual MixWord(0x41083005) // - 1 2 3 0 5
      word.updated(MixByte(0x25), value) mustEqual MixWord(0x41083240) // - 1 2 3 9 0
      word.updated(MixByte(0x2d), value) mustEqual MixWord(0x41083100) // - 1 2 3 4 0
    }

    "throw an exception if field number is wrong" in {
      val exception = the[WrongFieldSpecException] thrownBy MixWord(0x0).updated(MixByte(0x06), MixWord(0x0))
      exception.fieldSpec mustEqual 0x06
    }

    "throw an exception if l > r in a field spec" in {
      val exception = the[WrongFieldSpecException] thrownBy MixWord(0x0).updated(MixByte(0x08), MixWord(0x0))
      exception.fieldSpec mustEqual 0x08
    }
  }

  "binary double word field selection" should {
    "consider a positive number to be positive and not negative" in {
      // + 1 2 3 4 5 6 7 8 9 0
      MixDWord(0x00420c41461c8240L).isPositive mustBe true
      MixDWord(0x00420c41461c8240L).isNegative mustBe false
    }

    "consider a negative number to be negative and not positive" in {
      // - 1 2 3 4 5 6 7 8 9 0
      MixDWord(0x10420c41461c8240L).isPositive mustBe false
      MixDWord(0x10420c41461c8240L).isNegative mustBe true
    }

    "consider a positive zero to be positive and not negative" in {
      // + 0 0 0 0 0 0 0 0 0 0
      MixDWord(0x0000000000000000L).isPositive mustBe true
      MixDWord(0x0000000000000000L).isNegative mustBe false
    }

    "consider a negative zero to be negative and not positive" in {
      // - 0 0 0 0 0 0 0 0 0 0
      MixDWord(0x1000000000000000L).isPositive mustBe false
      MixDWord(0x1000000000000000L).isNegative mustBe true
    }
  }
}
