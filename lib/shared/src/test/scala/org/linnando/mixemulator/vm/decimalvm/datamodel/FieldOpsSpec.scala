package org.linnando.mixemulator.vm.decimalvm.datamodel

import org.linnando.mixemulator.vm.decimal._
import org.linnando.mixemulator.vm.exceptions.WrongFieldSpecException
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class FieldOpsSpec extends AsyncWordSpec with Matchers {
  "decimal index field selection" should {
    "consider a positive number to be positive and not negative" in {
      // + 1 2
      MixIndex(102).isPositive mustEqual true
      MixIndex(102).isNegative mustEqual false
    }

    "consider a negative number to be negative and not positive" in {
      // - 1 2
      MixIndex((0x4000 | 102).toShort).isPositive mustEqual false
      MixIndex((0x4000 | 102).toShort).isNegative mustEqual true
    }

    "consider a positive zero to be positive and not negative" in {
      // + 0 0
      MixIndex(0).isPositive mustEqual true
      MixIndex(0).isNegative mustEqual false
    }

    "consider a negative zero to be negative and not positive" in {
      // - 0 0
      MixIndex(0x4000).isPositive mustEqual false
      MixIndex(0x4000).isNegative mustEqual true
    }
  }

  "decimal word building" should {
    "collect a word from fields" in {
      getWord(getIndex(-1000), getByte(1), getByte(5), getByte(8)) mustEqual MixWord(0x400000000L | 1000010508L)
    }
  }

  "decimal word field selection" should {
    "consider a positive number to be positive and not negative" in {
      // + 1 2 3 4 5
      MixWord(102030405L).isPositive mustEqual true
      MixWord(102030405L).isNegative mustEqual false
    }

    "consider a negative number to be negative and not positive" in {
      // - 1 2 3 4 5
      MixWord(0x400000000L | 102030405L).isPositive mustEqual false
      MixWord(0x400000000L | 102030405L).isNegative mustEqual true
    }

    "consider a positive zero to be positive and not negative" in {
      // + 0 0 0 0 0
      MixWord(0).isPositive mustEqual true
      MixWord(0).isNegative mustEqual false
    }

    "consider a negative zero to be negative and not positive" in {
      // - 0 0 0 0 0
      MixWord(0x400000000L).isPositive mustEqual false
      MixWord(0x400000000L).isNegative mustEqual true
    }

    "select address from a positive word" in {
      // + 1 2 3 4 5 -> + 1 2
      MixWord(102030405L).getAddress mustEqual MixIndex(102)
    }

    "select address from a negative word" in {
      // - 1 2 3 4 5 -> - 1 2
      MixWord(0x400000000L | 102030405L).getAddress mustEqual MixIndex((0x4000 | 102).toShort)
    }

    "select index spec from a word" in {
      // + 1 2 3 4 5 -> 3
      MixWord(102030405).getIndexSpec mustEqual MixByte(3)
    }

    "select operation modifier from a word" in {
      // + 1 2 3 4 5 -> 4
      MixWord(102030405).getFieldSpec mustEqual MixByte(4)
    }

    "select operation code from a word" in {
      // + 1 2 3 4 5 -> 5
      MixWord(102030405).getOpCode mustEqual MixByte(5)
    }

    "select arbitrary field from a positive word" in {
      // + 1 2 3 4 5
      val word = MixWord(102030405)
      word.getField(MixByte(0x00)) mustEqual MixWord(0L) // + 0 0 0 0 0
      word.getField(MixByte(0x01)) mustEqual MixWord(1L) // + 0 0 0 0 1
      word.getField(MixByte(0x02)) mustEqual MixWord(102L) // + 0 0 0 1 2
      word.getField(MixByte(0x03)) mustEqual MixWord(10203L) // + 0 0 1 2 3
      word.getField(MixByte(0x04)) mustEqual MixWord(1020304L) // + 0 1 2 3 4
      word.getField(MixByte(0x05)) mustEqual MixWord(102030405L) // + 1 2 3 4 5
      word.getField(MixByte(0x09)) mustEqual MixWord(1L) // + 0 0 0 0 1
      word.getField(MixByte(0x0a)) mustEqual MixWord(102L) // + 0 0 0 1 2
      word.getField(MixByte(0x0b)) mustEqual MixWord(10203L) // + 0 0 1 2 3
      word.getField(MixByte(0x0c)) mustEqual MixWord(1020304L) // + 0 1 2 3 4
      word.getField(MixByte(0x0d)) mustEqual MixWord(102030405L) // + 1 2 3 4 5
      word.getField(MixByte(0x12)) mustEqual MixWord(2L) // + 0 0 0 0 2
      word.getField(MixByte(0x13)) mustEqual MixWord(203L) // + 0 0 0 2 3
      word.getField(MixByte(0x14)) mustEqual MixWord(20304L) // + 0 0 2 3 4
      word.getField(MixByte(0x15)) mustEqual MixWord(2030405L) // + 0 2 3 4 5
      word.getField(MixByte(0x1b)) mustEqual MixWord(3L) // + 0 0 0 0 3
      word.getField(MixByte(0x1c)) mustEqual MixWord(304L) // + 0 0 0 3 4
      word.getField(MixByte(0x1d)) mustEqual MixWord(30405L) // + 0 0 3 4 5
      word.getField(MixByte(0x24)) mustEqual MixWord(4L) // + 0 0 0 0 4
      word.getField(MixByte(0x25)) mustEqual MixWord(405L) // + 0 0 0 4 5
      word.getField(MixByte(0x2d)) mustEqual MixWord(5L) // + 0 0 0 0 5
    }

    "select arbitrary field from a negative word" in {
      // - 1 2 3 4 5
      val word = MixWord(0x400000000L | 102030405L)
      word.getField(MixByte(0x00)) mustEqual MixWord(0x400000000L) // - 0 0 0 0 0
      word.getField(MixByte(0x01)) mustEqual MixWord(0x400000000L | 1L) // - 0 0 0 0 1
      word.getField(MixByte(0x02)) mustEqual MixWord(0x400000000L | 102L) // - 0 0 0 1 2
      word.getField(MixByte(0x03)) mustEqual MixWord(0x400000000L | 10203L) // - 0 0 1 2 3
      word.getField(MixByte(0x04)) mustEqual MixWord(0x400000000L | 1020304L) // - 0 1 2 3 4
      word.getField(MixByte(0x05)) mustEqual MixWord(0x400000000L | 102030405L) // - 1 2 3 4 5
      word.getField(MixByte(0x09)) mustEqual MixWord(1L) // + 0 0 0 0 1
      word.getField(MixByte(0x0a)) mustEqual MixWord(102L) // + 0 0 0 1 2
      word.getField(MixByte(0x0b)) mustEqual MixWord(10203L) // + 0 0 1 2 3
      word.getField(MixByte(0x0c)) mustEqual MixWord(1020304L) // + 0 1 2 3 4
      word.getField(MixByte(0x0d)) mustEqual MixWord(102030405L) // + 1 2 3 4 5
      word.getField(MixByte(0x12)) mustEqual MixWord(2L) // + 0 0 0 0 2
      word.getField(MixByte(0x13)) mustEqual MixWord(203L) // + 0 0 0 2 3
      word.getField(MixByte(0x14)) mustEqual MixWord(20304L) // + 0 0 2 3 4
      word.getField(MixByte(0x15)) mustEqual MixWord(2030405L) // + 0 2 3 4 5
      word.getField(MixByte(0x1b)) mustEqual MixWord(3L) // + 0 0 0 0 3
      word.getField(MixByte(0x1c)) mustEqual MixWord(304L) // + 0 0 0 3 4
      word.getField(MixByte(0x1d)) mustEqual MixWord(30405L) // + 0 0 3 4 5
      word.getField(MixByte(0x24)) mustEqual MixWord(4L) // + 0 0 0 0 4
      word.getField(MixByte(0x25)) mustEqual MixWord(405L) // + 0 0 0 4 5
      word.getField(MixByte(0x2d)) mustEqual MixWord(5L) // + 0 0 0 0 5
    }

    "throw an exception if field number is wrong" in {
      a[WrongFieldSpecException] must be thrownBy MixWord(0L).getField(MixByte(0x06))
    }

    "throw an exception if l > r in a field spec" in {
      a[WrongFieldSpecException] must be thrownBy MixWord(0L).getField(MixByte(0x08))
    }
  }

  "decimal word field update" should {
    "update a field of a positive word" in {
      val word = MixWord(102030405L) // + 1 2 3 4 5
      val value = MixWord(607080900L) // + 6 7 8 9 0
      word.updated(MixByte(0x00), value) mustEqual MixWord(102030405L) // + 1 2 3 4 5
      word.updated(MixByte(0x01), value) mustEqual MixWord(2030405L) // + 0 2 3 4 5
      word.updated(MixByte(0x02), value) mustEqual MixWord(900030405L) // + 9 0 3 4 5
      word.updated(MixByte(0x03), value) mustEqual MixWord(809000405L) // + 8 9 0 4 5
      word.updated(MixByte(0x04), value) mustEqual MixWord(708090005L) // + 7 8 9 0 5
      word.updated(MixByte(0x05), value) mustEqual MixWord(607080900L) // + 6 7 8 9 0
      word.updated(MixByte(0x09), value) mustEqual MixWord(2030405L) // + 0 2 3 4 5
      word.updated(MixByte(0x0a), value) mustEqual MixWord(900030405L) // + 9 0 3 4 5
      word.updated(MixByte(0x0b), value) mustEqual MixWord(809000405L) // + 8 9 0 4 5
      word.updated(MixByte(0x0c), value) mustEqual MixWord(708090005L) // + 7 8 9 0 5
      word.updated(MixByte(0x0d), value) mustEqual MixWord(607080900L) // + 6 7 8 9 0
      word.updated(MixByte(0x12), value) mustEqual MixWord(100030405L) // + 1 0 3 4 5
      word.updated(MixByte(0x13), value) mustEqual MixWord(109000405L) // + 1 9 0 4 5
      word.updated(MixByte(0x14), value) mustEqual MixWord(108090005L) // + 1 8 9 0 5
      word.updated(MixByte(0x15), value) mustEqual MixWord(107080900L) // + 1 7 8 9 0
      word.updated(MixByte(0x1b), value) mustEqual MixWord(102000405L) // + 1 2 0 4 5
      word.updated(MixByte(0x1c), value) mustEqual MixWord(102090005L) // + 1 2 9 0 5
      word.updated(MixByte(0x1d), value) mustEqual MixWord(102080900L) // + 1 2 8 9 0
      word.updated(MixByte(0x24), value) mustEqual MixWord(102030005L) // + 1 2 3 0 5
      word.updated(MixByte(0x25), value) mustEqual MixWord(102030900L) // + 1 2 3 9 0
      word.updated(MixByte(0x2d), value) mustEqual MixWord(102030400L) // + 1 2 3 4 0
    }

    "update a field of a negative word" in {
      val word = MixWord(0x400000000L | 102030405L) // - 1 2 3 4 5
      val value = MixWord(607080900L) // + 6 7 8 9 0
      word.updated(MixByte(0x00), value) mustEqual MixWord(102030405L) // + 1 2 3 4 5
      word.updated(MixByte(0x01), value) mustEqual MixWord(2030405L) // + 0 2 3 4 5
      word.updated(MixByte(0x02), value) mustEqual MixWord(900030405L) // + 9 0 3 4 5
      word.updated(MixByte(0x03), value) mustEqual MixWord(809000405L) // + 8 9 0 4 5
      word.updated(MixByte(0x04), value) mustEqual MixWord(708090005L) // + 7 8 9 0 5
      word.updated(MixByte(0x05), value) mustEqual MixWord(607080900L) // + 6 7 8 9 0
      word.updated(MixByte(0x09), value) mustEqual MixWord(0x400000000L | 2030405L) // - 0 2 3 4 5
      word.updated(MixByte(0x0a), value) mustEqual MixWord(0x400000000L | 900030405L) // - 9 0 3 4 5
      word.updated(MixByte(0x0b), value) mustEqual MixWord(0x400000000L | 809000405L) // - 8 9 0 4 5
      word.updated(MixByte(0x0c), value) mustEqual MixWord(0x400000000L | 708090005L) // - 7 8 9 0 5
      word.updated(MixByte(0x0d), value) mustEqual MixWord(0x400000000L | 607080900L) // - 6 7 8 9 0
      word.updated(MixByte(0x12), value) mustEqual MixWord(0x400000000L | 100030405L) // - 1 0 3 4 5
      word.updated(MixByte(0x13), value) mustEqual MixWord(0x400000000L | 109000405L) // - 1 9 0 4 5
      word.updated(MixByte(0x14), value) mustEqual MixWord(0x400000000L | 108090005L) // - 1 8 9 0 5
      word.updated(MixByte(0x15), value) mustEqual MixWord(0x400000000L | 107080900L) // - 1 7 8 9 0
      word.updated(MixByte(0x1b), value) mustEqual MixWord(0x400000000L | 102000405L) // - 1 2 0 4 5
      word.updated(MixByte(0x1c), value) mustEqual MixWord(0x400000000L | 102090005L) // - 1 2 9 0 5
      word.updated(MixByte(0x1d), value) mustEqual MixWord(0x400000000L | 102080900L) // - 1 2 8 9 0
      word.updated(MixByte(0x24), value) mustEqual MixWord(0x400000000L | 102030005L) // - 1 2 3 0 5
      word.updated(MixByte(0x25), value) mustEqual MixWord(0x400000000L | 102030900L) // - 1 2 3 9 0
      word.updated(MixByte(0x2d), value) mustEqual MixWord(0x400000000L | 102030400L) // - 1 2 3 4 0
    }

    "throw an exception if field number is wrong" in {
      a[WrongFieldSpecException] must be thrownBy MixWord(0L).updated(MixByte(0x06), MixWord(0L))
    }

    "throw an exception if l > r in a field spec" in {
      a[WrongFieldSpecException] must be thrownBy MixWord(0L).updated(MixByte(0x08), MixWord(0L))
    }
  }

  "decimal double word field selection" should {
    "consider a positive number to be positive and not negative" in {
      // + 1 2 3 4 5 6 7 8 9 0
      MixDWord(102030405L, 607080900L).isPositive mustEqual true
      MixDWord(102030405L, 607080900L).isNegative mustEqual false
    }

    "consider a negative number to be negative and not positive" in {
      // - 1 2 3 4 5 6 7 8 9 0
      MixDWord(0x400000000L | 102030405L, 607080900L).isPositive mustEqual false
      MixDWord(0x400000000L | 102030405L, 607080900L).isNegative mustEqual true
    }

    "consider a positive zero to be positive and not negative" in {
      // + 0 0 0 0 0 0 0 0 0 0
      MixDWord(0L, 0L).isPositive mustEqual true
      MixDWord(0L, 0L).isNegative mustEqual false
    }

    "consider a negative zero to be negative and not positive" in {
      // - 0 0 0 0 0 0 0 0 0 0
      MixDWord(0x400000000L, 0L).isPositive mustEqual false
      MixDWord(0x400000000L, 0L).isNegative mustEqual true
    }
  }
}
