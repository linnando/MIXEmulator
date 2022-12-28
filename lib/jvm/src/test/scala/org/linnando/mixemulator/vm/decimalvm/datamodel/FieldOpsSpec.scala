package org.linnando.mixemulator.vm.decimalvm.datamodel

import org.linnando.mixemulator.vm.decimal._
import org.linnando.mixemulator.vm.exceptions.WrongFieldSpecException
import org.specs2.mutable.Specification

class FieldOpsSpec extends Specification {
  "decimal index field selection" should {
    "consider a positive number to be positive and not negative" in {
      // + 1 2
      MixIndex(102).isPositive must beTrue
      MixIndex(102).isNegative must beFalse
    }

    "consider a negative number to be negative and not positive" in {
      // - 1 2
      MixIndex((0x4000 | 102).toShort).isPositive must beFalse
      MixIndex((0x4000 | 102).toShort).isNegative must beTrue
    }

    "consider a positive zero to be positive and not negative" in {
      // + 0 0
      MixIndex(0).isPositive must beTrue
      MixIndex(0).isNegative must beFalse
    }

    "consider a negative zero to be negative and not positive" in {
      // - 0 0
      MixIndex(0x4000).isPositive must beFalse
      MixIndex(0x4000).isNegative must beTrue
    }
  }

  "decimal word building" should {
    "collect a word from fields" in {
      getWord(getIndex(-1000), getByte(1), getByte(5), getByte(8)) must be equalTo MixWord(0x400000000L | 1000010508L)
    }
  }

  "decimal word field selection" should {
    "consider a positive number to be positive and not negative" in {
      // + 1 2 3 4 5
      MixWord(102030405L).isPositive must beTrue
      MixWord(102030405L).isNegative must beFalse
    }

    "consider a negative number to be negative and not positive" in {
      // - 1 2 3 4 5
      MixWord(0x400000000L | 102030405L).isPositive must beFalse
      MixWord(0x400000000L | 102030405L).isNegative must beTrue
    }

    "consider a positive zero to be positive and not negative" in {
      // + 0 0 0 0 0
      MixWord(0).isPositive must beTrue
      MixWord(0).isNegative must beFalse
    }

    "consider a negative zero to be negative and not positive" in {
      // - 0 0 0 0 0
      MixWord(0x400000000L).isPositive must beFalse
      MixWord(0x400000000L).isNegative must beTrue
    }

    "select address from a positive word" in {
      // + 1 2 3 4 5 -> + 1 2
      MixWord(102030405L).getAddress must be equalTo MixIndex(102)
    }

    "select address from a negative word" in {
      // - 1 2 3 4 5 -> - 1 2
      MixWord(0x400000000L | 102030405L).getAddress must be equalTo MixIndex((0x4000 | 102).toShort)
    }

    "select index spec from a word" in {
      // + 1 2 3 4 5 -> 3
      MixWord(102030405).getIndexSpec must be equalTo MixByte(3)
    }

    "select operation modifier from a word" in {
      // + 1 2 3 4 5 -> 4
      MixWord(102030405).getFieldSpec must be equalTo MixByte(4)
    }

    "select operation code from a word" in {
      // + 1 2 3 4 5 -> 5
      MixWord(102030405).getOpCode must be equalTo MixByte(5)
    }

    "select arbitrary field from a positive word" in {
      // + 1 2 3 4 5
      val word = MixWord(102030405)
      word.getField(MixByte(0x00)) must be equalTo MixWord(0L) // + 0 0 0 0 0
      word.getField(MixByte(0x01)) must be equalTo MixWord(1L) // + 0 0 0 0 1
      word.getField(MixByte(0x02)) must be equalTo MixWord(102L) // + 0 0 0 1 2
      word.getField(MixByte(0x03)) must be equalTo MixWord(10203L) // + 0 0 1 2 3
      word.getField(MixByte(0x04)) must be equalTo MixWord(1020304L) // + 0 1 2 3 4
      word.getField(MixByte(0x05)) must be equalTo MixWord(102030405L) // + 1 2 3 4 5
      word.getField(MixByte(0x09)) must be equalTo MixWord(1L) // + 0 0 0 0 1
      word.getField(MixByte(0x0a)) must be equalTo MixWord(102L) // + 0 0 0 1 2
      word.getField(MixByte(0x0b)) must be equalTo MixWord(10203L) // + 0 0 1 2 3
      word.getField(MixByte(0x0c)) must be equalTo MixWord(1020304L) // + 0 1 2 3 4
      word.getField(MixByte(0x0d)) must be equalTo MixWord(102030405L) // + 1 2 3 4 5
      word.getField(MixByte(0x12)) must be equalTo MixWord(2L) // + 0 0 0 0 2
      word.getField(MixByte(0x13)) must be equalTo MixWord(203L) // + 0 0 0 2 3
      word.getField(MixByte(0x14)) must be equalTo MixWord(20304L) // + 0 0 2 3 4
      word.getField(MixByte(0x15)) must be equalTo MixWord(2030405L) // + 0 2 3 4 5
      word.getField(MixByte(0x1b)) must be equalTo MixWord(3L) // + 0 0 0 0 3
      word.getField(MixByte(0x1c)) must be equalTo MixWord(304L) // + 0 0 0 3 4
      word.getField(MixByte(0x1d)) must be equalTo MixWord(30405L) // + 0 0 3 4 5
      word.getField(MixByte(0x24)) must be equalTo MixWord(4L) // + 0 0 0 0 4
      word.getField(MixByte(0x25)) must be equalTo MixWord(405L) // + 0 0 0 4 5
      word.getField(MixByte(0x2d)) must be equalTo MixWord(5L) // + 0 0 0 0 5
    }

    "select arbitrary field from a negative word" in {
      // - 1 2 3 4 5
      val word = MixWord(0x400000000L | 102030405L)
      word.getField(MixByte(0x00)) must be equalTo MixWord(0x400000000L) // - 0 0 0 0 0
      word.getField(MixByte(0x01)) must be equalTo MixWord(0x400000000L | 1L) // - 0 0 0 0 1
      word.getField(MixByte(0x02)) must be equalTo MixWord(0x400000000L | 102L) // - 0 0 0 1 2
      word.getField(MixByte(0x03)) must be equalTo MixWord(0x400000000L | 10203L) // - 0 0 1 2 3
      word.getField(MixByte(0x04)) must be equalTo MixWord(0x400000000L | 1020304L) // - 0 1 2 3 4
      word.getField(MixByte(0x05)) must be equalTo MixWord(0x400000000L | 102030405L) // - 1 2 3 4 5
      word.getField(MixByte(0x09)) must be equalTo MixWord(1L) // + 0 0 0 0 1
      word.getField(MixByte(0x0a)) must be equalTo MixWord(102L) // + 0 0 0 1 2
      word.getField(MixByte(0x0b)) must be equalTo MixWord(10203L) // + 0 0 1 2 3
      word.getField(MixByte(0x0c)) must be equalTo MixWord(1020304L) // + 0 1 2 3 4
      word.getField(MixByte(0x0d)) must be equalTo MixWord(102030405L) // + 1 2 3 4 5
      word.getField(MixByte(0x12)) must be equalTo MixWord(2L) // + 0 0 0 0 2
      word.getField(MixByte(0x13)) must be equalTo MixWord(203L) // + 0 0 0 2 3
      word.getField(MixByte(0x14)) must be equalTo MixWord(20304L) // + 0 0 2 3 4
      word.getField(MixByte(0x15)) must be equalTo MixWord(2030405L) // + 0 2 3 4 5
      word.getField(MixByte(0x1b)) must be equalTo MixWord(3L) // + 0 0 0 0 3
      word.getField(MixByte(0x1c)) must be equalTo MixWord(304L) // + 0 0 0 3 4
      word.getField(MixByte(0x1d)) must be equalTo MixWord(30405L) // + 0 0 3 4 5
      word.getField(MixByte(0x24)) must be equalTo MixWord(4L) // + 0 0 0 0 4
      word.getField(MixByte(0x25)) must be equalTo MixWord(405L) // + 0 0 0 4 5
      word.getField(MixByte(0x2d)) must be equalTo MixWord(5L) // + 0 0 0 0 5
    }

    "throw an exception if field number is wrong" in {
      MixWord(0L).getField(MixByte(0x06)) must throwA[WrongFieldSpecException]
    }

    "throw an exception if l > r in a field spec" in {
      MixWord(0L).getField(MixByte(0x08)) must throwA[WrongFieldSpecException]
    }
  }

  "decimal word field update" should {
    "update a field of a positive word" in {
      val word = MixWord(102030405L) // + 1 2 3 4 5
      val value = MixWord(607080900L) // + 6 7 8 9 0
      word.updated(MixByte(0x00), value) must be equalTo MixWord(102030405L) // + 1 2 3 4 5
      word.updated(MixByte(0x01), value) must be equalTo MixWord(2030405L) // + 0 2 3 4 5
      word.updated(MixByte(0x02), value) must be equalTo MixWord(900030405L) // + 9 0 3 4 5
      word.updated(MixByte(0x03), value) must be equalTo MixWord(809000405L) // + 8 9 0 4 5
      word.updated(MixByte(0x04), value) must be equalTo MixWord(708090005L) // + 7 8 9 0 5
      word.updated(MixByte(0x05), value) must be equalTo MixWord(607080900L) // + 6 7 8 9 0
      word.updated(MixByte(0x09), value) must be equalTo MixWord(2030405L) // + 0 2 3 4 5
      word.updated(MixByte(0x0a), value) must be equalTo MixWord(900030405L) // + 9 0 3 4 5
      word.updated(MixByte(0x0b), value) must be equalTo MixWord(809000405L) // + 8 9 0 4 5
      word.updated(MixByte(0x0c), value) must be equalTo MixWord(708090005L) // + 7 8 9 0 5
      word.updated(MixByte(0x0d), value) must be equalTo MixWord(607080900L) // + 6 7 8 9 0
      word.updated(MixByte(0x12), value) must be equalTo MixWord(100030405L) // + 1 0 3 4 5
      word.updated(MixByte(0x13), value) must be equalTo MixWord(109000405L) // + 1 9 0 4 5
      word.updated(MixByte(0x14), value) must be equalTo MixWord(108090005L) // + 1 8 9 0 5
      word.updated(MixByte(0x15), value) must be equalTo MixWord(107080900L) // + 1 7 8 9 0
      word.updated(MixByte(0x1b), value) must be equalTo MixWord(102000405L) // + 1 2 0 4 5
      word.updated(MixByte(0x1c), value) must be equalTo MixWord(102090005L) // + 1 2 9 0 5
      word.updated(MixByte(0x1d), value) must be equalTo MixWord(102080900L) // + 1 2 8 9 0
      word.updated(MixByte(0x24), value) must be equalTo MixWord(102030005L) // + 1 2 3 0 5
      word.updated(MixByte(0x25), value) must be equalTo MixWord(102030900L) // + 1 2 3 9 0
      word.updated(MixByte(0x2d), value) must be equalTo MixWord(102030400L) // + 1 2 3 4 0
    }

    "update a field of a negative word" in {
      val word = MixWord(0x400000000L | 102030405L) // - 1 2 3 4 5
      val value = MixWord(607080900L) // + 6 7 8 9 0
      word.updated(MixByte(0x00), value) must be equalTo MixWord(102030405L) // + 1 2 3 4 5
      word.updated(MixByte(0x01), value) must be equalTo MixWord(2030405L) // + 0 2 3 4 5
      word.updated(MixByte(0x02), value) must be equalTo MixWord(900030405L) // + 9 0 3 4 5
      word.updated(MixByte(0x03), value) must be equalTo MixWord(809000405L) // + 8 9 0 4 5
      word.updated(MixByte(0x04), value) must be equalTo MixWord(708090005L) // + 7 8 9 0 5
      word.updated(MixByte(0x05), value) must be equalTo MixWord(607080900L) // + 6 7 8 9 0
      word.updated(MixByte(0x09), value) must be equalTo MixWord(0x400000000L | 2030405L) // - 0 2 3 4 5
      word.updated(MixByte(0x0a), value) must be equalTo MixWord(0x400000000L | 900030405L) // - 9 0 3 4 5
      word.updated(MixByte(0x0b), value) must be equalTo MixWord(0x400000000L | 809000405L) // - 8 9 0 4 5
      word.updated(MixByte(0x0c), value) must be equalTo MixWord(0x400000000L | 708090005L) // - 7 8 9 0 5
      word.updated(MixByte(0x0d), value) must be equalTo MixWord(0x400000000L | 607080900L) // - 6 7 8 9 0
      word.updated(MixByte(0x12), value) must be equalTo MixWord(0x400000000L | 100030405L) // - 1 0 3 4 5
      word.updated(MixByte(0x13), value) must be equalTo MixWord(0x400000000L | 109000405L) // - 1 9 0 4 5
      word.updated(MixByte(0x14), value) must be equalTo MixWord(0x400000000L | 108090005L) // - 1 8 9 0 5
      word.updated(MixByte(0x15), value) must be equalTo MixWord(0x400000000L | 107080900L) // - 1 7 8 9 0
      word.updated(MixByte(0x1b), value) must be equalTo MixWord(0x400000000L | 102000405L) // - 1 2 0 4 5
      word.updated(MixByte(0x1c), value) must be equalTo MixWord(0x400000000L | 102090005L) // - 1 2 9 0 5
      word.updated(MixByte(0x1d), value) must be equalTo MixWord(0x400000000L | 102080900L) // - 1 2 8 9 0
      word.updated(MixByte(0x24), value) must be equalTo MixWord(0x400000000L | 102030005L) // - 1 2 3 0 5
      word.updated(MixByte(0x25), value) must be equalTo MixWord(0x400000000L | 102030900L) // - 1 2 3 9 0
      word.updated(MixByte(0x2d), value) must be equalTo MixWord(0x400000000L | 102030400L) // - 1 2 3 4 0
    }

    "throw an exception if field number is wrong" in {
      MixWord(0L).updated(MixByte(0x06), MixWord(0L)) must throwA[WrongFieldSpecException]
    }

    "throw an exception if l > r in a field spec" in {
      MixWord(0L).updated(MixByte(0x08), MixWord(0L)) must throwA[WrongFieldSpecException]
    }
  }

  "decimal double word field selection" should {
    "consider a positive number to be positive and not negative" in {
      // + 1 2 3 4 5 6 7 8 9 0
      MixDWord(102030405L, 607080900L).isPositive must beTrue
      MixDWord(102030405L, 607080900L).isNegative must beFalse
    }

    "consider a negative number to be negative and not positive" in {
      // - 1 2 3 4 5 6 7 8 9 0
      MixDWord(0x400000000L | 102030405L, 607080900L).isPositive must beFalse
      MixDWord(0x400000000L | 102030405L, 607080900L).isNegative must beTrue
    }

    "consider a positive zero to be positive and not negative" in {
      // + 0 0 0 0 0 0 0 0 0 0
      MixDWord(0L, 0L).isPositive must beTrue
      MixDWord(0L, 0L).isNegative must beFalse
    }

    "consider a negative zero to be negative and not positive" in {
      // - 0 0 0 0 0 0 0 0 0 0
      MixDWord(0x400000000L, 0L).isPositive must beFalse
      MixDWord(0x400000000L, 0L).isNegative must beTrue
    }
  }
}
