package org.linnando.mixemulator.vm.decimalvm.datamodel

import org.linnando.mixemulator.vm.decimal._
import org.linnando.mixemulator.vm.exceptions.OverflowException
import org.linnando.mixemulator.vm.io.data.IOWord
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class ConversionOpsSpec extends AsyncWordSpec with Matchers {
  "decimal integer types conversion" should {
    "convert a Byte to a MIX byte" in {
      getByte(1) mustEqual MixByte(1)
      getByte(99) mustEqual MixByte(99)
    }

    "convert a Short to a MIX index" in {
      getIndex(1) mustEqual MixIndex(1)
      getIndex(9999) mustEqual MixIndex(9999)
      getIndex(-1) mustEqual MixIndex(0x4001)
      getIndex(-9999) mustEqual MixIndex((0x4000 | 9999).toShort)
    }

    "convert an Long to a MIX word" in {
      getWord(1L) mustEqual MixWord(1L)
      getWord(9999999999L) mustEqual MixWord(9999999999L)
      getWord(-1L) mustEqual MixWord(0x400000001L)
      getWord(-9999999999L) mustEqual MixWord(0x400000000L | 9999999999L)
    }

    "convert a zero to a positive zero" in {
      getByte(0) mustEqual MixByte(0)
      getIndex(0) mustEqual MixIndex(0)
      getWord(0L) mustEqual MixWord(0L)
    }

    "throw an exception if a negative value is converted to a MIX byte" in {
      an[OverflowException] must be thrownBy getByte(-1)
    }

    "throw an exception if the value is too big" in {
      an[OverflowException] must be thrownBy getByte(100)
      an[OverflowException] must be thrownBy getIndex(10000)
      an[OverflowException] must be thrownBy getIndex(-10000)
      an[OverflowException] must be thrownBy getWord(10000000000L)
      an[OverflowException] must be thrownBy getWord(-10000000000L)
    }
  }

  "decimal byte conversion" should {
    "convert a MIX byte to a Byte" in {
      MixByte(1).toByte mustEqual 1
    }

    "convert a MIX byte to an Int" in {
      MixByte(1).toInt mustEqual 1
    }

    "define zero as zero" in {
      MixByte(0).isZero mustEqual true
    }

    "define one as non-zero" in {
      MixByte(1).isZero mustEqual false
    }
  }

  "decimal index conversion" should {
    "convert a positive index to a word" in {
      MixIndex(1).toWord mustEqual MixWord(1)
    }

    "convert a negative index to a word" in {
      MixIndex(0x4001).toWord mustEqual MixWord(0x400000001L)
    }

    "convert a positive index to a Short" in {
      MixIndex(1).toShort mustEqual 1
    }

    "convert a negative index to a Short" in {
      MixIndex(0x4001).toShort mustEqual -1
    }
  }

  "decimal word conversion" should {
    "convert a positive word to a byte" in {
      MixWord(1L).toByte mustEqual MixByte(1)
    }

    "throw an exception if a negative value is converted to a byte" in {
      an[OverflowException] must be thrownBy MixWord(0x400000001L).toByte
    }

    "throw an exception if the value is too big for a byte" in {
      an[OverflowException] must be thrownBy MixWord(100L).toByte
    }

    "convert a positive word to an index" in {
      MixWord(1L).toIndex mustEqual MixIndex(1)
    }

    "convert a negative word to an index" in {
      MixWord(0x400000001L).toIndex mustEqual MixIndex(0x4001)
    }

    "throw an exception if the value is too big for an index" in {
      an[OverflowException] must be thrownBy MixWord(10000L).toIndex
    }

    "convert a positive word to a Long" in {
      MixWord(1).toLong mustEqual 1L
    }

    "convert a negative word to a Long" in {
      MixWord(0x400000001L).toLong mustEqual -1L
    }
  }

  "decimal double word conversion" should {
    "convert a positive double word to a word" in {
      MixDWord(0L, 1L).toWord mustEqual MixWord(1)
    }

    "convert a negative double word to a word" in {
      MixDWord(0x400000000L, 1L).toWord mustEqual MixWord(0x400000001L)
    }

    "throw an exception if the value is too big for a word" in {
      an[OverflowException] must be thrownBy MixDWord(1L, 0L).toWord
    }
  }

  "decimal i/o word conversion" should {
    "convert a positive index to an i/o word" in {
      MixIndex(102).toIOWord mustEqual IOWord(negative = false, Vector(0, 0, 0, 1, 2))
    }

    "convert a negative index to an i/o word" in {
      MixIndex((0x4000 | 102).toShort).toIOWord mustEqual IOWord(negative = true, Vector(0, 0, 0, 1, 2))
    }

    "convert a positive word to an i/o word" in {
      MixWord(102030405L).toIOWord mustEqual IOWord(negative = false, Vector(1, 2, 3, 4, 5))
    }

    "convert a negative word to an i/o word" in {
      MixWord(0x400000000L + 102030405L).toIOWord mustEqual IOWord(negative = true, Vector(1, 2, 3, 4, 5))
    }

    "convert a positive i/o word to a word" in {
      getWord(IOWord(negative = false, Seq(1, 2, 3, 4, 5))) mustEqual MixWord(102030405L)
    }

    "convert a negative i/o word to a word" in {
      getWord(IOWord(negative = true, Seq(1, 2, 3, 4, 5))) mustEqual MixWord(0x400000000L + 102030405L)
    }
  }

  "decimal character code conversion" should {
    "convert alphanumeric characters to a number" in {
      getWord("A1\u03949@") mustEqual MixWord(131103952L)
    }

    "convert a character code to a positive number" in {
      // + 0 0 31 32 39 37 57 47 30 30 -> 12977700
      MixDWord(313239L, 3757473030L).charToNumber mustEqual MixWord(12977700L)
    }

    "convert a character code to a negative number" in {
      // - 0 0 31 32 39 37 57 47 30 30 -> -12977700
      MixDWord(0x400000000L | 313239L, 3757473030L).charToNumber mustEqual MixWord(0x400000000L | 12977700L)
    }

    "convert a positive number to character code" in {
      // 12977699 -> + 30 30 31 32 39 37 37 36 39 39
      MixWord(12977699).toCharCode mustEqual MixDWord(3030313239L, 3737363939L)
    }

    "convert a negative number to character code" in {
      // -12977699 -> - 30 30 31 32 39 37 37 36 39 39
      MixWord(0x400000000L | 12977699L).toCharCode mustEqual MixDWord(0x400000000L | 3030313239L, 3737363939L)
    }
  }
}
