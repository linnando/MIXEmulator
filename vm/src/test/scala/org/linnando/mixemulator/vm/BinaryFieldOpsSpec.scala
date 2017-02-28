package org.linnando.mixemulator.vm

import org.specs2.mutable.Specification
import org.linnando.mixemulator.vm.BinaryVirtualMachine.{BinaryMixByte, BinaryMixIndex, BinaryMixWord}
import org.linnando.mixemulator.vm.exceptions.WrongFieldSpecException

class BinaryFieldOpsSpec extends Specification {
  "field selection" should {
    "select address from a positive word" in {
      // + 1 2 3 4 5 -> + 1 2
      BinaryMixWord(0x01083105).getAddress must be equalTo BinaryMixIndex(0x0042)
    }

    "select address from a negative word" in {
      // - 1 2 3 4 5 -> - 1 2
      BinaryMixWord(0x41083105).getAddress must be equalTo BinaryMixIndex(0x1042)
    }

    "select index spec from a word" in {
      // + 1 2 3 4 5 -> 3
      BinaryMixWord(0x01083105).getIndexSpec must be equalTo BinaryMixByte(0x03)
    }

    "select operation modifier from a word" in {
      // + 1 2 3 4 5 -> 3
      BinaryMixWord(0x01083105).getFieldSpec must be equalTo BinaryMixByte(0x04)
    }

    "select operation code from a word" in {
      // + 1 2 3 4 5 -> 5
      BinaryMixWord(0x01083105).getOpCode must be equalTo BinaryMixByte(0x05)
    }

    "select arbitrary field from a positive word" in {
      // + 1 2 3 4 5
      val word = BinaryMixWord(0x01083105)
      word.getField(BinaryMixByte(0x00)) must be equalTo BinaryMixWord(0x00000000) // + 0 0 0 0 0
      word.getField(BinaryMixByte(0x01)) must be equalTo BinaryMixWord(0x00000001) // + 0 0 0 0 1
      word.getField(BinaryMixByte(0x02)) must be equalTo BinaryMixWord(0x00000042) // + 0 0 0 1 2
      word.getField(BinaryMixByte(0x03)) must be equalTo BinaryMixWord(0x00001083) // + 0 0 1 2 3
      word.getField(BinaryMixByte(0x04)) must be equalTo BinaryMixWord(0x000420c4) // + 0 1 2 3 4
      word.getField(BinaryMixByte(0x05)) must be equalTo BinaryMixWord(0x01083105) // + 1 2 3 4 5
      word.getField(BinaryMixByte(0x09)) must be equalTo BinaryMixWord(0x00000001) // + 0 0 0 0 1
      word.getField(BinaryMixByte(0x0a)) must be equalTo BinaryMixWord(0x00000042) // + 0 0 0 1 2
      word.getField(BinaryMixByte(0x0b)) must be equalTo BinaryMixWord(0x00001083) // + 0 0 1 2 3
      word.getField(BinaryMixByte(0x0c)) must be equalTo BinaryMixWord(0x000420c4) // + 0 1 2 3 4
      word.getField(BinaryMixByte(0x0d)) must be equalTo BinaryMixWord(0x01083105) // + 1 2 3 4 5
      word.getField(BinaryMixByte(0x12)) must be equalTo BinaryMixWord(0x00000002) // + 0 0 0 0 2
      word.getField(BinaryMixByte(0x13)) must be equalTo BinaryMixWord(0x00000083) // + 0 0 0 2 3
      word.getField(BinaryMixByte(0x14)) must be equalTo BinaryMixWord(0x000020c4) // + 0 0 2 3 4
      word.getField(BinaryMixByte(0x15)) must be equalTo BinaryMixWord(0x00083105) // + 0 2 3 4 5
      word.getField(BinaryMixByte(0x1b)) must be equalTo BinaryMixWord(0x00000003) // + 0 0 0 0 3
      word.getField(BinaryMixByte(0x1c)) must be equalTo BinaryMixWord(0x000000c4) // + 0 0 0 3 4
      word.getField(BinaryMixByte(0x1d)) must be equalTo BinaryMixWord(0x00003105) // + 0 0 3 4 5
      word.getField(BinaryMixByte(0x24)) must be equalTo BinaryMixWord(0x00000004) // + 0 0 0 0 4
      word.getField(BinaryMixByte(0x25)) must be equalTo BinaryMixWord(0x00000105) // + 0 0 0 4 5
      word.getField(BinaryMixByte(0x2d)) must be equalTo BinaryMixWord(0x00000005) // + 0 0 0 0 5
    }

    "select arbitrary field from a negative word" in {
      // - 1 2 3 4 5
      val word = BinaryMixWord(0x41083105)
      word.getField(BinaryMixByte(0x00)) must be equalTo BinaryMixWord(0x40000000) // - 0 0 0 0 0
      word.getField(BinaryMixByte(0x01)) must be equalTo BinaryMixWord(0x40000001) // - 0 0 0 0 1
      word.getField(BinaryMixByte(0x02)) must be equalTo BinaryMixWord(0x40000042) // - 0 0 0 1 2
      word.getField(BinaryMixByte(0x03)) must be equalTo BinaryMixWord(0x40001083) // - 0 0 1 2 3
      word.getField(BinaryMixByte(0x04)) must be equalTo BinaryMixWord(0x400420c4) // - 0 1 2 3 4
      word.getField(BinaryMixByte(0x05)) must be equalTo BinaryMixWord(0x41083105) // - 1 2 3 4 5
      word.getField(BinaryMixByte(0x09)) must be equalTo BinaryMixWord(0x00000001) // + 0 0 0 0 1
      word.getField(BinaryMixByte(0x0a)) must be equalTo BinaryMixWord(0x00000042) // + 0 0 0 1 2
      word.getField(BinaryMixByte(0x0b)) must be equalTo BinaryMixWord(0x00001083) // + 0 0 1 2 3
      word.getField(BinaryMixByte(0x0c)) must be equalTo BinaryMixWord(0x000420c4) // + 0 1 2 3 4
      word.getField(BinaryMixByte(0x0d)) must be equalTo BinaryMixWord(0x01083105) // + 1 2 3 4 5
      word.getField(BinaryMixByte(0x12)) must be equalTo BinaryMixWord(0x00000002) // + 0 0 0 0 2
      word.getField(BinaryMixByte(0x13)) must be equalTo BinaryMixWord(0x00000083) // + 0 0 0 2 3
      word.getField(BinaryMixByte(0x14)) must be equalTo BinaryMixWord(0x000020c4) // + 0 0 2 3 4
      word.getField(BinaryMixByte(0x15)) must be equalTo BinaryMixWord(0x00083105) // + 0 2 3 4 5
      word.getField(BinaryMixByte(0x1b)) must be equalTo BinaryMixWord(0x00000003) // + 0 0 0 0 3
      word.getField(BinaryMixByte(0x1c)) must be equalTo BinaryMixWord(0x000000c4) // + 0 0 0 3 4
      word.getField(BinaryMixByte(0x1d)) must be equalTo BinaryMixWord(0x00003105) // + 0 0 3 4 5
      word.getField(BinaryMixByte(0x24)) must be equalTo BinaryMixWord(0x00000004) // + 0 0 0 0 4
      word.getField(BinaryMixByte(0x25)) must be equalTo BinaryMixWord(0x00000105) // + 0 0 0 4 5
      word.getField(BinaryMixByte(0x2d)) must be equalTo BinaryMixWord(0x00000005) // + 0 0 0 0 5
    }

    "throw an exception if field number is wrong" in {
      BinaryMixWord(0x0).getField(BinaryMixByte(0x06)) must throwA[WrongFieldSpecException]
    }

    "throw an exception if l > r in a field spec" in {
      BinaryMixWord(0x0).getField(BinaryMixByte(0x08)) must throwA[WrongFieldSpecException]
    }
  }
}
