package org.linnando.mixemulator.vm.io.data

import org.linnando.mixemulator.vm.exceptions.{UnsupportedCharacterException, WrongCharacterCodeException}
import org.specs2.mutable.Specification

class CharCodeSpec extends Specification {
  "character code conversion" should {
    "convert characters to bytes" in {
      IOWord(Seq(' ', 'A', 'B', 'C', 'D')) must be equalTo IOWord(negative = false, Seq(0, 1, 2, 3, 4))
      IOWord(Seq('E', 'F', 'G', 'H', 'I')) must be equalTo IOWord(negative = false, Seq(5, 6, 7, 8, 9))
      IOWord(Seq('\u0394', 'J', 'K', 'L', 'M')) must be equalTo IOWord(negative = false, Seq(10, 11, 12, 13, 14))
      IOWord(Seq('N', 'O', 'P', 'Q', 'R')) must be equalTo IOWord(negative = false, Seq(15, 16, 17, 18, 19))
      IOWord(Seq('\u03a3', '\u03a0', 'S', 'T', 'U')) must be equalTo IOWord(negative = false, Seq(20, 21, 22, 23, 24))
      IOWord(Seq('V', 'W', 'X', 'Y', 'Z')) must be equalTo IOWord(negative = false, Seq(25, 26, 27, 28, 29))
      IOWord(Seq('0', '1', '2', '3', '4')) must be equalTo IOWord(negative = false, Seq(30, 31, 32, 33, 34))
      IOWord(Seq('5', '6', '7', '8', '9')) must be equalTo IOWord(negative = false, Seq(35, 36, 37, 38, 39))
      IOWord(Seq('.', ',', '(', ')', '+')) must be equalTo IOWord(negative = false, Seq(40, 41, 42, 43, 44))
      IOWord(Seq('-', '*', '/', '=', '$')) must be equalTo IOWord(negative = false, Seq(45, 46, 47, 48, 49))
      IOWord(Seq('<', '>', '@', ';', ':')) must be equalTo IOWord(negative = false, Seq(50, 51, 52, 53, 54))
      IOWord(Seq('\'', ' ', 'A', 'B', 'C')) must be equalTo IOWord(negative = false, Seq(55, 0, 1, 2, 3))
    }

    "throw an exception for unsupported characters" in {
      IOWord(Seq(' ', 'A', 'B', 'C', 'd')) must throwAn[UnsupportedCharacterException].like {
        case e: UnsupportedCharacterException => e.char must be equalTo 'd'
      }
    }

    "convert bytes to characters" in {
      IOWord(negative = false, Seq(0, 1, 2, 3, 4)).toChars must be equalTo Seq(' ', 'A', 'B', 'C', 'D')
      IOWord(negative = false, Seq(5, 6, 7, 8, 9)).toChars must be equalTo Seq('E', 'F', 'G', 'H', 'I')
      IOWord(negative = false, Seq(10, 11, 12, 13, 14)).toChars must be equalTo Seq('\u0394', 'J', 'K', 'L', 'M')
      IOWord(negative = false, Seq(15, 16, 17, 18, 19)).toChars must be equalTo Seq('N', 'O', 'P', 'Q', 'R')
      IOWord(negative = false, Seq(20, 21, 22, 23, 24)).toChars must be equalTo Seq('\u03a3', '\u03a0', 'S', 'T', 'U')
      IOWord(negative = false, Seq(25, 26, 27, 28, 29)).toChars must be equalTo Seq('V', 'W', 'X', 'Y', 'Z')
      IOWord(negative = false, Seq(30, 31, 32, 33, 34)).toChars must be equalTo Seq('0', '1', '2', '3', '4')
      IOWord(negative = false, Seq(35, 36, 37, 38, 39)).toChars must be equalTo Seq('5', '6', '7', '8', '9')
      IOWord(negative = false, Seq(40, 41, 42, 43, 44)).toChars must be equalTo Seq('.', ',', '(', ')', '+')
      IOWord(negative = false, Seq(45, 46, 47, 48, 49)).toChars must be equalTo Seq('-', '*', '/', '=', '$')
      IOWord(negative = false, Seq(50, 51, 52, 53, 54)).toChars must be equalTo Seq('<', '>', '@', ';', ':')
      IOWord(negative = false, Seq(55, 0, 1, 2, 3)).toChars must be equalTo Seq('\'', ' ', 'A', 'B', 'C')
    }

    "ignore word signs" in {
      IOWord(negative = true, Seq(0, 1, 2, 3, 4)).toChars must be equalTo Seq(' ', 'A', 'B', 'C', 'D')
      IOWord(negative = true, Seq(5, 6, 7, 8, 9)).toChars must be equalTo Seq('E', 'F', 'G', 'H', 'I')
      IOWord(negative = true, Seq(10, 11, 12, 13, 14)).toChars must be equalTo Seq('\u0394', 'J', 'K', 'L', 'M')
      IOWord(negative = true, Seq(15, 16, 17, 18, 19)).toChars must be equalTo Seq('N', 'O', 'P', 'Q', 'R')
      IOWord(negative = true, Seq(20, 21, 22, 23, 24)).toChars must be equalTo Seq('\u03a3', '\u03a0', 'S', 'T', 'U')
      IOWord(negative = true, Seq(25, 26, 27, 28, 29)).toChars must be equalTo Seq('V', 'W', 'X', 'Y', 'Z')
      IOWord(negative = true, Seq(30, 31, 32, 33, 34)).toChars must be equalTo Seq('0', '1', '2', '3', '4')
      IOWord(negative = true, Seq(35, 36, 37, 38, 39)).toChars must be equalTo Seq('5', '6', '7', '8', '9')
      IOWord(negative = true, Seq(40, 41, 42, 43, 44)).toChars must be equalTo Seq('.', ',', '(', ')', '+')
      IOWord(negative = true, Seq(45, 46, 47, 48, 49)).toChars must be equalTo Seq('-', '*', '/', '=', '$')
      IOWord(negative = true, Seq(50, 51, 52, 53, 54)).toChars must be equalTo Seq('<', '>', '@', ';', ':')
      IOWord(negative = true, Seq(55, 0, 1, 2, 3)).toChars must be equalTo Seq('\'', ' ', 'A', 'B', 'C')
    }

    "throw an exception for wrong character codes" in {
      IOWord(negative = false, Seq(55, 56, 57, 58, 59)).toChars must throwA[WrongCharacterCodeException].like {
        case e: WrongCharacterCodeException => e.code must be equalTo 56
      }
    }
  }
}
