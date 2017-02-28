package org.linnando.mixemulator.vm.io.data

case class IOWord(negative: Boolean, bytes: Seq[Byte]) {
  def toChars: Seq[Char] = bytes.map(IOWord.CHARACTERS(_))
}

object IOWord {
  private val CHARACTERS = Array(
    ' ', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
    'H', 'I', '\u0394', 'J', 'K', 'L', 'M', 'N',
    'O', 'P', 'Q', 'R', '\u03a3', '\u03a0', 'S', 'T',
    'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1',
    '2', '3', '4', '5', '6', '7', '8', '9',
    '.', ',', '(', ')', '+', '-', '*', '/',
    '=', '$', '<', '>', '@', ';', ':', '\''
  )
  private val CODES = Map[Char, Byte](
    ' ' -> 0, 'A' -> 1, 'B' -> 2, 'C' -> 3, 'D' -> 4, 'E' -> 5, 'F' -> 6, 'G' -> 7,
    'H' -> 8, 'I' -> 9, '\u0394' -> 10, 'J' -> 11, 'K' -> 12, 'L' -> 13, 'M' -> 14, 'N' -> 15,
    'O' -> 16, 'P' -> 17, 'Q' -> 18, 'R' -> 19, '\u03a3' -> 20, '\u03a0' -> 21, 'S' -> 22, 'T' -> 23,
    'U' -> 24, 'V' -> 25, 'W' -> 26, 'X' -> 27, 'Y' -> 28, 'Z' -> 29, '0' -> 30, '1' -> 31,
    '2' -> 32, '3' -> 33, '4' -> 34, '5' -> 35, '6' -> 36, '7' -> 37, '8' -> 38, '9' -> 39,
    '.' -> 40, ',' -> 41, '(' -> 42, ')' -> 43, '+' -> 44, '-' -> 45, '*' -> 46, '/' -> 47,
    '=' -> 48, '$' -> 49, '<' -> 50, '>' -> 51, '@' -> 52, ';' -> 53, ':' -> 54, '\'' -> 55)

  def apply(chars: Seq[Char]): IOWord =
    IOWord(negative = false, chars.map(CODES))
}
