package org.linnando.mixemulator.vm.io.data

import org.linnando.mixemulator.vm.VirtualMachine
import org.linnando.mixemulator.vm.exceptions.{UnsupportedCharacterException, WrongCharacterCodeException}

case class IOWord(negative: Boolean, bytes: Seq[Byte]) {
  def toChars: Seq[Char] = bytes map { code =>
    if (code >= 0 && code < VirtualMachine.CHARACTERS.length) VirtualMachine.CHARACTERS(code)
    else throw new WrongCharacterCodeException(code)
  }
}

object IOWord {
  def apply(chars: Seq[Char]): IOWord = IOWord(
    negative = false,
    chars map { char =>
      VirtualMachine.CODES.get(char) match {
        case Some(code) => code
        case None => throw new UnsupportedCharacterException(char)
      }
    }
  )
}
