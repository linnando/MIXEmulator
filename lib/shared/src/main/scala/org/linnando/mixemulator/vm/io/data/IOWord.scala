package org.linnando.mixemulator.vm.io.data

import org.linnando.mixemulator.vm.VirtualMachine
import org.linnando.mixemulator.vm.exceptions.{UnsupportedCharacterException, WrongCharacterCodeException}

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("IOWord")
case class IOWord(@JSExport negative: Boolean, bytes: Seq[Byte]) {
  @JSExport("bytes")
  def bytesJs: js.Array[Byte] = bytes.toJSArray

  def toChars: Seq[Char] = bytes map { code =>
    if (code >= 0 && code < VirtualMachine.CHARACTERS.length) VirtualMachine.CHARACTERS(code)
    else throw new WrongCharacterCodeException(code)
  }
}

@JSExportTopLevel("IOWord$")
object IOWord {
  @JSExport
  def empty(): IOWord = IOWord(negative = false, bytes = Seq(0, 0, 0, 0, 0))

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
