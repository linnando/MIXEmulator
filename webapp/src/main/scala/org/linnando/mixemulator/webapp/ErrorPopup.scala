package org.linnando.mixemulator.webapp

import java.util.concurrent.ExecutionException

import com.karasiq.bootstrap.Bootstrap.default._
import org.linnando.mixemulator.asm.exceptions._
import org.linnando.mixemulator.vm.exceptions._

import scalaTags.all._

object ErrorPopup {
  def show(exception: Throwable): Unit = {
    Modal()
      .withTitle("Error")
      .withBody(errorMessage(exception))
      .withButtons(Modal.closeButton())
      .withDialogStyle(ModalDialogSize.large)
      .show()
  }

  private def errorMessage(exception: Throwable): String = exception match {
    case _: BackFromInitialStateException => "Cannot move back from the initial state"
    case e: DeviceNotConnectedException => s"Device ${e.deviceNum} is not connected"
    case _: DivisionByZeroException => "Division by zero"
    case e: DuplicateSymbolException => s"Duplicate symbol ${e.symbol}"
    case _: EndOfFileException => "Cannot read beyond the end of file"
    case e: FixedFieldSpecException =>
      s"Field specification for command ${e.operator} in line ${e.line} is used to distinguish commands" +
        s" and cannot be changed"
    case _: ForwardFromTerminalStateException => "Machine is halted"
    case _: InconsistentReadException =>
      "Memory content is unpredictable as a device input/output operation is not synchronised"
    case e: InvalidExpressionException => s"Invalid expression ${e.expression}"
    case _: OverflowException => "Register overflow"
    case e: UndefinedSymbolException => s"Symbol ${e.symbol} is not defined"
    case _: UnpredictableExecutionFlowException => "Execution sequence is unpredictable"
    case e: UnsupportedCharacterException => s"Character ${e.char} is not supported by MIX"
    case _: WriteConflictException => "Cannot write to memory as a device input operation is not synchronised"
    case e: WrongAddressPartException => s"Invalid address part ${e.addressPart} in line ${e.line}"
    case e: WrongCharacterCodeException => s"Code ${e.code} does not correspond to a character"
    case e: WrongComputedMemoryAddressException => s"Invalid memory address ${e.address} in line ${e.line}"
    case e: WrongFieldSpecException => s"Invalid field specification ${e.fieldSpec}"
    case e: WrongIndexSpecException => s"Invalid index specification ${e.indexSpec}"
    case e: WrongLabelException => s"Invalid label ${e.label}"
    case e: WrongLineException => s"Wrong line format in line ${e.line}"
    case e: WrongOperatorException => s"Invalid command ${e.operator} in line ${e.line}"
    case e: WrongMemoryAddressException => s"Invalid memory address ${e.address}"
    case e: ExecutionException =>
      println(e.getCause)
      "Unexpected error"
    case e =>
      println(e)
      "Unexpected error"
  }
}
