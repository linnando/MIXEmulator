import { Component } from '@angular/core';
import { BackFromInitialStateException, DeviceNotConnectedException, DivisionByZeroException, DuplicateSymbolException, EndOfFileException, FixedFieldSpecException, ForwardFromTerminalStateException, InconsistentReadException, InvalidExpressionException, OverflowException, UndefinedSymbolException, UnpredictableExecutionFlowException, UnsupportedCharacterException, UnsupportedIoOperationException, UnsupportedPunchedCardCharacterException, WriteConflictException, WrongAddressPartException, WrongCharacterCodeException, WrongFieldSpecException, WrongIndexSpecException, WrongLabelException, WrongLineException, WrongMemoryAddressException, WrongOperatorException } from '@mixemulator/lib';
import { BsModalRef } from 'ngx-bootstrap/modal';

@Component({
  selector: 'mix-error-popup',
  templateUrl: './error-popup.component.html',
  styleUrls: ['./error-popup.component.scss']
})
export class ErrorPopupComponent {
  exception: unknown;

  constructor(public bsModalRef: BsModalRef) { }

  get errorMessage(): string {
    const exception = this.exception;
    if ((exception as BackFromInitialStateException).tag === 'BackFromInitialStateException') {
      return 'Cannot move back from the initial state';
    } else if ((exception as DeviceNotConnectedException).tag === 'DeviceNotConnectedException') {
      const e = exception as DeviceNotConnectedException;
      return `Device ${e.deviceNum} is not connected`;
    } else if ((exception as DivisionByZeroException).tag === 'DivisionByZeroException') {
      return 'Division by zero';
    } else if ((exception as DuplicateSymbolException).tag === 'DuplicateSymbolException') {
      const e = exception as DuplicateSymbolException;
      return `Duplicate symbol ${e.symbol}`;
    } else if ((exception as EndOfFileException).tag === 'EndOfFileException') {
      return 'Cannot read beyond the end of file';
    } else if ((exception as FixedFieldSpecException).tag === 'FixedFieldSpecException') {
      const e = exception as FixedFieldSpecException;
      return `Field specification for command ${e.operator} in line ${e.line} is used to distinguish commands and cannot be changed`;
    } else if ((exception as ForwardFromTerminalStateException).tag === 'ForwardFromTerminalStateException') {
      return 'Machine is halted';
    } else if ((exception as InconsistentReadException).tag === 'InconsistentReadException') {
      return 'Memory content is unpredictable as a device input/output operation is not synchronised';
    } else if ((exception as InvalidExpressionException).tag === 'InvalidExpressionException') {
      const e = exception as InvalidExpressionException;
      return `Invalid expression ${e.expression}`;
    } else if ((exception as OverflowException).tag === 'OverflowException') {
      return 'Register overflow';
    } else if ((exception as UndefinedSymbolException).tag === 'UndefinedSymbolException') {
      const e = exception as UndefinedSymbolException;
      return `Symbol ${e.symbol} is not defined`;
    } else if ((exception as UnpredictableExecutionFlowException).tag === 'UnpredictableExecutionFlowException') {
      return '`Execution sequence is unpredictable';
    } else if ((exception as UnsupportedCharacterException).tag === 'UnsupportedCharacterException') {
      const e = exception as UnsupportedCharacterException;
      return `Character ${e.char} is not supported by MIX`;
    } else if ((exception as UnsupportedIoOperationException).tag === 'UnsupportedIoOperationException') {
      const e = exception as UnsupportedIoOperationException;
      return `Device ${e.deviceNum} does not support operation ${e.operation}`;
    } else if ((exception as UnsupportedPunchedCardCharacterException).tag === 'UnsupportedPunchedCardCharacterException') {
      const e = exception as UnsupportedPunchedCardCharacterException;
      return `Character ${e.char} is not representable on punched cards`;
    } else if ((exception as WriteConflictException).tag === 'WriteConflictException') {
      return 'Cannot write to memory as a device input operation is not synchronised';
    } else if ((exception as WrongAddressPartException).tag === 'WrongAddressPartException') {
      const e = exception as WrongAddressPartException;
      return `Invalid address part ${e.addressPart} in line ${e.line}`;
    } else if ((exception as WrongCharacterCodeException).tag === 'WrongCharacterCodeException') {
      const e = exception as WrongCharacterCodeException;
      return `Code ${e.code} does not correspond to a character`;
    } else if ((exception as WrongFieldSpecException).tag === 'WrongFieldSpecException') {
      const e = exception as WrongFieldSpecException;
      return `Invalid field specification ${e.fieldSpec}`;
    } else if ((exception as WrongIndexSpecException).tag === 'WrongIndexSpecException') {
      const e = exception as WrongIndexSpecException;
      return `Invalid index specification ${e.indexSpec}`;
    } else if ((exception as WrongLabelException).tag === 'WrongLabelException') {
      const e = exception as WrongLabelException;
      return `Invalid label ${e.label}`;
    } else if ((exception as WrongLineException).tag === 'WrongLineException') {
      const e = exception as WrongLineException;
      return `Wrong line format in line ${e.line}`;
    } else if ((exception as WrongOperatorException).tag === 'WrongOperatorException') {
      const e = exception as WrongOperatorException;
      return `Invalid command ${e.operator} in line ${e.line}`;
    } else if ((exception as WrongMemoryAddressException).tag === 'WrongMemoryAddressException') {
      const e = exception as WrongMemoryAddressException;
      return `Invalid memory address ${e.address}`;
    } else {
      console.log(exception);
      return 'Unexpected error';
    }
  }
}
