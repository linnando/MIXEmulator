/**
 * Amount of memory available to a MIX virtual machine, in MIX words.
 */
export const MEMORY_SIZE: number;

/**
 * Class representing a MIX virtual machine to the client.
 */
export class VirtualMachineFrontEnd {
  currentState: VirtualMachineState;

  /** Whether the machine can execute the stepForward and runForward commands. */
  canMoveForward: boolean;

  /**
   * Switches to the next state (either executes one command or traverses the history in the 
   * forward direction).
   */
  stepForward(): Promise<void>;

  /** Runs through subsequent states until the next breakpoint or halting the machine. */
  runForward(): Promise<void>;

  /** Whether the machine can execute the stepBack and runBack commands. */
  canMoveBack: boolean;

  /** Switches to the previous state in the history. */
  stepBack(): void;

  /** Runs through previous states until the previous breakpoint or the start of the program. */
  runBack(): void;

  /** Total number of memory cells and program lines that do not occupy a memory cell. */
  symbolsLength: number;

  /** Indices in the common list of memory cells and program lines that do not occupy a memory cell. */
  symbolIndices: number[];

  /**
   * Index of the memory cell pointed by the program counter in the common list of memory cells and 
   * program lines that do not occupy a memory cell.
   */
  programCounterIndex: number;

  /** 
   * Sets/unsets a breakpoint (by index in the common list of memory cells and program lines that 
   * do not occupy a memory cell).
   */
  toggleBreakpointAt(index: number): void;

  /**
   * If position index in the common list of memory cells and program lines that do not occupy a 
   * memory cell corresponds to a memory cell, returns the zero-based memory address of this cell.
   * If it is a program line that does not occupy a memory cell, returns undefined.
   */
  addressAt(index: number): number | undefined;

  /** Returns whether there is a breakpoint at the given index (in the common list of memory cells
   * and program lines that do not occupy a memory cell). */
  breakpointAt(index: number): boolean;

  /**
   * If position index in the common list of memory cells and program lines that do not occupy a 
   * memory cell corresponds to a memory cell, returns the contents of this cell. If it is a 
   * program line that does not occupy a memory cell, returns undefined.
   */
  cellContent(index: number): IOWord | undefined;

  /**
   * If position index in the common list of memory cells and program lines that do not occupy a
   * memory cell corresponds to a program line (whether occupying a memory cell or not), returns
   * the zero-based line number in the program. Otherwise, returns undefined.
   */
  lineNumberAt(index: number): number | undefined;

  /**
   * If position index in the common list of memory cells and program lines that do not occupy a
   * memory cell corresponds to a program line (whether occupying a memory cell or not), returns
   * this line. Otherwise, returns undefined.
   */
  lineAt(index: number): string | undefined;

  /**
   * If position index in the common list of memory cells and program lines that do not occupy a
   * memory cell corresponds to a program line (whether occupying a memory cell or not), returns
   * whether this line was modifed during the program execution. Otherwise, returns false.
   */
  lineIsModifiedAt(index: number): boolean;

  /** Returns all data from the current state of a block device. */
  blockDeviceData(deviceNum: number): Promise<IOWord[]>;

  /** Returns all data from the current state of a line device. */
  lineDeviceData(deviceNum: number): Promise<string[]>;
}

declare namespace VirtualMachineFrontEnd$ {
  /** Assembles a program and creates a binary virtual machine with history tracking. */
  function createBinaryTracking(lines: string[], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd;

  /** Assembles a program and creates a binary virtual machine without history tracking. */
  function createBinaryNonTracking(lines: string[], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd;

  /** Assembles a program and creates a decimal virtual machine with history tracking. */
  function createDecimalTracking(lines: string[], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd;

  /** Assembles a program and creates a decimal virtual machine without history tracking. */
  function createDecimalNonTracking(lines: string[], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd;

  /** Reads a program from the punched cards reader and creates a binary virtual machine with history tracking. */
  function goBinaryTracking(devicesFrontEnd: DevicesFrontEnd): Promise<VirtualMachineFrontEnd>;

  /** Reads a program from the punched cards reader and creates a binary virtual machine without history tracking. */
  function goBinaryNonTracking(devicesFrontEnd: DevicesFrontEnd): Promise<VirtualMachineFrontEnd>;

  /** Reads a program from the punched cards reader and creates a decimal virtual machine with history tracking. */
  function goDecimalTracking(devicesFrontEnd: DevicesFrontEnd): Promise<VirtualMachineFrontEnd>;

  /** Reads a program from the punched cards reader and creates a decimal virtual machine without history tracking. */
  function goDecimalNonTracking(devicesFrontEnd: DevicesFrontEnd): Promise<VirtualMachineFrontEnd>;
}

/**
 * State of the MIX virtual machine at a given moment.
 */
export class VirtualMachineState {
  /** Contents of a memory cell. */
  get(address: number): IOWord;

  /** Contents of register A. */
  getA: IOWord;

  /** Contents of register X. */
  getX: IOWord;

  /** Contents of register I1. */
  getI1: IOWord;

  /** Contents of register I2. */
  getI2: IOWord;

  /** Contents of register I3. */
  getI3: IOWord;

  /** Contents of register I4. */
  getI4: IOWord;

  /** Contents of register I5. */
  getI5: IOWord;

  /** Contents of register I6. */
  getI6: IOWord;

  /** Contents of register J. */
  getJ: IOWord;

  /** Overflow flag. */
  getOV: boolean;

  /** Comparison flag. */
  getCMP: Comparison;

  /** Time elapsed since program start, in MIX time units. */
  getTimeCounter: number;

  /** Whether the machine has encountered the HLT command and stopped. */
  isHalted: boolean;
}

/** External representation of a MIX word. */
export class IOWord {
  negative: boolean;

  bytes: number[];
}

export namespace IOWord$ {
  /** Returns an empty MIX word (positive sign and all bytes equal to zero). */
  function empty(): IOWord;
}

/** Result of value comparison. */
export class Comparison {
  /** signum = -1 for less, 0 for equals, +1 for greater. */
  constructor(signum: number);

  /** -1 for less, 0 for equals, +1 for greater. */
  signum: number;
}

/** Class representing the set of MIX devices. */
export class DevicesFrontEnd {
  /** Returns all data from the last state of a block device. */
  getBlockDeviceData(deviceNum: number): Promise<IOWord[]>;

  /** Saves the data as the initial state of a block device. */
  saveBlockDevice(deviceNum: number, data: number[]): Promise<void>;

  /** Returns all data from a line input device or the last state of a line output device. */
  getLineDeviceData(deviceNum: number): Promise<string[]>;

  /** Saves the data that a program would eventually read from a line input device. */
  saveLineDevice(deviceNum: number, data: string): Promise<void>;
}

declare namespace DevicesFrontEnd$ {
  /** Create DevicesFrontEnd backed by the given low-level operations. */
  function create(blockAccessFileOps: BlockAccessFileOps,
    lineAccessFileInputOps: LineAccessFileInputOps,
    lineAccessFileOutputOps: LineAccessFileOutputOps): DevicesFrontEnd;
}

/** Low-level operations for block devices. To be subclassed and implemented by the client. */
export class BlockAccessFileOps {
  /**
   * Reads data from a block device.
   * @param filename Identifier of the device.
   * @param version Identifier of the device state (the initial state at the beginning of program 
   *                execution is 0, every subsequent writes increases the number).
   * @param position Zero-based read start position.
   * @param blockSize Number of bytes to read.
   * @returns The data read.
   */
  readBlock(filename: string, version: number, position: number, blockSize: number): Promise<number[]>;

  /**
   * Writes data to a block device.
   * @param filename Identifier of the device.
   * @param version Identifier of the device state (the initial state at the beginning of program 
   *                execution is 0, every subsequent writes increases the number).
   * @param position Zero-based write start position.
   * @param bytes Data to write.
   */
  writeBlock(filename: string, version: number, position: number, bytes: number[]): Promise<void>;

  /**
   * Sets the last state of the device as the initial state and removes all other states.
   * @param filename Identifier of the device.
   */
  initialiseWithCurrentVersion(filename: string): Promise<void>;

  /** 
   * Returns identifiers of all states of a device.
   * @param filename Identifier of the device.
   */
  getVersions(filename: string): Promise<string[]>;

  /**
   * Saves the data as the initial state of a device.
   * @param filename Identifier of the device.
   * @param data Data to write.
   */
  save(filename: string, data: number[]): Promise<void>;

  /**
   * Retrieves a state of a device.
   * @param filename Identifier of the device.
   * @param version Identifier of the device state (the initial state at the beginning of program 
   *                execution is 0, every subsequent writes increases the number).
   * @returns Data stored on a device.
   */
  getData(filename: string, version: number): Promise<number[]>;
}

/** Low-level operations for line input devices. To be subclassed and implemented by the client. */
export class LineAccessFileInputOps {
  /**
   * Reads a single line from a device.
   * @param filename Identifier of the device.
   * @param position Zero-based line-number.
   * @returns The line read.
   */
  readLine(filename: string, position: number): Promise<string>;

  /**
   * Saves empty line as a device data.
   * @param filename Identifier of the device.
   */
  initialise(filename: string): Promise<void>;

  /**
   * Saves device data.
   * @param filename Identifier of the device.
   * @param contents Data to save.
   */
  save(filename: string, contents: string): Promise<void>;

  /**
   * Retrieves device data.
   * @param filename Identifier of the device.
   * @returns Data stored on a device.
   */
  getData(filename: string): Promise<string>;
}

/** Low-level operations for line output devices. To be subclassed and implemented by the client. */
export class LineAccessFileOutputOps {
  /**
   * Outputs a single line.
   * @param filename Identifier of the device.
   * @param version Identifier of the device state (the initial state at the beginning of program 
   *                execution is 0, every subsequent writes increases the number).
   * @param chars Characters to write (not including the new line character).
   */
  appendLine(filename: string, version: number, chars: string): Promise<void>;

  /**
   * Outputs a new page character.
   * @param filename Identifier of the device.
   * @param version Identifier of the device state (the initial state at the beginning of program 
   *                execution is 0, every subsequent writes increases the number).
   */
  appendNewPage(filename: string, version: number): Promise<void>;

  /**
   * Saves empty line as the initial state of a device and removes all other states.
   * @param filename Identifier of the device.
   */
  initialise(filename: string): Promise<void>;

  /**
   * Returns identifiers of all states of a device.
   * @param filename Identifier of the device.
   */
  getVersions(filename: string): Promise<string[]>;

  /**
   * Retrieves a state of a device.
   * @param filename Identifier of the device.
   * @param version Identifier of the device state (the initial state at the beginning of program 
   *                execution is 0, every subsequent writes increases the number).
   * @returns Data stored on a device.
   */
  getData(filename: string, version: number): Promise<string>;
}

/** An attempt to go back from the initial state of the virtual machine. */
export class BackFromInitialStateException {
  /** Name of the class. */
  tag: string;
}

/** Device does not exist. */
export class DeviceNotConnectedException {
  /** Name of the class. */
  tag: string;
  /** Device number. */
  deviceNum: number;

  /**
   * @param deviceNum Device number.
   */
  constructor(deviceNum: number);
}

/** An attempt to divide a number by zero. */
export class DivisionByZeroException {
  /** Name of the class. */
  tag: string;
}

/** A non-local symbol is used more than once in a program. */
export class DuplicateSymbolException {
  /** Name of the class. */
  tag: string;
  symbol: string;

  constructor(symbol: string);
}

/** An attempt to read beyond the end of a file. */
export class EndOfFileException {
  /** Name of the class. */
  tag: string;
}

/** Field spec is specified for a command where the field spec is used only to distinguish commands. */
export class FixedFieldSpecException {
  /** Name of the class. */
  tag: string;
  /** Name of the command. */
  operator: string;
  /** Line number. */
  line: number;

  /**
   * @param operator Name of the command.
   * @param line Line number.
   */
  constructor(operator: string, line: number);
}

/** An attempt to go forward after the virtual machine is halted. */
export class ForwardFromTerminalStateException {
  /** Name of the class. */
  tag: string;
}

/** 
 * An attempt to read from a memory cell whose contents is unpredictable due to absence of I/O 
 * operations synchronization. 
 */
export class InconsistentReadException {
  /** Name of the class. */
  tag: string;
}

/** String is not a valid MIXAL expression. */
export class InvalidExpressionException {
  /** Name of the class. */
  tag: string;
  expression: string;

  constructor(expression: string);
}

/** Value too big for a register. */
export class OverflowException {
  /** Name of the class. */
  tag: string;
}

/** Used symbol is not defined in the program. */
export class UndefinedSymbolException {
  /** Name of the class. */
  tag: string;
  symbol: string;

  constructor(symbol: string);
}

/** Cannot determine the branching condition depending on the speed of I/O operations. */
export class UnpredictableExecutionFlowException {
  /** Name of the class. */
  tag: string;
}

/** Character is absent from MIX alphabet. */
export class UnsupportedCharacterException {
  /** Name of the class. */
  tag: string;
  char: string;
}

/** Device does not support the requested operation. */
export class UnsupportedIoOperationException {
  /** Name of the class. */
  tag: string;
  operation: string;
  /** Device number. */
  deviceNum: number;
}

/** Character cannot be printed on punched cards. */
export class UnsupportedPunchedCardCharacterException {
  /** Name of the class. */
  tag: string;
  char: string;
}

/** Concurrent writes to the same memory address. */
export class WriteConflictException {
  /** Name of the class. */
  tag: string;
}

/** String is not a valid address part of a MIXAL statement. */
export class WrongAddressPartException {
  /** Name of the class. */
  tag: string;
  addressPart: string;
  line: number;

  constructor(addressPart: string, line: number);
}

/** No character corresponds to the given numeric value. */
export class WrongCharacterCodeException {
  /** Name of the class. */
  tag: string;
  code: number;

  constructor(code: number);
}

/** Number is not a valid MIXAL field specification. */
export class WrongFieldSpecException {
  /** Name of the class. */
  tag: string;
  fieldSpec: number;

  constructor(fieldSpec: number);
}

/** There is no index register with the given number. */
export class WrongIndexSpecException {
  /** Name of the class. */
  tag: string;
  /** Index register number. */
  indexSpec: number;

  /**
   * @param indexSpec Index register number.
   */
  constructor(indexSpec: number);
}

/** String is not a valid label for a MIXAL statement. */
export class WrongLabelException {
  /** Name of the class. */
  tag: string;
  label: string;

  constructor(label: string);
}

/** Text line is not a valid MIXAL line. */
export class WrongLineException {
  /** Name of the class. */
  tag: string;
  line: number;

  constructor(line: number);
}

/** Number is not a valid MIX memory address or a valid address for the executed command. */
export class WrongMemoryAddressException {
  /** Name of the class. */
  tag: string;
  address: number;
}

/** String is not a valid MIXAL operator. */
export class WrongOperatorException {
  /** Name of the class. */
  tag: string;
  operator: string;
  line: number;

  constructor(operator: string, line: number);
}
