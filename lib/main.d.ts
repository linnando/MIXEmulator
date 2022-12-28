export const MEMORY_SIZE: number;

export class VirtualMachineFrontEnd {
  currentState: VirtualMachineState;

  canMoveForward: boolean;

  stepForward(): Promise<void>;

  runForward(): Promise<void>;

  canMoveBack: boolean;

  stepBack(): void;

  runBack(): void;

  symbolsLength: number;

  symbolIndices: number[];

  programCounterIndex: number;

  toggleBreakpointAt(index: number): void;

  addressAt(index: number): number | undefined;

  breakpointAt(index: number): boolean;

  cellContent(index: number): IOWord | undefined;

  lineNumberAt(index: number): number | undefined;

  lineAt(index: number): string | undefined;

  lineIsModifiedAt(index: number): boolean;

  blockDeviceData(deviceNum: number): Promise<IOWord[]>;

  lineDeviceData(deviceNum: number): Promise<string[]>;
}

declare namespace VirtualMachineFrontEnd$ {
  function createBinaryTracking(lines: string[], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd;

  function createBinaryNonTracking(lines: string[], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd;

  function createDecimalTracking(lines: string[], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd;

  function createDecimalNonTracking(lines: string[], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd;

  function goBinaryTracking(devicesFrontEnd: DevicesFrontEnd): Promise<VirtualMachineFrontEnd>;

  function goBinaryNonTracking(devicesFrontEnd: DevicesFrontEnd): Promise<VirtualMachineFrontEnd>;

  function goDecimalTracking(devicesFrontEnd: DevicesFrontEnd): Promise<VirtualMachineFrontEnd>;

  function goDecimalNonTracking(devicesFrontEnd: DevicesFrontEnd): Promise<VirtualMachineFrontEnd>;
}

export class VirtualMachineState {
  get(address: number): IOWord;

  getA: IOWord;

  getX: IOWord;

  getI1: IOWord;

  getI2: IOWord;

  getI3: IOWord;

  getI4: IOWord;

  getI5: IOWord;

  getI6: IOWord;

  getJ: IOWord;

  getOV: boolean;

  getCMP: Comparison;

  getTimeCounter: number;

  isHalted: boolean;
}

export class IOWord {
  negative: boolean;

  bytes: number[];
}

export namespace IOWord$ {
  function empty(): IOWord
}

export class Comparison {
  constructor(signum: number);

  signum: number;
}

export class DevicesFrontEnd {
  getBlockDeviceData(deviceNum: number): Promise<IOWord[]>;

  saveBlockDevice(deviceNum: number, data: number[]): Promise<void>;

  getLineDeviceData(deviceNum: number): Promise<string[]>;

  saveLineDevice(deviceNum: number, data: string): Promise<void>;
}

declare namespace DevicesFrontEnd$ {
  function create(blockAccessFileOps: BlockAccessFileOps,
    lineAccessFileInputOps: LineAccessFileInputOps,
    lineAccessFileOutputOps: LineAccessFileOutputOps): DevicesFrontEnd;
}

export class BlockAccessFileOps {
  readBlock(filename: string, version: number, position: number, blockSize: number): Promise<number[]>;

  writeBlock(filename: string, version: number, position: number, bytes: number[]): Promise<void>;

  initialiseWithCurrentVersion(filename: string): Promise<void>;

  getVersions(filename: string): Promise<string[]>;

  save(filename: string, data: number[]): Promise<void>;

  getData(filename: string, version: number): Promise<number[]>;
}

export class LineAccessFileInputOps {
  readLine(filename: string, position: number): Promise<string>;

  initialise(filename: string): Promise<void>;

  save(filename: string, contents: string): Promise<void>;

  getData(filename: string): Promise<string>;
}

export class LineAccessFileOutputOps {
  appendLine(filename: string, version: number, chars: string): Promise<void>;

  appendNewPage(filename: string, version: number): Promise<void>;

  initialise(filename: string): Promise<void>;

  getVersions(filename: string): Promise<string[]>;

  getData(filename: string, version: number): Promise<string>;
}

export class BackFromInitialStateException {
  tag: string;
}

export class DeviceNotConnectedException {
  tag: string;
  deviceNum: number;

  constructor(deviceNum: number);
}

export class DivisionByZeroException {
  tag: string;
}

export class DuplicateSymbolException {
  tag: string;
  symbol: string;

  constructor(symbol: string);
}

export class EndOfFileException {
  tag: string;
}

export class FixedFieldSpecException {
  tag: string;
  operator: string;
  line: number;

  constructor(operator: string, line: number);
}

export class ForwardFromTerminalStateException {
  tag: string;
}

export class InconsistentReadException {
  tag: string;
}

export class InvalidExpressionException {
  tag: string;
  expression: string;

  constructor(expression: string);
}

export class OverflowException {
  tag: string;
}

export class UndefinedSymbolException {
  tag: string;
  symbol: string;

  constructor(symbol: string);
}

export class UnpredictableExecutionFlowException {
  tag: string;
}

export class UnsupportedCharacterException {
  tag: string;
  char: string;
}

export class UnsupportedPunchedCardCharacterException {
  tag: string;
  char: string;
}

export class WriteConflictException {
  tag: string;
}

export class WrongAddressPartException {
  tag: string;
  addressPart: string;
  line: number;

  constructor(addressPart: string, line: number);
}

export class WrongCharacterCodeException {
  tag: string;
  code: number;

  constructor(code: number);
}

export class WrongFieldSpecException {
  tag: string;
  fieldSpec: number;

  constructor(fieldSpec: number);
}

export class WrongIndexSpecException {
  tag: string;
  indexSpec: number;

  constructor(indexSpec: number);
}

export class WrongLabelException {
  tag: string;
  label: string;

  constructor(label: string);
}

export class WrongLineException {
  tag: string;
  line: number;

  constructor(line: number);
}

export class WrongMemoryAddressException {
  tag: string;
  address: number;
}

export class WrongOperatorException {
  tag: string;
  operator: string;
  line: number;

  constructor(operator: string, line: number);
}
