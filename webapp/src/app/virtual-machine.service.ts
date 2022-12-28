import { Injectable } from '@angular/core';
import { DevicesFrontEnd, DevicesFrontEnd$, IOWord, VirtualMachineFrontEnd, VirtualMachineFrontEnd$, VirtualMachineState } from '@mixemulator/lib';
import { Subject } from 'rxjs';
import { BlockAccessFileOpsImpl } from './block-access-file-ops-impl';
import { LineAccessFileInputOpsImpl } from './line-access-file-input-ops-impl';
import { LineAccessFileOutputOpsImpl } from './line-access-file-output-ops-impl';

@Injectable({
  providedIn: 'root'
})
export class VirtualMachineService {
  private static readonly deviceNames: Map<number, string> = new Map([
    [0, 'Tape Unit 0'],
    [1, 'Tape Unit 1'],
    [2, 'Tape Unit 2'],
    [3, 'Tape Unit 3'],
    [4, 'Tape Unit 4'],
    [5, 'Tape Unit 5'],
    [6, 'Tape Unit 6'],
    [7, 'Tape Unit 7'],
    [8, 'Disk Unit 0'],
    [9, 'Disk Unit 1'],
    [10, 'Disk Unit 2'],
    [11, 'Disk Unit 3'],
    [12, 'Disk Unit 4'],
    [13, 'Disk Unit 5'],
    [14, 'Disk Unit 6'],
    [15, 'Disk Unit 7'],
    [16, 'Card Reader'],
    [17, 'Card Punch'],
    [18, 'Line Printer'],
    [20, 'Paper Tape'],
  ]);

  text = `START      OUT  HELLO(18)
           JBUS *(18)
           HLT
HELLO      ALF  HELLO
           ALF  , WOR
           ALF  LD
           END START`;
  mode = 'binary';
  tracking = true;

  readonly stateChange = new Subject<void>();

  private maybeMachine?: VirtualMachineFrontEnd;
  private readonly devicesFrontEnd: DevicesFrontEnd = DevicesFrontEnd$.create(
    new BlockAccessFileOpsImpl(localStorage),
    new LineAccessFileInputOpsImpl(localStorage),
    new LineAccessFileOutputOpsImpl(localStorage));

  assemble(): void {
    const lines = this.text.split('\n');
    switch (this.mode) {
      case 'binary':
        if (this.tracking) {
          this.maybeMachine = VirtualMachineFrontEnd$.createBinaryTracking(lines, this.devicesFrontEnd);
        } else {
          this.maybeMachine = VirtualMachineFrontEnd$.createBinaryNonTracking(lines, this.devicesFrontEnd);
        }
        break;
      case 'decimal':
        if (this.tracking) {
          this.maybeMachine = VirtualMachineFrontEnd$.createDecimalTracking(lines, this.devicesFrontEnd);
        } else {
          this.maybeMachine = VirtualMachineFrontEnd$.createDecimalNonTracking(lines, this.devicesFrontEnd);
        }
        break;
    }
  }

  async go(): Promise<void> {
    switch (this.mode) {
      case 'binary':
        if (this.tracking) {
          this.maybeMachine = await VirtualMachineFrontEnd$.goBinaryTracking(this.devicesFrontEnd);
        } else {
          this.maybeMachine = await VirtualMachineFrontEnd$.goBinaryNonTracking(this.devicesFrontEnd);
        }
        break;
      case 'decimal':
        if (this.tracking) {
          this.maybeMachine = await VirtualMachineFrontEnd$.goDecimalTracking(this.devicesFrontEnd);
        }
        else {
          this.maybeMachine = await VirtualMachineFrontEnd$.goDecimalNonTracking(this.devicesFrontEnd);
        }
        break;
    }
  }

  switchOffMachine(): void {
    this.maybeMachine = undefined;
  }

  get isActive(): boolean {
    return this.maybeMachine != null;
  }

  get canMoveForward(): boolean {
    return this.maybeMachine?.canMoveForward ?? false;
  }

  async runForward(): Promise<void> {
    await this.maybeMachine?.runForward();
    this.stateChange.next();
  }

  async stepForward(): Promise<void> {
    await this.maybeMachine?.stepForward();
    this.stateChange.next();
  }

  get canMoveBack(): boolean {
    return this.maybeMachine?.canMoveBack ?? false;
  }

  runBack(): void {
    if (this.maybeMachine != null) {
      this.maybeMachine.runBack();
      this.stateChange.next();
    }
  }

  stepBack(): void {
    if (this.maybeMachine != null) {
      this.maybeMachine.stepBack();
      this.stateChange.next();
    }
  }

  get machineState(): VirtualMachineState | undefined {
    return this.maybeMachine?.currentState;
  }

  get symbolsLength(): number {
    return this.maybeMachine?.symbolsLength ?? 0;
  }

  get symbolIndices(): number[] {
    return this.maybeMachine?.symbolIndices ?? [];
  }

  get programCounterIndex(): number | undefined {
    return this.maybeMachine?.programCounterIndex;
  }

  toggleBreakpointAt(index: number): void {
    this.maybeMachine?.toggleBreakpointAt(index);
  }

  addressAt(index: number): number | undefined {
    return this.maybeMachine?.addressAt(index);
  }

  breakpointAt(index: number): boolean | undefined {
    return this.maybeMachine?.breakpointAt(index);
  }

  cellContent(index: number): IOWord | undefined {
    return this.maybeMachine?.cellContent(index);
  }

  lineNumberAt(index: number): number | undefined {
    return this.maybeMachine?.lineNumberAt(index);
  }

  lineAt(index: number): string | undefined {
    return this.maybeMachine?.lineAt(index);
  }

  lineIsModifiedAt(index: number): boolean | undefined {
    return this.maybeMachine?.lineIsModifiedAt(index);
  }

  get deviceNumbers(): IterableIterator<number> {
    return VirtualMachineService.deviceNames.keys();
  }

  deviceName(deviceNum: number): string {
    return VirtualMachineService.deviceNames.get(deviceNum) ?? '';
  }

  blockDeviceData(deviceNum: number): Promise<IOWord[]> {
    return this.maybeMachine?.blockDeviceData(deviceNum) ?? this.devicesFrontEnd.getBlockDeviceData(deviceNum);
  }

  saveBlockDevice(deviceNum: number, data: number[]): Promise<void> {
    return this.devicesFrontEnd.saveBlockDevice(deviceNum, data);
  }

  lineDeviceData(deviceNum: number): Promise<string[]> {
    return this.maybeMachine?.lineDeviceData(deviceNum) ?? this.devicesFrontEnd.getLineDeviceData(deviceNum);
  }

  saveLineDevice(deviceNum: number, data: string): Promise<void> {
    return this.devicesFrontEnd.saveLineDevice(deviceNum, data);
  }
}
