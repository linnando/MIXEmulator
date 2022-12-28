import { Component } from '@angular/core';
import { IOWord, IOWord$, MEMORY_SIZE, VirtualMachineState } from '@mixemulator/lib';
import { VirtualMachineService } from '../virtual-machine.service';

@Component({
  selector: 'mix-memory-data',
  templateUrl: './memory-data.component.html',
  styleUrls: ['./memory-data.component.scss']
})
export class MemoryDataComponent {
  readonly addresses: number[] = [...Array(MEMORY_SIZE).keys()];

  constructor(private virtualMachineService: VirtualMachineService) {
  }

  get machineState(): VirtualMachineState | undefined {
    return this.virtualMachineService.machineState;
  }

  cellSign(address: number): string {
    return this.cellContent(address).negative ? '-' : '+';
  }

  cellByte(address: number, pos: number): number {
    return this.cellContent(address).bytes[pos];
  }

  cellContent(address: number): IOWord {
    return this.machineState?.get(address) ?? IOWord$.empty();
  }
};
