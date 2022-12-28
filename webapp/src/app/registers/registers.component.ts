import { Component } from '@angular/core';
import { IOWord, IOWord$ } from '@mixemulator/lib';
import { VirtualMachineService } from '../virtual-machine.service';

@Component({
  selector: 'mix-registers',
  templateUrl: './registers.component.html',
  styleUrls: ['./registers.component.scss']
})
export class RegistersComponent {
  constructor(private virtualMachineService: VirtualMachineService) { }

  registerSign(register: string): string {
    return this.registerContent(register).negative ? '-' : '+';
  }

  registerByte(register: string, pos: number): number {
    return this.registerContent(register).bytes[pos];
  }

  registerContent(register: string): IOWord {
    switch (register) {
      case 'A':
        return this.machineState?.getA ?? IOWord$.empty();
      case 'X':
        return this.machineState?.getX ?? IOWord$.empty();
      case 'I1':
        return this.machineState?.getI1 ?? IOWord$.empty();
      case 'I2':
        return this.machineState?.getI2 ?? IOWord$.empty();
      case 'I3':
        return this.machineState?.getI3 ?? IOWord$.empty();
      case 'I4':
        return this.machineState?.getI4 ?? IOWord$.empty();
      case 'I5':
        return this.machineState?.getI5 ?? IOWord$.empty();
      case 'I6':
        return this.machineState?.getI6 ?? IOWord$.empty();
      case 'J':
        return this.machineState?.getJ ?? IOWord$.empty();
      default:
        return IOWord$.empty();
    }
  }

  private get machineState() {
    return this.virtualMachineService.machineState;
  }

  get overflow(): boolean {
    return this.machineState?.getOV ?? false;
  }

  get comparison(): number {
    return this.machineState?.getCMP?.signum ?? 0;
  }

  get timeElapsed(): number {
    return this.machineState?.getTimeCounter ?? 0;
  }

  get isHalted(): boolean {
    return this.machineState?.isHalted ?? false;
  }
}
