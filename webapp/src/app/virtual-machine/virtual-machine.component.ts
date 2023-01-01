import { Component } from '@angular/core';
import { faFastBackward, faFastForward, faHistory, faLongArrowAltRight, faPowerOff, faPuzzlePiece, faStepBackward, faStepForward } from '@fortawesome/free-solid-svg-icons';
import { BsModalService } from 'ngx-bootstrap/modal';
import { ErrorPopupComponent } from '../error-popup/error-popup.component';
import { VirtualMachineService } from '../virtual-machine.service';

@Component({
  selector: 'mix-virtual-machine',
  templateUrl: './virtual-machine.component.html',
  styleUrls: ['./virtual-machine.component.scss']
})
export class VirtualMachineComponent {
  readonly faHistory = faHistory;
  readonly faLongArrowAltRight = faLongArrowAltRight;
  readonly faPuzzlePiece = faPuzzlePiece;
  readonly faPowerOff = faPowerOff;
  readonly faFastBackward = faFastBackward;
  readonly faStepBackward = faStepBackward;
  readonly faStepForward = faStepForward;
  readonly faFastForward = faFastForward;

  constructor(private modalService: BsModalService, private virtualMachineService: VirtualMachineService) { }

  get mode(): string {
    return this.virtualMachineService.mode;
  }

  set mode(value: string) {
    this.virtualMachineService.mode = value;
  }

  get tracking(): boolean {
    return this.virtualMachineService.tracking;
  }

  set tracking(value: boolean) {
    this.virtualMachineService.tracking = value;
  }

  assemble(): void {
    try {
      this.virtualMachineService.assemble();
    } catch (e) {
      this.modalService.show(ErrorPopupComponent, { initialState: { exception: e } });
    }
  }

  switchOff(): void {
    this.virtualMachineService.switchOffMachine();
  }

  get canMoveForward(): boolean {
    return this.virtualMachineService.canMoveForward;
  }

  async runForward(): Promise<void> {
    try {
      await this.virtualMachineService.runForward();
    } catch (e) {
      this.modalService.show(ErrorPopupComponent, { initialState: { exception: e } });
    }
  }

  async stepForward(): Promise<void> {
    try {
      await this.virtualMachineService.stepForward();
    } catch (e) {
      this.modalService.show(ErrorPopupComponent, { initialState: { exception: e } });
    }
  }

  get canMoveBack(): boolean {
    return this.virtualMachineService.canMoveBack;
  }

  runBack(): void {
    try {
      this.virtualMachineService.runBack();
    } catch (e) {
      this.modalService.show(ErrorPopupComponent, { initialState: { exception: e } });
    }
  }

  stepBack(): void {
    try {
      this.virtualMachineService.stepBack();
    } catch (e) {
      this.modalService.show(ErrorPopupComponent, { initialState: { exception: e } });
    }
  }

  get isActive(): boolean {
    return this.virtualMachineService.isActive;
  }
}
