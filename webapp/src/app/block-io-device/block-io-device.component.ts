import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { IOWord } from '@mixemulator/lib';
import { BsModalService } from 'ngx-bootstrap/modal';
import { Subscription } from 'rxjs';
import { ErrorPopupComponent } from '../error-popup/error-popup.component';
import { VirtualMachineService } from '../virtual-machine.service';

@Component({
  selector: 'mix-block-io-device',
  templateUrl: './block-io-device.component.html',
  styleUrls: ['./block-io-device.component.scss']
})
export class BlockIoDeviceComponent implements OnInit, OnDestroy {
  @Input() deviceNum = 0;
  stateChangeSubscription?: Subscription;
  ioWords: IOWord[] = [];

  constructor(private modalService: BsModalService, private virtualMachineService: VirtualMachineService) { }

  ngOnInit(): void {
    this.fetchDeviceData();
    this.stateChangeSubscription = this.virtualMachineService.stateChange.subscribe(_ => {
      this.fetchDeviceData();
    });
  }

  private async fetchDeviceData(): Promise<void> {
    try {
      this.ioWords = await this.virtualMachineService.blockDeviceData(this.deviceNum);
    } catch (e) {
      this.modalService.show(ErrorPopupComponent, { initialState: { exception: e } });
    }
  }

  ngOnDestroy(): void {
    this.stateChangeSubscription?.unsubscribe();
  }

  get canUploadFile(): boolean {
    return !this.virtualMachineService.isActive;
  }

  startFileUpload(): void {
    document.getElementById('inputFile')?.click();
  }

  uploadFile(input: EventTarget | null): void {
    if (!(input instanceof HTMLInputElement)) {
      return;
    }
    const files = input.files;
    if (!files || files.length == 0) {
      return;
    }
    const file = files[0];
    const reader = new FileReader();
    reader.onload = async (event: ProgressEvent<FileReader>) => {
      const buffer = event.target?.result;
      if (!(buffer instanceof ArrayBuffer)) {
        return;
      }
      const fileData = Array.from(new Uint8Array(buffer));
      try {
        await this.virtualMachineService.saveBlockDevice(this.deviceNum, fileData);
        this.fetchDeviceData();
      } catch (e) {
        this.modalService.show(ErrorPopupComponent, { initialState: { exception: e } });
      }
    };
    reader.readAsArrayBuffer(file);
  }

  downloadFile(): void {
    const bytes = this.ioWords.flatMap(word => {
      const headByte = word.negative ? (word.bytes[0] | 0x80) : word.bytes[0];
      return [headByte, ...word.bytes.slice(1, 5)];
    });
    const buffer = new Uint8Array(bytes).buffer;
    const blob = new Blob([buffer], { 'type': 'application/octet-stream' });
    window.open(URL.createObjectURL(blob));
  }
}
