import { Component, Input, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { BsModalService } from 'ngx-bootstrap/modal';
import { ErrorPopupComponent } from '../error-popup/error-popup.component';
import { VirtualMachineService } from '../virtual-machine.service';

@Component({
  selector: 'mix-line-input-device',
  templateUrl: './line-input-device.component.html',
  styleUrls: ['./line-input-device.component.scss']
})
export class LineInputDeviceComponent implements OnInit {
  @Input() deviceNum = 0;
  @Input() hasGoButton = false;
  lines: string[] = [];

  constructor(private modalService: BsModalService,
    private router: Router,
    private virtualMachineService: VirtualMachineService) { }

  ngOnInit(): void {
    this.fetchDeviceData();
  }

  private async fetchDeviceData(): Promise<void> {
    try {
      this.lines = await this.virtualMachineService.lineDeviceData(this.deviceNum);
    } catch (e) {
      this.modalService.show(ErrorPopupComponent, { initialState: { exception: e } });
    }
  }

  get canGo(): boolean {
    return !this.virtualMachineService.isActive;
  }

  get canUploadFile(): boolean {
    return !this.virtualMachineService.isActive;
  }

  startFileUpload(): void {
    document.getElementById("inputFile")?.click();
  }

  uploadFile(target: EventTarget | null): void {
    if (!(target instanceof HTMLInputElement)) {
      return;
    }
    const files = target.files;
    if (!files || files.length == 0) {
      return;
    }
    const file = files[0];
    const reader = new FileReader();
    reader.onload = async (event: ProgressEvent<FileReader>) => {
      const fileData = event.target?.result;
      if (!(typeof fileData === 'string')) {
        return;
      }
      try {
        await this.virtualMachineService.saveLineDevice(this.deviceNum, fileData);
        this.fetchDeviceData();
      } catch (e) {
        this.modalService.show(ErrorPopupComponent, { initialState: { exception: e } });
      }
    };
    reader.readAsText(file);
  }

  downloadFile(): void {
    const blobParts = this.lines.map(line => line + "\n");
    const blob = new Blob(blobParts, { 'type': 'text/plain' });
    const url = URL.createObjectURL(blob);
    window.open(url);
  }

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

  async go(): Promise<void> {
    try {
      await this.virtualMachineService.go();
      this.router.navigate(['/vm']);
    } catch (e) {
      this.modalService.show(ErrorPopupComponent, { initialState: { exception: e } });
    }
  }
}
