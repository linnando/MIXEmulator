import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { faDownload } from '@fortawesome/free-solid-svg-icons';
import { BsModalService } from 'ngx-bootstrap/modal';
import { Subscription } from 'rxjs';
import { ErrorPopupComponent } from '../error-popup/error-popup.component';
import { VirtualMachineService } from '../virtual-machine.service';

@Component({
  selector: 'mix-line-output-device',
  templateUrl: './line-output-device.component.html',
  styleUrls: ['./line-output-device.component.scss']
})
export class LineOutputDeviceComponent implements OnInit, OnDestroy {
  @Input() deviceNum = 0;
  stateChangeSubscription?: Subscription;
  lines: string[] = [];
  readonly faDownload = faDownload;

  constructor(private modalService: BsModalService, private virtualMachineService: VirtualMachineService) { }

  ngOnInit(): void {
    this.fetchDeviceData();
    this.stateChangeSubscription = this.virtualMachineService.stateChange.subscribe(_ => {
      this.fetchDeviceData();
    });
  }

  private async fetchDeviceData(): Promise<void> {
    try {
      this.lines = await this.virtualMachineService.lineDeviceData(this.deviceNum);
    } catch (e) {
      this.modalService.show(ErrorPopupComponent, { initialState: { exception: e } });
    }
  }

  ngOnDestroy(): void {
    this.stateChangeSubscription?.unsubscribe();
  }

  downloadFile(): void {
    const blobParts = this.lines.map(line => line + '\n');
    const blob = new Blob(blobParts, { 'type': 'text/plain' });
    const url = URL.createObjectURL(blob);
    window.open(url);
  }
}
