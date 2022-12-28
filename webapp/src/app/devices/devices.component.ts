import { Component, OnInit } from '@angular/core';
import { VirtualMachineService } from '../virtual-machine.service';

@Component({
  selector: 'mix-devices',
  templateUrl: './devices.component.html',
  styleUrls: ['./devices.component.scss']
})
export class DevicesComponent implements OnInit {
  deviceNumbers: number[] = [];
  selectedDevice = 0;

  constructor(private virtualMachineService: VirtualMachineService) { }

  ngOnInit(): void {
    this.deviceNumbers = [...this.virtualMachineService.deviceNumbers];
  }

  deviceDescription(deviceNumber: number): string {
    const deviceName = this.virtualMachineService.deviceName(deviceNumber);
    return `${deviceNumber} - ${deviceName}`;
  };
}
