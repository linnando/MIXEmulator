import { TestBed } from '@angular/core/testing';

import { VirtualMachineService } from './virtual-machine.service';

describe('VirtualMachineService', () => {
  let service: VirtualMachineService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(VirtualMachineService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
