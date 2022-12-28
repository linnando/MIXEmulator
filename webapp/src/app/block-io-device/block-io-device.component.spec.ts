import { ComponentFixture, TestBed } from '@angular/core/testing';

import { BlockIoDeviceComponent } from './block-io-device.component';

describe('BlockIoDeviceComponent', () => {
  let component: BlockIoDeviceComponent;
  let fixture: ComponentFixture<BlockIoDeviceComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ BlockIoDeviceComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(BlockIoDeviceComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
