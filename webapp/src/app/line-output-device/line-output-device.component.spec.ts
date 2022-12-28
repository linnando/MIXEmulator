import { ComponentFixture, TestBed } from '@angular/core/testing';

import { LineOutputDeviceComponent } from './line-output-device.component';

describe('LineOutputDeviceComponent', () => {
  let component: LineOutputDeviceComponent;
  let fixture: ComponentFixture<LineOutputDeviceComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ LineOutputDeviceComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(LineOutputDeviceComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
