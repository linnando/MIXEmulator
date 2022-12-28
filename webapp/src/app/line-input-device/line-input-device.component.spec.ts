import { ComponentFixture, TestBed } from '@angular/core/testing';

import { LineInputDeviceComponent } from './line-input-device.component';

describe('LineInputDeviceComponent', () => {
  let component: LineInputDeviceComponent;
  let fixture: ComponentFixture<LineInputDeviceComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ LineInputDeviceComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(LineInputDeviceComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
