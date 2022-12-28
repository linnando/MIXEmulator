import { ComponentFixture, TestBed } from '@angular/core/testing';

import { RegistersComponent } from './registers.component';

describe('RegistersComponent', () => {
  let component: RegistersComponent;
  let fixture: ComponentFixture<RegistersComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ RegistersComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(RegistersComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
