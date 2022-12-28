import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ErrorPopupComponent } from './error-popup.component';

describe('ErrorPopupComponent', () => {
  let component: ErrorPopupComponent;
  let fixture: ComponentFixture<ErrorPopupComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ErrorPopupComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ErrorPopupComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
