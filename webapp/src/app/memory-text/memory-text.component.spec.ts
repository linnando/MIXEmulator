import { ComponentFixture, TestBed } from '@angular/core/testing';

import { MemoryTextComponent } from './memory-text.component';

describe('MemoryTextComponent', () => {
  let component: MemoryTextComponent;
  let fixture: ComponentFixture<MemoryTextComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ MemoryTextComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(MemoryTextComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
