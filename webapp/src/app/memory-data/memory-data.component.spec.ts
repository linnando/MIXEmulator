import { ComponentFixture, TestBed } from '@angular/core/testing';

import { MemoryDataComponent } from './memory-data.component';

describe('MemoryDataComponent', () => {
  let component: MemoryDataComponent;
  let fixture: ComponentFixture<MemoryDataComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ MemoryDataComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(MemoryDataComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
