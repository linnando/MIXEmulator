import { AfterViewInit, Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { IOWord, IOWord$ } from '@mixemulator/lib';
import { Subscription } from 'rxjs';
import { VirtualMachineService } from '../virtual-machine.service';

@Component({
  selector: 'mix-memory-text',
  templateUrl: './memory-text.component.html',
  styleUrls: ['./memory-text.component.scss']
})
export class MemoryTextComponent implements OnInit, AfterViewInit, OnDestroy {
  @ViewChild("scrolling") scrolling!: ElementRef;
  stateChangeSubscription?: Subscription;

  constructor(private virtualMachineService: VirtualMachineService) { }

  ngOnInit(): void {
    this.stateChangeSubscription = this.virtualMachineService.stateChange.subscribe(_ => {
      this.positionToProgramCounter();
    });
  }

  ngAfterViewInit(): void {
    this.positionToProgramCounter();
  }

  private positionToProgramCounter(): void {
    if (this.virtualMachineService.programCounterIndex != null) {
      const scrollHeight = this.scrolling.nativeElement.scrollHeight;
      const clientHeight = this.scrolling.nativeElement.clientHeight;
      this.scrolling.nativeElement.scrollTop = this.virtualMachineService.programCounterIndex * scrollHeight / this.virtualMachineService.symbolsLength - clientHeight / 2;
    }
  }

  ngOnDestroy(): void {
    this.stateChangeSubscription?.unsubscribe();
  }

  get indices(): number[] {
    return this.virtualMachineService.symbolIndices;
  }

  toggleBreakpoint(index: number): void {
    this.virtualMachineService.toggleBreakpointAt(index);
  }

  hasBreakpointAt(index: number): boolean {
    return this.virtualMachineService.breakpointAt(index) ?? false;
  }

  isCurrent(index: number): boolean {
    return this.virtualMachineService.programCounterIndex === index;
  }

  hasWordAt(index: number): boolean {
    return this.virtualMachineService.addressAt(index) != null;
  }

  cellAddressAt(index: number): number {
    return this.virtualMachineService.addressAt(index) ?? 0;
  }

  cellSignAt(index: number): string {
    return this.cellContentAt(index).negative ? '-' : '+';
  }

  private cellContentAt(index: number): IOWord {
    return this.virtualMachineService.cellContent(index) ?? IOWord$.empty();
  }

  cellByteAt(index: number, pos: number): number {
    return this.cellContentAt(index).bytes[pos];
  }

  hasLineAt(index: number): boolean {
    return this.virtualMachineService.lineNumberAt(index) != null;
  }

  lineNumberAt(index: number): number {
    return this.virtualMachineService.lineNumberAt(index) ?? 0;
  }

  lineAt(index: number): string {
    return this.virtualMachineService.lineAt(index) ?? '';
  }

  lineIsModifiedAt(index: number): boolean {
    return this.virtualMachineService.lineIsModifiedAt(index) ?? false;
  }
}
