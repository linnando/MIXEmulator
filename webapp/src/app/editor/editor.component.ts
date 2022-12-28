import { AfterViewInit, Component } from '@angular/core';
import { Router } from '@angular/router';
import * as ace from 'ace-builds';
import 'ace-builds/webpack-resolver';
import { BsModalService } from 'ngx-bootstrap/modal';
import { ErrorPopupComponent } from '../error-popup/error-popup.component';
import { VirtualMachineService } from '../virtual-machine.service';

@Component({
  selector: 'mix-editor',
  templateUrl: './editor.component.html',
  styleUrls: ['./editor.component.scss']
})
export class EditorComponent implements AfterViewInit {
  editor?: ace.Ace.Editor;

  constructor(private modalService: BsModalService,
    private router: Router,
    private virtualMachineService: VirtualMachineService) { }

  ngAfterViewInit(): void {
    this.editor = ace.edit('editor');
    this.editor.setTheme('ace/theme/chrome');
    this.editor.setValue(this.virtualMachineService.text);
    this.editor.getSession().setMode('ace/mode/mixal');
    this.editor.getSession().setUseSoftTabs(true);
    this.editor.getSession().on('change', (): void => {
      this.virtualMachineService.text = this.editor?.getValue() ?? '';
    });
  }

  startFileUpload(): void {
    document.getElementById('inputFile')?.click();
  }

  uploadFile(target: EventTarget | null): void {
    if (!(target instanceof HTMLInputElement)) {
      return;
    }
    const files = target.files;
    if (!files || files.length == 0) {
      return;
    }
    const file = files[0];
    const reader = new FileReader();
    reader.onload = (event: ProgressEvent<FileReader>) => {
      const fileData = event.target?.result;
      if (!(typeof fileData === 'string')) {
        return;
      }
      const programText = fileData.replace(/\t/g, '    ');
      this.editor?.setValue(programText);
    };
    reader.readAsText(file);
  }

  downloadFile(): void {
    const blob = new Blob([this.virtualMachineService.text], { 'type': 'text/plain' });
    const url = URL.createObjectURL(blob);
    window.open(url);
  }

  get mode(): string {
    return this.virtualMachineService.mode;
  }

  set mode(value: string) {
    this.virtualMachineService.mode = value;
  }

  get tracking(): boolean {
    return this.virtualMachineService.tracking;
  }

  set tracking(value: boolean) {
    this.virtualMachineService.tracking = value;
  }

  assemble(): void {
    try {
      this.virtualMachineService.assemble();
      this.router.navigate(['/vm']);
    } catch (e) {
      this.modalService.show(ErrorPopupComponent, { initialState: { exception: e } });
    }
  }
}
