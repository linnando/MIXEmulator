import { Component } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  selector: 'mix-app',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent {
  navbarIsCollapsed = true;

  constructor(public router: Router) { }
}
