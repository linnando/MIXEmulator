(function (global) {
  System.config({
    paths: {
      'npm:': 'node_modules/'
    },
    map: {
      'app': 'webapp/target/scala-2.11',
      '@angular/core': 'npm:@angular/core/bundles/core.umd.js',
      '@angular/common': 'npm:@angular/common/bundles/common.umd.js',
      '@angular/compiler': 'npm:@angular/compiler/bundles/compiler.umd.js',
      '@angular/platform-browser': 'npm:@angular/platform-browser/bundles/platform-browser.umd.js',
      '@angular/platform-browser-dynamic': 'npm:@angular/platform-browser-dynamic/bundles/platform-browser-dynamic.umd.js',
      '@angular/http': 'npm:@angular/http/bundles/http.umd.js',
      '@angular/router': 'npm:@angular/router/bundles/router.umd.js',
      '@angular/forms': 'npm:@angular/forms/bundles/forms.umd.js',
      '@angular/upgrade': 'npm:@angular/upgrade/bundles/upgrade.umd.js',
      'buffer': 'lib/browserfs/shims/buffer.js',
      'bufferGlobal': 'lib/browserfs/shims/bufferGlobal.js',
      'fs': 'lib/browserfs/shims/fs.js',
      'path': 'lib/browserfs/shims/path.js',
      'processGlobal': 'lib/browserfs/shims/process.js',
      'rxjs': 'npm:rxjs'
    },
    packages: {
      app: {
        main: './webapp-sjsx.js',
        map: {
          'scalaModule': './webapp-opt.js',
        },
        format: 'cjs',
        defaultExtension: 'js'
      },
      rxjs: {
        defaultExtension: 'js'
      }
    }
  });
})(this);
