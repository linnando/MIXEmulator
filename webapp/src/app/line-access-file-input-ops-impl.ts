import { EndOfFileException, LineAccessFileInputOps } from '@mixemulator/lib';

export class LineAccessFileInputOpsImpl extends LineAccessFileInputOps {
  constructor(private storage: Storage) {
    super();
  }

  override readLine(filename: string, position: number): Promise<string> {
    return new Promise((resolve, reject) => {
      try {
        const data = this.storage.getItem(filename);
        if (data == null) {
          return reject(new Error(`File ${filename} not found.`));
        }
        const lines = data.split('\n');
        if (position < lines.length) {
          return resolve(lines[position]);
        } else {
          return reject(new EndOfFileException());
        }
      } catch (e) {
        return reject(e);
      }
    });
  }

  override initialise(filename: string): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        if (this.storage.getItem(filename) == null) {
          this.storage.setItem(filename, '');
        }
        return resolve();
      } catch (e) {
        return reject(e);
      }
    });
  }

  override save(filename: string, data: string): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        this.storage.setItem(filename, data);
        return resolve();
      } catch (e) {
        return reject(e);
      }
    });
  }

  override getData(filename: string): Promise<string> {
    return new Promise((resolve, reject) => {
      try {
        const data = this.storage.getItem(filename);
        if (data == null) {
          return reject(new Error(`File ${filename} not found.`));
        }
        return resolve(data);
      } catch (e) {
        return reject(e);
      }
    });
  }
}
