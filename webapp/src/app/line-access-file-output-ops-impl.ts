import { LineAccessFileOutputOps } from '@mixemulator/lib';

export class LineAccessFileOutputOpsImpl extends LineAccessFileOutputOps {
  constructor(private storage: Storage) {
    super();
  }

  override appendLine(filename: string, version: number, chars: string): Promise<void> {
    return this.appendCharsToNewVersion(filename, version, chars + '\n');
  }

  private appendCharsToNewVersion(filename: string, version: number, chars: string): Promise<void> {
    const oldFile = `${filename}/${version}`;
    const newFile = `${filename}/${version + 1}`;
    return new Promise((resolve, reject) => {
      try {
        const srcData = this.storage.getItem(oldFile) ?? '';
        this.storage.setItem(newFile, srcData + chars);
        return resolve();
      } catch (e) {
        return reject(e);
      }
    });
  }

  override appendNewPage(filename: string, version: number): Promise<void> {
    return this.appendCharsToNewVersion(filename, version, 'f');
  }

  override async initialise(filename: string): Promise<void> {
    const versions = await this.getVersions(filename);
    return new Promise((resolve, reject) => {
      try {
        for (const version of versions) {
          this.storage.removeItem(`${filename}/${version}`);
        }
        this.storage.setItem(`${filename}/0`, '');
        return resolve();
      } catch (e) {
        return reject(e);
      }
    });
  }

  override getVersions(filename: string): Promise<string[]> {
    return new Promise((resolve, reject) => {
      try {
        const versions: string[] = [];
        for (let i = 0; i < this.storage.length; i++) {
          const key = this.storage.key(i);
          const match = key?.match(new RegExp(`^${filename}/(\\d+)$`));
          if (match) {
            versions.push(match[1]);
          }
        }
        return resolve(versions);
      } catch (e) {
        return reject(e);
      }
    });
  }

  override getData(filename: string, version: number): Promise<string> {
    return new Promise((resolve, reject) => {
      try {
        const data = this.storage.getItem(`${filename}/${version}`);
        if (data == null) {
          return reject(new Error(`File ${filename}/${version} not found.`));
        }
        return resolve(data);
      } catch (e) {
        return reject(e);
      }
    });
  }
}
