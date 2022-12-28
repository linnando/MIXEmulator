import { BlockAccessFileOps } from '@mixemulator/lib';
import { Buffer } from 'buffer';

export class BlockAccessFileOpsImpl extends BlockAccessFileOps {
  constructor(private storage: Storage) {
    super();
  }

  override readBlock(filename: string, version: number, position: number, blockSize: number): Promise<number[]> {
    return new Promise((resolve, reject) => {
      try {
        const data = this.storage.getItem(`${filename}/${version}`);
        if (data == null) {
          return reject(new Error(`File ${filename}/${version} not found.`));
        }
        const buffer = Buffer.from(data, 'base64');
        return resolve(Array.from(buffer.subarray(position, position + blockSize)));
      } catch (e) {
        return reject(e);
      }
    });
  }

  override writeBlock(filename: string, version: number, position: number, bytes: number[]): Promise<void> {
    const oldFile = `${filename}/${version}`;
    const newFile = `${filename}/${version + 1}`;
    return new Promise((resolve, reject) => {
      try {
        const srcData = this.storage.getItem(oldFile) ?? '';
        const srcBuffer = Buffer.from(srcData, 'base64');
        const destBuffer = Buffer.alloc(Math.max(srcBuffer.length, position + bytes.length));
        srcBuffer.copy(destBuffer);
        destBuffer.fill(Buffer.from(bytes), position, position + bytes.length);
        this.storage.setItem(newFile, destBuffer.toString('base64'));
        resolve();
      } catch (e) {
        return reject(e);
      }
    });
  }

  override async initialiseWithCurrentVersion(filename: string): Promise<void> {
    const versions = await this.getVersions(filename);
    if (versions.length == 0) {
      await this.save(filename, []);
    } else {
      const currentVersion = Math.max(
        ...versions.map(version => Number(version)).filter(version => !isNaN(version))
      );
      return new Promise((resolve, reject) => {
        try {
          const data = this.storage.getItem(`${filename}/${currentVersion}`) ?? '';
          for (const version of versions) {
            this.storage.removeItem(`${filename}/${version}`);
          }
          this.storage.setItem(`${filename}/0`, data);
          resolve();
        } catch (e) {
          return reject(e);
        }
      });
    }
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

  override save(filename: String, data: number[]): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        const buffer = Buffer.from(data);
        this.storage.setItem(`${filename}/0`, buffer.toString('base64'));
        return resolve();
      } catch (e) {
        return reject(e);
      }
    });
  }

  override getData(filename: string, version: number): Promise<number[]> {
    return new Promise((resolve, reject) => {
      try {
        const data = this.storage.getItem(`${filename}/${version}`);
        if (data == null) {
          return reject(new Error(`File ${filename}/${version} not found.`));
        }
        const buffer = Buffer.from(data, 'base64');
        return resolve(Array.from(buffer));
      } catch (e) {
        return reject(e);
      }
    });
  }
}
