import { spawn } from 'child_process';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import fs from 'fs';
const { stat } = fs.promises;
import cacache from 'cacache';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

export type PostExt = 'md' | 'org';

export interface PostMetaData {
  language: string,
  title: string,
  date: string,
  draft: boolean,
  author: string,
  tags: string[],
  categories: string[],
  ext: PostExt,
  fullName: string,
}

export interface Post {
  id: string,
  path: string,
  metaData: PostMetaData,
}

function isSupportedExt(ext: string): ext is PostExt {
  return ext === 'md' || ext === 'org';
}

async function readMetaData(id: string | null, language: string | null) {
  // should ensure the file structure is correct
  // |- contents
  //   |- cms
  //     |- index.ts  (this file)
  //   |- _posts
  //     |- post1.en.md
  const contentDir = join(dirname(__dirname), "_posts");
  const files = fs.readdirSync(contentDir)
  const posts: Post[] = [];
  for (const file of files) {
    const fileExts = file.split('.');
    const ext = fileExts[fileExts.length - 1];
    if (fileExts.length < 3 || !isSupportedExt(ext)) {
      continue;
    }
    const _id = fileExts[0];
    if (id != null && id !== _id) continue;
    const _language = fileExts[1];
    if (language != null && language !== _language) continue;
    const fullPath = join(contentDir, file);
    const metaData = await getCachedMetaData(fullPath, _id, _language, ext);
    const post: Post = {
      id: _id,
      path: join(contentDir, file),
      metaData: metaData,
    }
    posts.push(post);
  }
  return posts;
}

function callPanda(fullPath: string) {
  return new Promise((resolve, reject) => {
    let res = "";
    let error = "";
    const panda = spawn(`panda-exe`, ['-m', '-i', fullPath]);
    panda.stdout.on('data', (data) => {
      res += data.toString();
    });

    panda.stderr.on('data', (data) => {
      error += data.toString();
    });

    panda.on('close', (code) => {
      if (code !== 0) {
        reject(new Error(`Panda exited with code ${code}: ${error}`));
        return;
      }
      resolve(JSON.parse(res));
    });
  });
}

async function parseMetaData(fullPath: string, id: string, language: string, ext: PostExt) {
  const data: any = await callPanda(fullPath);
  const metaData: PostMetaData = {
    language: language,
    title: data.title ?? '',
    draft: data.draft ?? false,
    date: data.date ?? new Date().toLocaleDateString(),
    author: data.author ?? '',
    tags: data.tags ?? [],
    categories: data.categories ?? [],
    ext: ext,
    fullName: `${id}.${language}.${ext}`
  }
  return metaData;
}

async function getCachedMetaData(fullPath: string, id: string, language: string, ext: PostExt) {
  // FIXME: hard coded is evil,
  const cachePath = '/tmp/panda';
  const stats = await stat(fullPath);
  const lastModified = stats.mtimeMs;

  // TRY-CATCH ? Far from good. But seems no better way for cacache
  try {
    const { data } = await cacache.get(cachePath, fullPath);
    const obj: any = JSON.parse(data.toString());
    if (obj.lastModified === lastModified) {
      return obj.data;
    } else {
      throw Error("cache invalid");
    }
  } catch (e) {
    const data = await parseMetaData(fullPath, id, language, ext);
    const buffer = Buffer.from(JSON.stringify({ data, lastModified }));
    cacache.put(cachePath, fullPath, buffer);
    return data;
  }
}

export const getAllPosts = () => {
  return readMetaData(null, null);
}

export async function getPostById(id: string, lang: string) {
  const posts = await readMetaData(id, lang);
  return posts[0];
}

export function getPostByLang(lang: string) {
  return readMetaData(null, lang);
}
