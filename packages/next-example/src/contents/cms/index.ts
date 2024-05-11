// TODO: avoid using gray-matter, this can only support markdown files
//       maybe use the metadata directly from pandoc (But currently it's a little complex)

import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import fs from 'fs';
import grayMatter from 'gray-matter';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

export type PostExt = 'md' | 'org';

export interface PostMetaData {
  language: string,
  title: string,
  date: Date,
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

function readMetaData() {
  // should ensure the file structure is correct
  // |- contents
  //   |- cms
  //     |- index.ts  (this file)
  //   |- post1.en.md
  const contentDir = dirname(__dirname);
  const files = fs.readdirSync(contentDir);
  const posts: Post[] = [];
  for (const file of files) {
    const fileExts = file.split('.');
    const ext = fileExts[fileExts.length - 1];
    if (fileExts.length < 3 || !isSupportedExt(ext)) {
      continue;
    }
    const id = fileExts[0];
    const language = fileExts[1];
    const fullPath = join(contentDir, file);
    const content = fs.readFileSync(fullPath, 'utf8');
    const metaData = parseMetaData(content, id, language, ext);
    const post: Post = {
      id: id,
      path: join(contentDir, file),
      metaData: metaData,
    }
    posts.push(post);
  }
  return posts;
}

function parseMetaData(content: string, id: string, language: string, ext: PostExt) {
  const data = grayMatter(content).data;
  const metaData: PostMetaData = {
    language: language,
    title: data.title ?? '',
    draft: data.draft ?? false,
    date: data.date == null ? new Date() : new Date(data.date),
    author: data.author ?? '',
    tags: data.tags ?? [],
    categories: data.categories ?? [],
    ext: ext,
    fullName: `${id}.${language}.${ext}`
  }
  return metaData;
}

export function getAllPosts() {
  return readMetaData();
}

export function getPostById(id: string, lang: string) {
  const posts = readMetaData();
  return posts.find(post => post.id === id && post.metaData.language === lang);
}