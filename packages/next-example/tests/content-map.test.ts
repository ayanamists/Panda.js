import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import test from 'node:test';
import { fileURLToPath } from 'node:url';

const root = path.join(path.dirname(fileURLToPath(import.meta.url)), '..');
const postsDir = path.join(root, 'src/contents/_posts');
const mapPath = path.join(root, 'src/contents/index.tsx');

function listedPostFiles(): string[] {
  return fs.readdirSync(postsDir).filter((name) => {
    const parts = name.split('.');
    const ext = parts[parts.length - 1];
    return parts.length >= 3 && (ext === 'md' || ext === 'org');
  }).sort();
}

function mappedPostFiles(): string[] {
  const src = fs.readFileSync(mapPath, 'utf8');
  return [...src.matchAll(/'([^']+)': \(\) => import\('\.\/_posts\/\1'\)/g)]
    .map((m) => m[1])
    .sort();
}

test('contents/index.tsx lists every markdown/org post file', () => {
  assert.deepEqual(mappedPostFiles(), listedPostFiles());
});

test('contents/index.tsx does not use next/dynamic', () => {
  const src = fs.readFileSync(mapPath, 'utf8');
  assert.equal(src.includes('next/dynamic'), false);
  assert.match(src, /export default async function Post/);
});
