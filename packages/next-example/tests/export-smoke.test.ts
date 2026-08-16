import assert from 'node:assert/strict';
import fs from 'node:fs';
import http from 'node:http';
import path from 'node:path';
import test from 'node:test';
import { fileURLToPath } from 'node:url';

const root = path.join(path.dirname(fileURLToPath(import.meta.url)), '..');
const outDir = path.join(root, 'out');

function requireOut() {
  if (!fs.existsSync(outDir)) {
    throw new Error('out/ is missing. Run yarn build before yarn test:export');
  }
}

function readOut(rel: string) {
  requireOut();
  const file = rel.endsWith('.html')
    ? path.join(outDir, rel)
    : path.join(outDir, rel, 'index.html');
  const direct = path.join(outDir, rel);
  if (fs.existsSync(file)) return fs.readFileSync(file, 'utf8');
  if (fs.existsSync(direct)) return fs.readFileSync(direct, 'utf8');
  throw new Error(`missing export: ${rel}`);
}

test('exported / is a no-JS redirect, not an __next_error__ shell', () => {
  const html = readOut('index.html');
  assert.doesNotMatch(html, /id="__next_error__"/);
  assert.doesNotMatch(html, /NEXT_REDIRECT/);
  assert.match(html, /http-equiv="refresh"[^>]*url=\/zh-cn/i);
  assert.match(html, /href="\/zh-cn"/);
});

test('Cloudflare _redirects is copied into the export', () => {
  requireOut();
  const text = fs.readFileSync(path.join(outDir, '_redirects'), 'utf8');
  assert.match(text, /^\/\s+\/zh-cn\s+30[12]\s*$/m);
});

test('homepage HTML paints the name without an error document', () => {
  const html = readOut('zh-cn.html');
  assert.doesNotMatch(html, /id="__next_error__"/);
  assert.match(html, /李晨曦|aria-label="李晨曦"/);
  assert.doesNotMatch(html, /<h1[^>]*animate-fade/);
});

test('article HTML is visible in the first document (no opacity:0 wrapper)', () => {
  const html = readOut('zh-cn/posts/hindley-milner.html');
  assert.doesNotMatch(html, /id="__next_error__"/);
  assert.match(html, /<article/);
  assert.match(html, /类型推理|Hindley-Milner/);
  assert.doesNotMatch(html, /<div style="opacity:0"/);
  assert.doesNotMatch(html, /NEXT_REDIRECT/);
});

test('article page first-load JS+CSS stays under the old 1.8MB trap', () => {
  const html = readOut('zh-cn/posts/hindley-milner.html');
  const urls = [...html.matchAll(/(?:src|href)="(\/_next\/static\/[^"]+\.(?:js|css))"/g)]
    .map((m) => m[1]);
  const unique = [...new Set(urls)];
  let bytes = 0;
  for (const url of unique) {
    if (url.includes('polyfills')) continue;
    const file = path.join(outDir, decodeURIComponent(url).replace(/^\//, ''));
    assert.equal(fs.existsSync(file), true, `missing ${url}`);
    bytes += fs.statSync(file).size;
  }
  const kb = bytes / 1024;
  assert.ok(kb < 800, `modern JS+CSS is ${kb.toFixed(1)} KB; expected < 800 KB`);
});

test('static file server serves / as a redirect document', async () => {
  requireOut();
  const server = http.createServer((req, res) => {
    const urlPath = req.url === '/' ? '/index.html' : req.url ?? '/index.html';
    const file = path.join(outDir, decodeURIComponent(urlPath));
    if (!file.startsWith(outDir) || !fs.existsSync(file) || fs.statSync(file).isDirectory()) {
      res.writeHead(404);
      res.end();
      return;
    }
    res.writeHead(200, { 'content-type': file.endsWith('.html') ? 'text/html' : 'text/plain' });
    res.end(fs.readFileSync(file));
  });
  await new Promise<void>((resolve) => server.listen(0, '127.0.0.1', resolve));
  const { port } = server.address() as { port: number };
  try {
    const rootRes = await fetch(`http://127.0.0.1:${port}/`);
    const rootHtml = await rootRes.text();
    assert.equal(rootRes.status, 200);
    assert.match(rootHtml, /url=\/zh-cn/i);
    assert.doesNotMatch(rootHtml, /__next_error__/);

    const home = await fetch(`http://127.0.0.1:${port}/zh-cn.html`);
    const homeHtml = await home.text();
    assert.equal(home.status, 200);
    assert.match(homeHtml, /李晨曦|aria-label="李晨曦"/);
  } finally {
    await new Promise<void>((resolve, reject) =>
      server.close((err) => (err ? reject(err) : resolve())));
  }
});
