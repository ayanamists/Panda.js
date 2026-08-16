import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import test from 'node:test';
import { fileURLToPath } from 'node:url';

const root = path.join(path.dirname(fileURLToPath(import.meta.url)), '..');
const srcRoot = path.join(root, 'src');

function walk(dir: string): string[] {
  const out: string[] = [];
  for (const ent of fs.readdirSync(dir, { withFileTypes: true })) {
    const p = path.join(dir, ent.name);
    if (ent.isDirectory()) out.push(...walk(p));
    else if (/\.(tsx|ts|jsx|js|mjs|css)$/.test(ent.name)) out.push(p);
  }
  return out;
}

test('post page renders PostContent as a server component', () => {
  const page = fs.readFileSync(
    path.join(srcRoot, 'app/[locale]/posts/[slug]/page.tsx'),
    'utf8',
  );
  assert.match(page, /import PostContent/);
  assert.doesNotMatch(page, /AnimatedPostContent/);
  assert.doesNotMatch(page, /framer-motion/);
  assert.doesNotMatch(page, /initial=\{\{\s*opacity:\s*0/);
});

test('source does not pull HeroUI back onto the critical path', () => {
  const hits = walk(srcRoot).flatMap((file) => {
    const text = fs.readFileSync(file, 'utf8');
    return /from\s+['"]@heroui\//.test(text) ? [path.relative(root, file)] : [];
  });
  assert.deepEqual(hits, []);
});

test('homepage LCP name is not faded in from opacity 0', () => {
  const page = fs.readFileSync(
    path.join(srcRoot, 'app/[locale]/page.tsx'),
    'utf8',
  );
  assert.match(page, /<h1>/);
  assert.doesNotMatch(page, /<h1[^>]*animate-fade/);
});

test('draft posts are not statically generated', () => {
  const page = fs.readFileSync(
    path.join(srcRoot, 'app/[locale]/posts/[slug]/page.tsx'),
    'utf8',
  );
  assert.match(page, /filter\(post => !post\.metaData\.draft\)/);
});

test('every [locale] page calls setRequestLocale for static export', () => {
  const appLocale = path.join(srcRoot, 'app/[locale]');
  const pages = walk(appLocale).filter((file) => path.basename(file) === 'page.tsx');
  assert.ok(pages.length > 0);
  const missing = pages.filter((file) => {
    const text = fs.readFileSync(file, 'utf8');
    return !text.includes('setRequestLocale');
  }).map((file) => path.relative(root, file));
  assert.deepEqual(missing, []);
});

test('static hosting has a real root redirect, not only next/navigation redirect()', () => {
  const redirects = fs.readFileSync(path.join(root, 'public/_redirects'), 'utf8');
  assert.match(redirects, /^\/\s+\/zh-cn\s+30[12]\s*$/m);
  assert.equal(
    fs.existsSync(path.join(root, 'scripts/fix-static-root.mjs')),
    true,
  );
});
