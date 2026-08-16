import fs from 'fs';
import http from 'http';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const outDir = path.resolve(__dirname, '..', 'out');
const port = Number(process.env.PORT || 4173);
const host = process.env.HOST || '127.0.0.1';

if (!fs.existsSync(outDir)) {
  console.error('out/ is missing. Run yarn build before serving the export.');
  process.exit(1);
}

const MIME = {
  '.css': 'text/css; charset=utf-8',
  '.html': 'text/html; charset=utf-8',
  '.ico': 'image/x-icon',
  '.jpeg': 'image/jpeg',
  '.jpg': 'image/jpeg',
  '.js': 'text/javascript; charset=utf-8',
  '.json': 'application/json',
  '.map': 'application/json',
  '.png': 'image/png',
  '.svg': 'image/svg+xml',
  '.txt': 'text/plain; charset=utf-8',
  '.woff': 'font/woff',
  '.woff2': 'font/woff2',
  '.xml': 'application/xml',
};

function parseRedirects() {
  const file = path.join(outDir, '_redirects');
  if (!fs.existsSync(file)) return [];
  return fs.readFileSync(file, 'utf8').split('\n').flatMap((line) => {
    const trimmed = line.trim();
    if (!trimmed || trimmed.startsWith('#')) return [];
    const parts = trimmed.split(/\s+/);
    if (parts.length < 3) return [];
    return [{ from: parts[0], to: parts[1], status: Number(parts[2]) || 302 }];
  });
}

const redirects = parseRedirects();

function resolveFile(urlPath) {
  const decoded = decodeURIComponent(urlPath.split('?')[0]);
  const rel = decoded === '/' ? '/index.html' : decoded;
  const candidates = [
    rel,
    `${rel}.html`,
    rel.replace(/\/$/, '') + '.html',
    path.join(rel, 'index.html'),
  ];
  for (const candidate of candidates) {
    const file = path.resolve(outDir, `.${candidate.startsWith('/') ? candidate : `/${candidate}`}`);
    if (!file.startsWith(outDir)) continue;
    if (fs.existsSync(file) && fs.statSync(file).isFile()) return file;
  }
  return null;
}

const server = http.createServer((req, res) => {
  const urlPath = req.url ?? '/';
  const pathname = urlPath.split('?')[0];
  const redirect = redirects.find((rule) => rule.from === pathname);
  if (redirect) {
    res.writeHead(redirect.status, { location: redirect.to });
    res.end();
    return;
  }

  const file = resolveFile(pathname);
  if (!file) {
    res.writeHead(404, { 'content-type': 'text/plain; charset=utf-8' });
    res.end('Not found');
    return;
  }

  const type = MIME[path.extname(file)] ?? 'application/octet-stream';
  res.writeHead(200, { 'content-type': type });
  if (req.method === 'HEAD') {
    res.end();
    return;
  }
  fs.createReadStream(file).pipe(res);
});

server.listen(port, host, () => {
  console.log(`export server http://${host}:${port}`);
});
