import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const outDir = path.join(__dirname, '..', 'out');
const indexPath = path.join(outDir, 'index.html');

if (!fs.existsSync(outDir)) {
  console.error('out/ is missing; run yarn build first');
  process.exit(1);
}

// `output: 'export'` serializes next/navigation redirect() as a JS-only
// NEXT_REDIRECT digest inside an __next_error__ shell. That 200 HTML is
// what CrUX/Web Analytics measure as LCP. Replace it with a real
// no-JS redirect. Cloudflare Pages also honors public/_redirects.
const html = `<!DOCTYPE html>
<html lang="zh-cn">
<head>
  <meta charset="utf-8">
  <meta http-equiv="refresh" content="0; url=/zh-cn">
  <link rel="canonical" href="/zh-cn">
  <title>Redirecting…</title>
</head>
<body>
  <p><a href="/zh-cn">Continue to /zh-cn</a></p>
</body>
</html>
`;

fs.writeFileSync(indexPath, html);
console.log('rewrote out/index.html as a static redirect to /zh-cn');
