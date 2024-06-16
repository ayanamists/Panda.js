import { spawn } from 'child_process';
import config from "../panda.config.mjs";
import { fileURLToPath } from 'url';
import path from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const __root = path.dirname(__dirname);

async function markdownToJSX(markdown) {
  return new Promise((resolve, reject) => {
    const pandoc = spawn(`${__root}/bin/panda-exe`);

    let htmlText = '';
    let errorText = '';

    pandoc.stdout.on('data', (data) => {
      htmlText += data.toString();
    });

    pandoc.stderr.on('data', (data) => {
      errorText += data.toString();
    });

    pandoc.on('close', (code) => {
      if (code !== 0) {
        reject(new Error(`Pandoc exited with code ${code}: ${errorText}`));
        return;
      }

      resolve(htmlText);
    });

    pandoc.stdin.write(markdown);
    pandoc.stdin.end();
  });
}

function getTag(tag) {
  if (typeof tag === 'string') {
    return [`"${tag}"`, null];
  } else {
    return [tag.name, tag.path];
  }
}

function getComponentsTable(config) {
  const tag = [ 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 
    'p', 'div', 'em', "strong", "blockquote", 'a', 'code', 'pre', 'math',
    'u', 'strong', 'del', 'sup', 'sub', 'q', 'cite', 'br', 'note', 'span',
    'hr', 'figure', 'img',
    "ul", "ol", "li",
    "article"];
  let components = "{\n";
  let imports = new Set();
  for (const t of tag) {
    const data = config.transform(t);
    const tp = getTag(data.tag);
    const [tag, path] = tp;
    const add_props = data.add_props == null ? '{}' : JSON.stringify(data.add_props);
    components += `${t}: { tag: ${tag}, add_props: ${add_props} },\n`
    if (path != null) {
      imports.add(path);
    }
  }
  components += "}";
  return [components, [...imports]];
}

export default async function loader(source) {
  const jsx = await markdownToJSX(source);
  const line1 = 'import {Fragment as _Fragment, jsx as _jsx, jsxs as _jsxs} from "react/jsx-runtime";';
  const [components, imports] = getComponentsTable(config);
  const importsStr = imports.join('\n');
  const defs = `const _component = ${components}`;
  const content = `export default function Markdown() { return ${jsx}; }`;
  const res = `${line1}\n${importsStr}\n${defs}\n${content}`;
  return res;
}
