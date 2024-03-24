import { spawn } from 'child_process';

async function markdownToJSX(markdown) {
  return new Promise((resolve, reject) => {
    const pandoc = spawn('panda-exe');

    let htmlText = '';
    let errorText = '';

    pandoc.stdout.on('data', (data) => {
      htmlText += data.toString();
    });

    pandoc.stderr.on('data', (data) => {
      errorText += data.toString();
    });

    pandoc.on('exit', (code) => {
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

export default async function loader(source) {
  const jsx = await markdownToJSX(source);
  const res = `import {Fragment as _Fragment, jsx as _jsx, jsxs as _jsxs} from "react/jsx-runtime";\nexport default function Markdown() { return ${jsx}; }`;
  return res;
}