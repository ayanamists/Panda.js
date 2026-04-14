import { PrismLight as SyntaxHighlighter } from 'react-syntax-highlighter';
import c from 'react-syntax-highlighter/dist/esm/languages/prism/c';
import cpp from 'react-syntax-highlighter/dist/esm/languages/prism/cpp';
import python from 'react-syntax-highlighter/dist/esm/languages/prism/python';
import haskell from 'react-syntax-highlighter/dist/esm/languages/prism/haskell';
import java from 'react-syntax-highlighter/dist/esm/languages/prism/java';
import scheme from 'react-syntax-highlighter/dist/esm/languages/prism/scheme';
import fsharp from 'react-syntax-highlighter/dist/esm/languages/prism/fsharp';
import csharp from 'react-syntax-highlighter/dist/esm/languages/prism/csharp';
import ocaml from 'react-syntax-highlighter/dist/esm/languages/prism/ocaml';
import scala from 'react-syntax-highlighter/dist/esm/languages/prism/scala';
import bash from 'react-syntax-highlighter/dist/esm/languages/prism/bash';
import lisp from 'react-syntax-highlighter/dist/esm/languages/prism/lisp';
import nasm from 'react-syntax-highlighter/dist/esm/languages/prism/nasm';
// @ts-ignore
import wolfram from 'react-syntax-highlighter/dist/esm/languages/prism/wolfram';
import markup from 'react-syntax-highlighter/dist/esm/languages/prism/markup';

SyntaxHighlighter.registerLanguage('c', c);
SyntaxHighlighter.registerLanguage('cpp', cpp);
SyntaxHighlighter.registerLanguage('python', python);
SyntaxHighlighter.registerLanguage('haskell', haskell);
SyntaxHighlighter.registerLanguage('java', java);
SyntaxHighlighter.registerLanguage('scheme', scheme);
SyntaxHighlighter.registerLanguage('fsharp', fsharp);
SyntaxHighlighter.registerLanguage('csharp', csharp);
SyntaxHighlighter.registerLanguage('ocaml', ocaml);
SyntaxHighlighter.registerLanguage('scala', scala);
SyntaxHighlighter.registerLanguage('bash', bash);
SyntaxHighlighter.registerLanguage('lisp', lisp);
SyntaxHighlighter.registerLanguage('nasm', nasm);
SyntaxHighlighter.registerLanguage('mathematica', wolfram);
SyntaxHighlighter.registerLanguage('markup', markup);

// Register aliases used in blog posts
SyntaxHighlighter.registerLanguage('c++', cpp);
SyntaxHighlighter.registerLanguage('shell', bash);
SyntaxHighlighter.registerLanguage('elisp', lisp);
SyntaxHighlighter.registerLanguage('Haskell', haskell);
SyntaxHighlighter.registerLanguage('Java', java);
SyntaxHighlighter.registerLanguage('Python', python);
SyntaxHighlighter.registerLanguage('Scala', scala);

import styles from './styles';
import CopyButton from "./CopyButton";
import './index.css';

interface PreCodeBlockProps {
  children: React.ReactElement;
  className?: string;
  exports?: "string";
}

interface CodeBlockProps {
  code: string,
  language: string
}

function CodeBlock({ code, language }: CodeBlockProps) {
  return (
    <div className="border border-solid rounded-md not-prose
      border-foreground/10
      relative">
      <CopyButton code={code} />
      <SyntaxHighlighter
        language={language}
        // @ts-ignore
        style={styles}
        showLineNumbers={true}
        showInlineLineNumbers={true}
        lineNumberStyle={{
          minWidth: "1.6em",
          textAlign: "right",
          paddingRight: "0",
          marginRight: "1.5em",
          fontStyle: "italic",
        }}
      >
        {code.trim()}
      </SyntaxHighlighter>
    </div>
  );
}

function PreCodeBlock({ children, className }: PreCodeBlockProps) {
  const classNames = className?.split(" ");
  const language = classNames?.[0];
  let code = "";
  if (typeof children === "string") {
    code = children;
  } else if (Array.isArray(children)) {
    if (typeof children[0] === "string") {
      code = children[0];
    }
  }

  return (<CodeBlock code={code} language={language ?? ""} />);
}

export default PreCodeBlock;
