"use client";

import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter';
import { oneDark } from 'react-syntax-highlighter/dist/esm/styles/prism';

interface PreCodeBlockProps {
  children: string,
  className: string
}

// TODO: I find it very hard to switch code highlight with SSR.
//       Currently use the same
//       Try to figure out some solutions for it
function PreCodeBlock({ children, className }: PreCodeBlockProps) {
  const code = children;
  const language = className;
  return (
    <SyntaxHighlighter
     language={language} 
     style={oneDark}
     showLineNumbers={true}
    >
      {code}
    </SyntaxHighlighter>
  );
}

export default PreCodeBlock;
