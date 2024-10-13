import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter';
import styles from './styles';
import CopyButton from "./CopyButton";
import './index.css';

interface PreCodeBlockProps {
  children: React.ReactDOM;
  className?: string;
}

interface CodeBlockProps {
  code: string,
  language: string
}

function CodeBlock({ code, language }: CodeBlockProps) {
  return (
    <div className="border border-solid rounded-md not-prose
      border-zinc-400
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
      {code}
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
