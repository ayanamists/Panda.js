import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter';
import styles from './styles';
import CopyButton from "./CopyButton";
import './index.css';

interface PreCodeBlockProps {
  children: string,
  className: string
}

function PreCodeBlock({ children, className }: PreCodeBlockProps) {
  const code = children;
  const language = className;
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
        minWidth: "2em",
        textAlign: "right",
        paddingRight: "0",
        marginRight: "1em",
        fontStyle: "italic",
      }}
    >
      {code}
    </SyntaxHighlighter>
    </div>
  );
}

export default PreCodeBlock;
