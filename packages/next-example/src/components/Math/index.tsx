import { BlockMath, InlineMath } from "react-katex";
import "katex/dist/katex.min.css";

interface MathProps {
  children: string[],
  display: "inline" | "block"
}

function Math({ children, display }: MathProps) {
  const math = children.join("");
  if (display === "inline") {
    return <InlineMath math={math} />;
  } else {
    return <BlockMath math={math} />;
  }
}

export default Math;