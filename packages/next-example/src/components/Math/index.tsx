import { BlockMath, InlineMath } from "react-katex";
import "katex/dist/katex.min.css";

interface MathProps {
  children: string[],
  display: "inline" | "block"
}

function Math({ children, display }: MathProps) {
  const math = children.join("");
  if (display === "inline") {
    // TODO: how to handle overflow?
    return <InlineMath math={math} />;
  } else {
    return (
      <div className="overflow-auto">
        <BlockMath math={math} />
      </div>
    );  
  }
}

export default Math;