import { BlockMath, InlineMath } from "react-katex";
import "katex/dist/katex.min.css";
import { joinChildren } from "@/utils";

interface MathProps {
  children: React.ReactDOM;
  display: "inline" | "block";
}

function Math({ children, display }: MathProps) {
  const math = joinChildren(children, "");
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
