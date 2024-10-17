import NoteItem from "@/components/NoteItem";
import Admonitions from "@/components/Admonitions";
import TableOfContents from "@/components/TableOfContents";
import OrgResult from "@/components/OrgResult";
import CodeWithResult from "../CodeWithResult";

export default function Div({ children, ...rest }: { children: React.ReactNode, rest: any }) {
  const div = parseDiv({ children, ...rest });
  if (div) {
    return div;
  } else {
    return (
      <div {...rest}>
        {children}
      </div>
    );
  }
}

function parseDiv({ children, ...rest }: { children: React.ReactNode, rest: any }) {
  if ("className" in rest) {
    if (rest.className === "note-item") {
      const props = {
        itemId: selectDivProps(rest, "data-note-id"),
        id: selectDivProps(rest, "id"),
      };
      return (<NoteItem {...props}>{children}</NoteItem>);
    } else if (rest.className === "admonition") {
      const props = {
        type: selectDivProps(rest, "data-admonition-type"),
      };
      return (<Admonitions {...props}>{children}</Admonitions>);
    } else if (rest.className === "toc") {
      return (<TableOfContents>{children}</TableOfContents>);
    } else if (rest.className === "org-result") {
      return (<OrgResult>{children}</OrgResult>);
    } else if (rest.className === "code-with-result") {
      return (<CodeWithResult>{children}</CodeWithResult>);
    }
  }
}

function selectDivProps(props: any, key: string) {
  if (key in props) {
    const data = props[key];
    if (typeof data === "object") {
      return data.toString();
    } else if (typeof data === "string") {
      return data;
    } else if (typeof data === "number") {
      return data.toString();
    }
  }
}
