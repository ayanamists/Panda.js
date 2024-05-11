"use client";

import './index.module.css';
import { Tooltip } from "react-tooltip";

interface NoteItemProps {
  id: string;
  itemId: string;
  children: React.ReactNode;
}

// @ts-ignore
export default function NoteItem({ children, itemId, id }: NoteItemProps) {
  const thisId = `note-${itemId}`;
  const returnIdRef = `#note-anchor-${itemId}`;
  return (
    // <div className="note-item" id={thisId}>
    //   <Link href={thisIdRef} className="pr-3">
    //     <sup>{itemId}</sup>
    //   </Link>
    //   {children}
    //   <Link href={returnIdRef} className="pl-3">
    //     ⏎
    //   </Link>
    // </div>
    <Tooltip id={thisId} anchorSelect={returnIdRef} place="top" clickable opacity={1}>
      {children}
    </Tooltip>
  );
}