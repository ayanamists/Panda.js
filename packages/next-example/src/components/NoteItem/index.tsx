"use client";

import './index.module.css';
import 'react-tooltip/dist/react-tooltip.css';
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
    <Tooltip id={thisId} anchorSelect={returnIdRef} place="top" clickable opacity={1}>
      {children}
    </Tooltip>
  );
}
