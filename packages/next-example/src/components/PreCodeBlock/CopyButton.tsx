"use client";

import { FaRegCopy } from "react-icons/fa6";
import copy from 'copy-to-clipboard';
import { useState } from "react";
import { IoCheckmarkDoneOutline } from "react-icons/io5";


interface CopyButtonProps {
  code: string;
}

export default function CopyButton({ code }: CopyButtonProps) {
  const [isCopied, setIsCopied] = useState(false);

  return (<div className="hidden sm:block sm:absolute z-10 right-0">
    <button
      type="button"
      aria-label="Copy to clipboard"
      className="p-2 text-foreground/40 hover:text-foreground/70 transition-colors duration-200"
      onClick={() => {
        copy(code);
        setIsCopied(true);
        setTimeout(() => {
          setIsCopied(false);
        }, 2000);
      }}
    >
      {isCopied ?
        <IoCheckmarkDoneOutline /> :
        <FaRegCopy />
      }
    </button>
  </div>);
}
