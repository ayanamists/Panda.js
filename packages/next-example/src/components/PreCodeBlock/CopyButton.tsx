"use client";

import { FaRegCopy } from "react-icons/fa6";
import { Button } from "@heroui/button";
import copy from 'copy-to-clipboard';
import { useState } from "react";
import { IoCheckmarkDoneOutline } from "react-icons/io5";


interface CopyButtonProps {
  code: string;
}

export default function CopyButton({ code }: CopyButtonProps) {
  const [isCopied, setIsCopied] = useState(false);

  return (<div className="hidden sm:block sm:absolute z-10 right-0">
    <Button isIconOnly aria-label="Copy to clipboard" variant='light' size='sm'
      onPress={() => {
        copy(code);
        setIsCopied(true);
        setTimeout(() => {
          setIsCopied(false);
        }, 2000);
      }}>

      {isCopied ?
        <IoCheckmarkDoneOutline /> :
        <FaRegCopy />
      }
    </Button>
  </div>);
}

