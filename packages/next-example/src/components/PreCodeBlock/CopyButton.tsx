"use client";

import { FaRegCopy } from "react-icons/fa6";
import { Button } from "@nextui-org/button";
import copy from 'copy-to-clipboard';


interface CopyButtonProps {
  code: string;
}

export default function CopyButton({ code } : CopyButtonProps) {
  return (<div className="hidden sm:block sm:absolute z-10 right-0">
    <Button isIconOnly aria-label="Copy to clipboard" variant='light' size='sm'
      onPress={() => {
        copy(code);}
      }>
      <FaRegCopy />
    </Button>
  </div>);
}

