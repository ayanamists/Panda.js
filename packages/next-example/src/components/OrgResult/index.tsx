"use client";

import React, { useState } from "react";
import './table.css';
import { FaCaretRight } from "react-icons/fa";
import { Button } from "@nextui-org/button";


interface OrgResultProp {
  children: React.ReactNode;
}

const OrgResult: React.FC<OrgResultProp> = ({ children }) => {
  const [isOpen, setIsOpen] = useState(true);

  return (
    <div className="">
      <div className="flex items-center text-xl text-bold">
        <Button isIconOnly variant="light"
          onPress={() => {
            setIsOpen((open) => !open)
          }}
        >
          <div className={(isOpen ? "rotate-90" : "")}>
            <FaCaretRight />
          </div>
        </Button>
        <span className="font-mono">Output</span>
      </div>
      {isOpen &&
        <div className="org-table font-mono mt-0 not-prose mb-2 ml-12">
          {children}
        </div>
      }
    </div>);
}

export default OrgResult;
