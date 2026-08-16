"use client";

import React, { useState } from "react";
import './table.css';
import { FaCaretRight } from "react-icons/fa";


interface OrgResultProp {
  children: React.ReactNode;
}

const OrgResult: React.FC<OrgResultProp> = ({ children }) => {
  const [isOpen, setIsOpen] = useState(true);

  return (
    <div className="">
      <div className="flex items-center text-xl text-bold">
        <button
          type="button"
          aria-label="Toggle output"
          className="p-2 text-foreground/50 hover:text-foreground/80 transition-colors duration-200"
          onClick={() => {
            setIsOpen((open) => !open)
          }}
        >
          <div className={(isOpen ? "rotate-90" : "")}>
            <FaCaretRight />
          </div>
        </button>
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
