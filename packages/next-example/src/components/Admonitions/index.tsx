"use client";
import { CiCircleInfo } from "react-icons/ci";
import { CiWarning } from "react-icons/ci";
import { CiSquareMore } from "react-icons/ci";
import React from "react";

type AdmonitionsType = 'NOTE' | 'WARNING' | 'OTHER'

interface AdmonitionsProps {
  type: AdmonitionsType;
  children: React.ReactNode;
}

export default function Admonitions({ type, children }: AdmonitionsProps) {
  const t = type.toLocaleUpperCase();
  const Icon = {
    "NOTE": CiCircleInfo,
    "WARNING": CiWarning
  }[t] ?? CiSquareMore;

  // TODO: avoid map and clone
  return (<div className="border-l-4 border-l-primary shadow-md rounded-xl mb-3 mt-3">
    <div className="pl-4 pr-3">
      <div className="pt-3">
        <div className="flex items-center font-bold gap-2">
          <span className="text-3xl">
            <Icon />
          </span>
          {t}
        </div>
      </div>
      <div className="pb-3 pt-3">
        {React.Children.map(children, (child: any, index) =>
          React.cloneElement(child, {
            className: `first:mt-0 last:mb-0 ${child.props.className || ''}`
          }))
        }
      </div>
    </div>
  </div>);
}
