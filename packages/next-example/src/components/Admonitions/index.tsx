"use client";
import { CiCircleInfo } from "react-icons/ci";
import { CiWarning } from "react-icons/ci";
import { CiSquareMore } from "react-icons/ci";

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

  return (<div className="not-prose border-l-4 border-l-primary shadow-md rounded-xl">
    <div className="pl-4 pr-3">
      <div className="pt-3">
        <div className="flex items-center font-bold gap-2">
          <span className="text-3xl">
            <Icon />
          </span>
          {t}
        </div>
      </div>
      <div className="pt-2 pb-3">
        {children}
      </div>
    </div>
  </div>);
}