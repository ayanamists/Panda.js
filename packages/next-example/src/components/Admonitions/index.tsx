import { CiCircleInfo } from "react-icons/ci";
import { CiWarning } from "react-icons/ci";
import { CiSquareMore } from "react-icons/ci";
import React from "react";
import styles from './index.module.css';

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

  return (<div className={`${styles.admonition} border-l-4 border-l-primary rounded-md bg-primary/[0.04]`}>
    <div className={`${styles.title} font-heading text-primary`}>
      <Icon aria-hidden="true" />
      <span>{t}</span>
    </div>
    <div className={styles.content}>
      {children}
    </div>
  </div>);
}
