"use client";

import { useTheme } from "next-themes";
import NextTopLoader from "nextjs-toploader"

export default function TopLoader() {
  const { theme } = useTheme();
  const color = (theme == 'light') ? "#5b7f6a" : "#6aab8e";
  return (
    <NextTopLoader
      showSpinner={false}
      color={color}
    />);
}
