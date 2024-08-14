"use client";

import { useTheme } from "next-themes";
import NextTopLoader from "nextjs-toploader"

export default function TopLoader() {
  const { theme } = useTheme();
  const color = (theme == 'light') ? "#cb4b16" : "#d33682";
  return (
    <NextTopLoader
      showSpinner={false}
      color={color}
    />);
}
