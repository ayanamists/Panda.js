"use client";

import { useEffect, useState } from "react";
import MoonIcon from "./MoonIcon";
import SunIcon from "./SunIcon";
import { useTheme } from "next-themes";

// TODO: any way to remove the need of lazy loading? Current impl will cause a flash on load
export default function ThemeSwitcher() {
  const [mounted, setMounted] = useState(false);
  const { theme, setTheme } = useTheme();

  useEffect(() => {
    setMounted(true);
  }, [])

  if (!mounted) return <div className="w-5 h-5" />

  return (
    <button
      aria-label="Theme Switch"
      className="text-foreground/30 hover:text-foreground/60 transition-colors duration-200 p-1"
      onClick={() => setTheme(theme === "dark" ? "light" : "dark")}
    >
      <div className="w-[14px] h-[14px]">
        {(theme === "light") ? <MoonIcon /> : <SunIcon />}
      </div>
    </button>
  );
}
