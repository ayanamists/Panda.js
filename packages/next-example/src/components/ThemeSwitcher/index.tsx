"use client";

import { useEffect, useState } from "react";
import MoonIcon from "./MoonIcon";
import SunIcon from "./SunIcon";
import { useTheme } from "next-themes";
import { Button } from "@heroui/button";

// TODO: any way to remove the need of lazy loading? Current impl will cause a flash on load 
export default function ThemeSwitcher() {
  const [mounted, setMounted] = useState(false);
  const { theme, setTheme } = useTheme();

  useEffect(() => {
    setMounted(true);
  }, [])

  if (!mounted) return null

  return (
    <Button isIconOnly aria-label="Theme Switch" variant="light"
      onPress={() => {
        setTheme(theme === "dark" ? "light" : "dark");
      }}
    >
      <div className="text-foreground">
      {(theme === "light") ? <MoonIcon /> : <SunIcon />}
      </div>
    </Button>  
  );
}
