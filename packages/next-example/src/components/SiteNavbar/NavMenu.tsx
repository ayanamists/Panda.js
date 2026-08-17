"use client";

import { useEffect, useId, useRef, useState } from "react";

type Align = "center" | "left" | "right";

export default function NavMenu({
  trigger,
  children,
  align = "center",
  label,
}: {
  trigger: React.ReactNode;
  children: React.ReactNode;
  align?: Align;
  label: string;
}) {
  const [open, setOpen] = useState(false);
  const rootRef = useRef<HTMLDivElement>(null);
  const menuId = useId();

  useEffect(() => {
    if (!open) return;

    const onPointerDown = (event: PointerEvent) => {
      if (!rootRef.current?.contains(event.target as Node)) {
        setOpen(false);
      }
    };
    const onKeyDown = (event: KeyboardEvent) => {
      if (event.key === "Escape") setOpen(false);
    };

    document.addEventListener("pointerdown", onPointerDown);
    document.addEventListener("keydown", onKeyDown);
    return () => {
      document.removeEventListener("pointerdown", onPointerDown);
      document.removeEventListener("keydown", onKeyDown);
    };
  }, [open]);

  const panelAlign =
    align === "center"
      ? "left-1/2 -translate-x-1/2"
      : align === "right"
        ? "right-0"
        : "left-0";

  return (
    <div ref={rootRef} className="relative inline-flex items-center">
      <button
        type="button"
        className="inline-flex items-center appearance-none border-0 bg-transparent p-0 m-0 font-inherit text-[length:inherit] leading-[inherit] tracking-[inherit] cursor-pointer"
        aria-label={label}
        aria-haspopup="menu"
        aria-expanded={open}
        aria-controls={menuId}
        onClick={() => setOpen((value) => !value)}
      >
        {trigger}
      </button>
      {open && (
        <div
          id={menuId}
          role="menu"
          className={`absolute z-50 mt-2 min-w-full whitespace-nowrap rounded-md border border-foreground/[0.06] bg-background py-1 shadow-sm shadow-foreground/[0.04] ${panelAlign}`}
          onClick={() => setOpen(false)}
        >
          {children}
        </div>
      )}
    </div>
  );
}
