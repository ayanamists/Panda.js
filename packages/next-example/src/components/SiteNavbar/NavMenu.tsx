"use client";

import { useEffect, useId, useLayoutEffect, useRef, useState } from "react";

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
  const [coords, setCoords] = useState<{ top: number; left: number } | null>(null);
  const rootRef = useRef<HTMLDivElement>(null);
  const triggerRef = useRef<HTMLButtonElement>(null);
  const menuId = useId();

  useLayoutEffect(() => {
    if (!open || !triggerRef.current) {
      setCoords(null);
      return;
    }

    const place = () => {
      const box = triggerRef.current?.getBoundingClientRect();
      if (!box) return;
      const left =
        align === "right" ? box.right
        : align === "left" ? box.left
        : box.left + box.width / 2;
      setCoords({ top: box.bottom + 8, left });
    };

    place();
    window.addEventListener("resize", place);
    window.addEventListener("scroll", place, true);
    return () => {
      window.removeEventListener("resize", place);
      window.removeEventListener("scroll", place, true);
    };
  }, [open, align]);

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

  const shift =
    align === "right" ? "translateX(-100%)"
    : align === "left" ? "none"
    : "translateX(-50%)";

  return (
    <div ref={rootRef} className="relative inline-flex items-center">
      <button
        ref={triggerRef}
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
      {open && coords && (
        <div
          id={menuId}
          role="menu"
          className="fixed z-50 min-w-max whitespace-nowrap rounded-md border border-foreground/[0.06] bg-background py-1 shadow-sm shadow-foreground/[0.04]"
          style={{ top: coords.top, left: coords.left, transform: shift }}
          onClick={() => setOpen(false)}
        >
          {children}
        </div>
      )}
    </div>
  );
}
