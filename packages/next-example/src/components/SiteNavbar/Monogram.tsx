"use client";

import { Link } from "@/navigation";

export default function Monogram() {
  return (
    <Link
      href={"/"}
      aria-label="Home"
      className="group relative flex items-center justify-center w-7 h-7"
    >
      {/* Subtle ring that appears on hover */}
      <span className="absolute inset-0 rounded-full border border-foreground/0 group-hover:border-foreground/10 transition-all duration-300 scale-90 group-hover:scale-100" />
      {/* Lambda monogram */}
      <span className="font-serif italic text-[15px] text-primary/60 group-hover:text-primary transition-colors duration-300 select-none">
        λ
      </span>
    </Link>
  );
}
