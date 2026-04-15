"use client";

import { DropdownTrigger }  from "@heroui/dropdown";
import { usePathname, Link } from "@/navigation";

interface NavbarButtonProps {
  name: string;
  dropdown?: boolean;
  link: string;
}

export default function NavbarButton({ name, dropdown = false, link }: NavbarButtonProps) {
  const path = usePathname();
  const highlight = (link === '/' && path === '/') ||
                    (link !== '/' && path.includes(link.substring(1)));

  const base = "relative font-heading text-[12.5px] tracking-[0.08em] uppercase transition-colors duration-300";
  const state = highlight
    ? "text-foreground/90"
    : "text-foreground/35 hover:text-foreground/65";
  const classes = `${base} ${state}`;

  /* Animated underline accent for active link */
  const underline = (
    <span
      className={`absolute -bottom-1 left-0 h-px bg-primary/50 transition-all duration-300 ${
        highlight ? "w-full" : "w-0"
      }`}
    />
  );

  if (dropdown) {
    return (
      <DropdownTrigger>
        <button className={`${classes} cursor-pointer`} aria-label={name}>
          {name}
          {underline}
        </button>
      </DropdownTrigger>
    );
  }

  const isExternal = link.startsWith('http');
  if (isExternal) {
    return (
      <a href={link} target="_blank" rel="noopener noreferrer" className={`${classes} group`}>
        {name}
        <span className="text-[9px] ml-0.5 opacity-30 group-hover:opacity-50 transition-opacity duration-300">↗</span>
        {underline}
      </a>
    );
  }

  return (
    <Link href={link as "/"} className={classes}>
      {name}
      {underline}
    </Link>
  );
}
