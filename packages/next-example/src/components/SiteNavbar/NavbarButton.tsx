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

  const classes = `text-[13px] tracking-wide transition-colors duration-200 ${
    highlight
      ? "text-foreground"
      : "text-foreground/40 hover:text-foreground/70"
  }`;

  if (dropdown) {
    return (
      <DropdownTrigger>
        <button className={classes} aria-label={name}>
          {name}
        </button>
      </DropdownTrigger>
    );
  }

  const isExternal = link.startsWith('http');
  if (isExternal) {
    return (
      <a href={link} target="_blank" rel="noopener noreferrer" className={classes}>
        {name}<span className="text-[10px] ml-0.5 opacity-40">↗</span>
      </a>
    );
  }

  return (
    <Link href={link as "/"} className={classes}>
      {name}
    </Link>
  );
}
