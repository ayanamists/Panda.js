"use client";

import { Link, usePathname } from "@/navigation";
import { navLinkClass, NavUnderline } from "./NavbarButton";
import NavMenu from "./NavMenu";

interface GardenProps {
  name: string;
  item: {
    name: string;
    link: string;
  }[]
}

export default function Garden({ name, item }: GardenProps) {
  const path = usePathname();
  const highlight = path.includes("favorites");

  return (
    <NavMenu
      label={name}
      align="center"
      trigger={
        <span className={navLinkClass(highlight)}>
          {name}
          <NavUnderline highlight={highlight} />
        </span>
      }
    >
      {item.map((i) =>
        <Link
          key={i.link}
          role="menuitem"
          href={`/favorites/${i.link}` as "/"}
          className="block px-3 py-1.5 text-center text-[13px] text-foreground/60 hover:bg-foreground/[0.04] hover:text-foreground"
        >
          {i.name}
        </Link>)}
    </NavMenu>
  );
}
