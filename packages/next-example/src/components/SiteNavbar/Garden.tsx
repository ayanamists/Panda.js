"use client";

import { Link, usePathname } from "@/navigation";
import { navLinkClass, NavUnderline } from "./NavbarButton";

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
    <details className="nav-details relative">
      <summary className={`${navLinkClass(highlight)} cursor-pointer`}>
        {name}
        <NavUnderline highlight={highlight} />
      </summary>
      <div className="absolute left-0 z-50 mt-2 min-w-24 rounded-md border border-foreground/[0.06] bg-background py-1 shadow-sm shadow-foreground/[0.04]">
        {item.map((i) =>
          <Link
            key={i.link}
            href={`/favorites/${i.link}` as "/"}
            className="block px-3 py-1.5 text-[13px] text-foreground/60 hover:bg-foreground/[0.04] hover:text-foreground"
          >
            {i.name}
          </Link>)}
      </div>
    </details>
  );
}
