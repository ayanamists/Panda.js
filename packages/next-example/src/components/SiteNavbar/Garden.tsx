"use client";

import { Dropdown, DropdownMenu, DropdownItem } from "@heroui/dropdown";
import React from "react";
import { Link } from "@/navigation";
import NavbarButton from "./NavbarButton";

interface GardenProps {
  name: string;
  item: {
    name: string;
    link: string;
  }[]
}

export default function Garden( { name, item } : GardenProps) {
  return (
    <Dropdown className="bg-background min-w-24 border border-foreground/[0.06] shadow-sm shadow-foreground/[0.04] rounded-md">
      <NavbarButton name={name} dropdown={true} link={"/favorites"} />
      <DropdownMenu
        aria-label="favorite selects"
        className="w-auto min-w-24"
        itemClasses={{
          base: "gap-2 text-[13px] text-foreground/60 data-[hover=true]:text-foreground data-[hover=true]:bg-foreground/[0.04]",
        }}
      >
        {item.map((i) =>
          <DropdownItem
            key={`/favorites/${i.link}`}
            as={Link}
            href={`/favorites/${i.link}`}
          >
            {i.name}
          </DropdownItem>)
        }
      </DropdownMenu>
    </Dropdown>);
}
