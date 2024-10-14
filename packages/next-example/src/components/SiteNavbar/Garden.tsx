"use client";

import { Dropdown, DropdownMenu, DropdownItem } from "@nextui-org/dropdown";
import React from "react";
import { Link } from "@/navigation";
import NavbarButton from "./NavbarButton";
import { IoSparkles } from "react-icons/io5";

interface GardenProps {
  name: string;
  item: {
    name: string;
    link: string;
    icon: React.ReactNode;
  }[]
}

export default function Garden( { name, item } : GardenProps) {
  return (
    <Dropdown className="bg-background min-w-20">
      <NavbarButton name={name} dropdown={true} icon={<IoSparkles />} link={"/favorites"}  />
      <DropdownMenu
        aria-label="favorite selects"
        className="w-auto min-w-20"
        itemClasses={{
          base: "gap-4",
        }}
      >
        {item.map((i) =>
          <DropdownItem
            className="gap-1"
            key={`/favorites/${i.link}`}
            startContent={i.icon}
            as={Link}
            href={`/favorites/${i.link}`}
          >
            {i.name}
          </DropdownItem>)
        }
      </DropdownMenu>
    </Dropdown>);
}
