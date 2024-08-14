"use client";

import { NavbarItem } from "@nextui-org/navbar";
import { Dropdown, DropdownTrigger, DropdownMenu, DropdownItem } from "@nextui-org/dropdown";
import React from "react";
import { useRouter } from "@/navigation";
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
  const rounter = useRouter();
  return (
    <Dropdown className="bg-background min-w-20">
      <NavbarButton name={name} dropdown={true} icon={<IoSparkles />} link={"/favorites"}  />
      <DropdownMenu
        aria-label="favorite selects"
        className="w-auto min-w-20"
        itemClasses={{
          base: "gap-4",
        }}
        onAction={(key) => {
          rounter.push(key as string)
        }}
      >
        {item.map((i) =>
          <DropdownItem
            className="gap-1"
            key={`/favorites/${i.link}`}
            startContent={i.icon}
          >
            {i.name}
          </DropdownItem>)
        }
      </DropdownMenu>
    </Dropdown>);
}
