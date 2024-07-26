"use client";

import { NavbarItem } from "@nextui-org/navbar";
import {  Dropdown, DropdownTrigger, DropdownMenu, DropdownItem } from "@nextui-org/dropdown";
import { Button } from "@nextui-org/button";
import React from "react";
import { useRouter } from "@/navigation";

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
      <NavbarItem>
        <DropdownTrigger>
          <Button
            className="font-bold text-medium w-auto p-0 bg-transparent
            data-[hover=true]:bg-transparen min-w-0 text-foreground"
            radius="sm"
            variant="light"
          >
            {name}
          </Button>
        </DropdownTrigger>
      </NavbarItem>
      <DropdownMenu
        aria-label="favorite selects"
        className="w-auto min-w-0"
        itemClasses={{
          base: "gap-4",
        }}
        onAction={(key) => {
          rounter.push(key as string)
        }}
      >
        {item.map((i) =>
          <DropdownItem key={`/favorites/${i.link}`}
          startContent={i.icon}
        >
          {i.name}
        </DropdownItem>)}
      </DropdownMenu>
    </Dropdown>);
}
