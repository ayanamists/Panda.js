"use client";

import { Button } from "@nextui-org/button";
import { DropdownTrigger }  from "@nextui-org/dropdown";
import { NavbarItem } from "@nextui-org/navbar";
import { usePathname, Link } from "@/navigation";

interface NavbarButtonProps {
  name: string;
  dropdown?: boolean;
  link: string;
  icon?: React.ReactNode
}

function guard<a>(cond: boolean, value: a) {
  return cond ? value : undefined
}

export default function NavbarButton({ name, dropdown = false, link, icon }: NavbarButtonProps) {
  const path = usePathname();
  const highlight = (link === '/' && path === '/') ||
                    (link !== '/' && path.includes(link.substring(1)));
  const button = (
    <Button
      className={
      `text-sm w-auto p-0 bg-transparent
      min-w-14
      gap-0.5
      ${highlight ? "text-primary" : "text-foreground"}
      `}
      radius="sm"
      variant="light"
      startContent={icon}
      aria-label={`Goto ${name}`}
      href={guard(!dropdown, link)}
      as={guard(!dropdown, Link)}
    >
      {name}
    </Button>
  );

  return dropdown ? (
    <NavbarItem>
      <DropdownTrigger>
        {button}
      </DropdownTrigger>
    </NavbarItem>
  ) : (
    <NavbarItem>
      {button}
    </NavbarItem>
  );
}
