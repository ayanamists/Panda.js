"use client";

import { Button } from "@nextui-org/button";
import { DropdownTrigger }  from "@nextui-org/dropdown";
import { NavbarItem } from "@nextui-org/navbar";
import { useRouter, usePathname } from "@/navigation";

interface NavbarButtonProps {
  name: string;
  dropdown?: boolean;
  link: string;
  icon?: React.ReactNode
}

export default function NavbarButton({ name, dropdown = false, link, icon }: NavbarButtonProps) {
  const router = useRouter();
  const onPress = () => router.push(link ?? "");
  const path = usePathname();
  const highlight = (link === '/' && path === '/') ||
                    (link !== '/' && path.includes(link.substring(1)));
  const button = (
    <Button
      className={
      `text-medium w-auto p-0 bg-transparent
      min-w-14
      gap-0.5
      ${highlight ? "text-primary" : "text-foreground"}
      `}
      radius="sm"
      variant="light"
      onPress={link ? onPress : undefined}
      startContent={icon}
      aria-label={`Goto ${name}`}
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
