"use client";

import { IoLanguage } from "react-icons/io5";
import { Link, usePathname } from "@/navigation";
import NavMenu from "@/components/SiteNavbar/NavMenu";

const lang = [{
  value: 'en',
  name: 'English',
  icon: "🇬🇧"
},
{
  value: 'zh-cn',
  name: '简体中文',
  icon: "🇨🇳"
},
{
  value: 'ja',
  name: '日本語',
  icon: "🇯🇵"
}] as const;

export default function LangSwitcher() {
  const pathname = usePathname();

  return (
    <NavMenu
      label="Select Language"
      align="right"
      trigger={
        <span className="block p-1 text-foreground/30 transition-colors duration-200 hover:text-foreground/60">
          <IoLanguage className="w-[14px] h-[14px]" />
        </span>
      }
    >
      {lang.map((item) => (
        <Link
          key={item.value}
          role="menuitem"
          href={pathname}
          locale={item.value}
          className="flex items-center gap-2 px-3 py-1.5 text-[13px] text-foreground/60 hover:bg-foreground/[0.04] hover:text-foreground"
          aria-label={`Select Language ${item.name}`}
        >
          <span>{item.icon}</span>
          <span>{item.name}</span>
        </Link>
      ))}
    </NavMenu>
  );
}
