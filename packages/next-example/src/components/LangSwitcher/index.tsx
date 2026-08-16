"use client";

import { IoLanguage } from "react-icons/io5";
import { Link, usePathname } from "@/navigation";

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
    <details className="nav-details relative">
      <summary
        className="cursor-pointer p-1 text-foreground/30 transition-colors duration-200 hover:text-foreground/60"
        aria-label="Select Language"
      >
        <IoLanguage className="w-[14px] h-[14px]" />
      </summary>
      <div className="absolute right-0 z-50 mt-2 min-w-28 rounded-md border border-foreground/[0.06] bg-background py-1 shadow-sm shadow-foreground/[0.04]">
        {lang.map((item) => (
          <Link
            key={item.value}
            href={pathname}
            locale={item.value}
            className="flex items-center gap-2 px-3 py-1.5 text-[13px] text-foreground/60 hover:bg-foreground/[0.04] hover:text-foreground"
            aria-label={`Select Language ${item.name}`}
          >
            <span>{item.icon}</span>
            <span>{item.name}</span>
          </Link>
        ))}
      </div>
    </details>
  );
}
