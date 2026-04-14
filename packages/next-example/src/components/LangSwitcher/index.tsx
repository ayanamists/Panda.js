"use client";

import { Dropdown, DropdownItem, DropdownTrigger, DropdownMenu } from "@heroui/dropdown";
import { IoLanguage } from "react-icons/io5";

const changeLocale = (locale: string) => {
  const currentUrl = window.location.pathname;
  const newUrl = currentUrl.split('/').map((item, index) => {
    if (index === 1) {
      return locale;
    }
    return item;
  }).join('/');
  window.location.href = newUrl;
}

export default function LangSwitcher() {
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
  }];

  return (
    <Dropdown className="bg-background min-w-20 border border-foreground/[0.06] shadow-sm shadow-foreground/[0.04] rounded-md">
      <DropdownTrigger>
        <button
          className="text-foreground/30 hover:text-foreground/60 transition-colors duration-200 p-1"
          aria-label="Select Language"
        >
          <IoLanguage className="w-[14px] h-[14px]" />
        </button>
      </DropdownTrigger>

      <DropdownMenu onAction={(key) => {
        changeLocale(key as string);
      }} aria-label="Language Select Menu"
        itemClasses={{
          base: "gap-2 text-[13px] text-foreground/60 data-[hover=true]:text-foreground data-[hover=true]:bg-foreground/[0.04]",
        }}
      >
        {lang.map((item) => (
          <DropdownItem key={item.value} value={item.value}
            aria-label={`Select Language ${item.name}`}
            startContent={item.icon}
          >
            {item.name}
          </DropdownItem>
        ))}
      </DropdownMenu>
    </Dropdown>
  );
}
