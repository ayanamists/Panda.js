"use client";

import { Dropdown, DropdownItem, DropdownTrigger, DropdownMenu } from '@nextui-org/dropdown';
import { Button } from '@nextui-org/button';
import { IoLanguage } from "react-icons/io5";
/**
 * Change the locale of the page
 * @param locale 
 * 
 * TODO: dirty hack here. any better way to change locale?
 */
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

  // TODO: aria-labels
  return (
    <Dropdown className="text-foreground bg-background min-w-16"
    >
      <DropdownTrigger>
        <Button
          isIconOnly
          variant="light"
          className='text-large font-bold text-foreground'
          aria-label='Select Language'
        >
          <IoLanguage />
        </Button>
      </DropdownTrigger>

      <DropdownMenu onAction={(key) => {
        changeLocale(key as string);
      }} aria-label='Language Select Menu'>
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
