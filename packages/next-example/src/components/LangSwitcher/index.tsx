"use client";

import { Dropdown, DropdownItem, DropdownTrigger, DropdownMenu } from '@nextui-org/dropdown';
import { Button } from '@nextui-org/button';

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
  },
  {
    value: 'zh-cn',
    name: '简体中文',
  },
  {
    value: 'ja',
    name: '日本語',
  }];

  // TODO: aria-labels
  return (
    <Dropdown
    >
      <DropdownTrigger>
        <Button
          isIconOnly
          variant="light"
          className='text-md font-bold'
          aria-label='Select Language'
        >
          文/A
        </Button>
      </DropdownTrigger>

      <DropdownMenu onAction={(key) => {
        changeLocale(key as string);
      }} aria-label='Language Select Menu'>
        {lang.map((item) => (
          <DropdownItem key={item.value} value={item.value} 
           aria-label={`Select Language ${item.name}`}>
            {item.name}
          </DropdownItem>
        ))}
      </DropdownMenu>
    </Dropdown>
  );
}