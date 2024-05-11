"use client";

import { Select, SelectItem } from '@nextui-org/select';
import { useLocale } from 'next-intl';
import { useState } from 'react';

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
  const locale = useLocale();
  const [selectKeys, setSelectKeys] = useState(new Set([locale]));

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

  const current = lang.find((item) => item.value === locale);
  
  return (
    <Select
      key={locale}
      label="文/A"
      placeholder={`${current?.name}`}
      className="w-32"
      variant='flat'
      onChange={(e) => {
        const value = e.target.value;
        setSelectKeys(new Set([value]));
        changeLocale(value);
      }}
      selectedKeys={selectKeys}
    >
      {lang.map((item) => (
        <SelectItem key={item.value} value={item.value}>
          {item.name}
        </SelectItem>
      ))}
    </Select>
  );
}