import {createNavigation} from 'next-intl/navigation';
import {defineRouting} from 'next-intl/routing';

export const routing = defineRouting({
  locales: [ 'zh-cn', 'en', 'ja' ],
  defaultLocale: 'zh-cn',
  localePrefix: {
    mode: 'always',
  },
  pathnames: {
    '/': '/',
  }
});
 
export const locales = [ 'zh-cn', 'en', 'ja' ] as const;
export const localePrefix = 'always'; // Default
 
export const {Link, redirect, usePathname, useRouter} =
  createNavigation(routing);
