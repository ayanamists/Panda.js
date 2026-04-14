import {notFound} from 'next/navigation';
import {getRequestConfig} from 'next-intl/server';

const locales = ['zh-cn', 'en', 'ja'];

export default getRequestConfig(async ({requestLocale}) => {
  const locale = await requestLocale ?? 'zh-cn';
  if (!locales.includes(locale)) notFound();

  return {
    locale,
    messages: (await import(`../../messages/${locale}.json`)).default
  };
});
