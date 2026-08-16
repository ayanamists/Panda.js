'use client';

import { ThemeProvider as NextThemesProvider } from "next-themes";
import { NextIntlClientProvider } from 'next-intl';


export function Providers({ locale, children }: {
  locale: string,
  children: React.ReactNode,
}) {

  return (
    <NextIntlClientProvider locale={locale}>
      <NextThemesProvider attribute="class" defaultTheme="system">
        {children}
      </NextThemesProvider>
    </NextIntlClientProvider>
  )
}
 