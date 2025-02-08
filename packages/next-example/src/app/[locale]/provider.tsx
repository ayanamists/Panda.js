'use client';

import { HeroUIProvider } from "@heroui/system"
import { ThemeProvider as NextThemesProvider } from "next-themes";
import { NextIntlClientProvider } from 'next-intl';


export function Providers({ locale, children }: {
  locale: string,
  children: React.ReactNode,
}) {

  return (
    <NextIntlClientProvider locale={locale}>
      <HeroUIProvider>
        <NextThemesProvider attribute="class" defaultTheme="system">
          {children}
        </NextThemesProvider>
      </HeroUIProvider>
    </NextIntlClientProvider>
  )
}
