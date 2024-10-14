import type { Metadata } from "next";
import "./globals.css";
import { Providers } from './provider';
import { unstable_setRequestLocale } from 'next-intl/server';
import dynamic from 'next/dynamic';

import SiteNavbar from "@/components/SiteNavbar";
const DynamicTopLoader = dynamic(() => import("@/components/TopLoader"), {
  ssr: false
});

export const metadata: Metadata = {
  title: "Aya's Blog",
  description: "A site powered by next.js and pandoc"
};

export default function RootLayout({
  children,
  params
}: Readonly<{
  children: React.ReactNode;
  params: { locale: string };
}>) {
  unstable_setRequestLocale(params.locale);
  return (
    <html lang={params.locale} suppressHydrationWarning>
      <body className={`text-foreground bg-background`}>
        <Providers>
          <DynamicTopLoader />
          <SiteNavbar />
          {children}
        </Providers>
      </body>
    </html>
  );
}

export async function generateStaticParams() {
  const langs = ['zh-cn', 'en', 'ja'];
  return langs.map(lang => ({ locale: lang }));
}
