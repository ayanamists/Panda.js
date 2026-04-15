import type { Metadata } from "next";
import { Crimson_Pro, Jost, JetBrains_Mono } from "next/font/google";
import "./globals.css";
import './fonts.css';
import { Providers } from './provider';
import { setRequestLocale } from 'next-intl/server';
import DynamicTopLoader from "@/components/DynamicTopLoader";
import SiteNavbar from "@/components/SiteNavbar";

const crimsonPro = Crimson_Pro({
  subsets: ['latin', 'latin-ext'],
  style: ['normal', 'italic'],
  variable: '--font-serif-en',
  display: 'swap',
});

const jost = Jost({
  subsets: ['latin', 'latin-ext'],
  style: ['normal', 'italic'],
  variable: '--font-heading-en',
  display: 'swap',
});

const jetbrainsMono = JetBrains_Mono({
  subsets: ['latin'],
  variable: '--font-mono-web',
  display: 'swap',
});

export const metadata: Metadata = {
  title: "Aya's Blog",
  description: "A site powered by next.js and pandoc"
};

export default async function RootLayout(
  props: Readonly<{
    children: React.ReactNode;
    params: Promise<{ locale: string }>;
  }>
) {
  const params = await props.params;

  const {
    children
  } = props;

  setRequestLocale(params.locale);
  return (
    <html lang={params.locale} suppressHydrationWarning>
      <body className={`${crimsonPro.variable} ${jost.variable} ${jetbrainsMono.variable} text-foreground bg-background`}>
        <Providers locale={params.locale}>
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
