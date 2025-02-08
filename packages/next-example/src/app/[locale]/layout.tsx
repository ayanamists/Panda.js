import type { Metadata } from "next";
import "./globals.css";
import { Providers } from './provider';
import { setRequestLocale } from 'next-intl/server';
import DynamicTopLoader from "@/components/DynamicTopLoader";
import SiteNavbar from "@/components/SiteNavbar";
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
      <body className={`text-foreground bg-background`}>
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
