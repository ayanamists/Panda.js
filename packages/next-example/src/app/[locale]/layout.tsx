import type { Metadata } from "next";
import "./globals.css";
import { Providers } from './provider';
import SiteNavbar from "@/components/SiteNavbar";
import { unstable_setRequestLocale } from 'next-intl/server';
import 'react-tooltip/dist/react-tooltip.css';

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
      <body className={`text-foreground bg-background min-h-screen`}>
        <Providers>
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
