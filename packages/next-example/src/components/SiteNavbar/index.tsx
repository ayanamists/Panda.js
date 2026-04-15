import { useTranslations } from "next-intl";
import ThemeSwitcher from "../ThemeSwitcher";
import LangSwitcher from "../LangSwitcher";
import Garden from "./Garden";
import NavbarButton from "./NavbarButton";
import Monogram from "./Monogram";

function NavSep() {
  return <span className="text-[10px] text-foreground/15 select-none" aria-hidden="true">·</span>;
}

export default function SiteNavbar() {
  const t = useTranslations('Navbar');
  const tf = useTranslations('Favorites');
  const favouriteData = {
    name: t("favorites"),
    item: [{
      name: tf("mottos"),
      link: "mottos",
    }],
  }

  return (
    <nav className="sticky top-0 z-50 bg-background/85 backdrop-blur-md">
      <div className="max-w-3xl mx-auto h-11 px-6 flex items-center justify-between">

        {/* Left: Monogram + navigation links */}
        <div className="flex items-center">
          <Monogram />
          <div className="ml-5 flex items-center gap-3">
            <NavbarButton name={t("main")} link="/" />
            <NavSep />
            <NavbarButton name={t("blog")} link="/posts" />
            <NavSep />
            <Garden {...favouriteData} />
            <NavSep />
            <NavbarButton name="Wiki" link="https://wiki.ayayaya.org" />
          </div>
        </div>

        {/* Right: Utilities */}
        <div className="flex items-center gap-1.5">
          <LangSwitcher />
          <ThemeSwitcher />
        </div>

      </div>
      {/* Tapered rule — fades at edges like a typographic ornament */}
      <div className="h-px max-w-3xl mx-auto bg-gradient-to-r from-transparent via-foreground/[0.08] to-transparent" />
    </nav>
  );
}
