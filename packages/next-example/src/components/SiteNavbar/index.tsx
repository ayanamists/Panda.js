import { useTranslations } from "next-intl";
import ThemeSwitcher from "../ThemeSwitcher";
import LangSwitcher from "../LangSwitcher";
import Garden from "./Garden";
import NavbarButton from "./NavbarButton";

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
    <nav className="sticky top-0 z-50 bg-background/80 backdrop-blur-sm border-b border-foreground/[0.06]">
      <div className="max-w-3xl mx-auto h-10 px-6 flex items-center justify-between">

        {/* Navigation links */}
        <div className="flex items-center gap-5">
          <NavbarButton name={t("main")} link="/" />
          <NavbarButton name={t("blog")} link="/posts" />
          <Garden {...favouriteData} />
          <NavbarButton name="Wiki" link="https://wiki.ayayaya.org" />
        </div>

        {/* Utilities */}
        <div className="flex items-center gap-2">
          <LangSwitcher />
          <ThemeSwitcher />
        </div>

      </div>
    </nav>
  );
}
