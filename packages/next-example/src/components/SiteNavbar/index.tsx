import
{ Navbar, NavbarBrand, NavbarContent, NavbarItem
} from "@nextui-org/navbar";
import { useTranslations } from "next-intl";
import { Link as LLink } from "@/navigation";
import ThemeSwitcher from "../ThemeSwitcher";
import LangSwitcher from "../LangSwitcher";
import Garden from "./Garden";
import { BsChatLeftQuote } from "react-icons/bs";

export default function SiteNavbar() {
  const t = useTranslations('Navbar');
  const tf = useTranslations('Favorites');
  const favouriteData = {
    name: t("favorites"),
    item: [{
      name: tf("mottos"),
      link: "mottos",
      icon: <BsChatLeftQuote />
    }]
  }
  return (
    <Navbar shouldHideOnScroll>
      <NavbarContent className="flex flex-row gap-4" justify="center">
        <NavbarItem className="font-bold">
          <LLink href="/" aria-label={t("main")}>
            {t("main")}
          </LLink>
        </NavbarItem>

        <NavbarItem>
          <LLink href="/blog/" className="font-bold" aria-label={t("blog")}>
            {t("blog")}
          </LLink>
        </NavbarItem>

        <Garden {...favouriteData} />

      </NavbarContent>

      <NavbarContent justify="end" className="gap-0">

        <NavbarItem>
          <LangSwitcher />
        </NavbarItem>

        <NavbarItem>
          <ThemeSwitcher />
        </NavbarItem>

      </NavbarContent>
    </Navbar>);
}
