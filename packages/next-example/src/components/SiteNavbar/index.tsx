import { Navbar, NavbarBrand, NavbarContent, NavbarItem, NavbarMenuToggle, NavbarMenu, NavbarMenuItem } from "@nextui-org/navbar";
import { useTranslations } from "next-intl";
import { Link as LLink } from "@/navigation";
import ThemeSwitcher from "../ThemeSwitcher";
import LangSwitcher from "../LangSwitcher";

export default function SiteNavbar() {
  const t = useTranslations('Navbar');
  return (
    <Navbar shouldHideOnScroll>
      <NavbarBrand className="flex flex-row gap-4">
        <p className="font-bold text-inherit">
          <LLink href="/">
            {t("main")}
          </LLink>
        </p>

        <NavbarItem>
          <LLink href="/blog/" className="font-bold">
            {t("blog")}
          </LLink>
        </NavbarItem>
      </NavbarBrand>

      <NavbarContent justify="end">

        <NavbarItem>
          <LangSwitcher />
        </NavbarItem>

        <NavbarItem>
          <ThemeSwitcher />
        </NavbarItem>

      </NavbarContent>
    </Navbar>);
}