import { Navbar, NavbarBrand, NavbarContent, NavbarItem } from "@heroui/navbar";
import { useTranslations } from "next-intl";
import ThemeSwitcher from "../ThemeSwitcher";
import LangSwitcher from "../LangSwitcher";
import Garden from "./Garden";
import { HiExternalLink } from "react-icons/hi";
import { IoNewspaper, IoHome, IoBookmarks } from "react-icons/io5";
import NavbarButton from "./NavbarButton";

export default function SiteNavbar() {
  const t = useTranslations('Navbar');
  const tf = useTranslations('Favorites');
  const favouriteData = {
    name: t("favorites"),
    item: [{
      name: tf("mottos"),
      link: "mottos",
      icon: <IoBookmarks />
    }],
  }

  const links = [
    {
      name: t("main"),
      link: "/",
      icon: <IoHome />
    },
    {
      name: t("blog"),
      link: "/posts",
      icon: <IoNewspaper />
    }
  ]
  return (
    <Navbar height={'2.5rem'}>
      <NavbarBrand className="hidden sm:block">
      </NavbarBrand>
      <NavbarContent className="flex flex-row gap-1" justify="center">
        {links.map((i) => <NavbarButton key={i.name} {...i} />)}
        <Garden {...favouriteData} />
        <NavbarButton link={"https://wiki.ayayaya.org"} name={"Wiki"} icon={<HiExternalLink />} />
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
