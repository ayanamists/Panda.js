/** @type {import('tailwindcss').Config} */
import { nextui } from '@nextui-org/react';
import typo from "@tailwindcss/typography";

const color_primary = "#ec4899";
const color_primary_dark = "#d946ef";

// eslint-disable-next-line import/no-anonymous-default-export
export default {
  content: [
    "./app/**/*.{js,ts,jsx,tsx,mdx}",
    "./pages/**/*.{js,ts,jsx,tsx,mdx}",
    "./components/**/*.{js,ts,jsx,tsx,mdx}",

    // Or if using `src` directory:
    "./src/**/*.{js,ts,jsx,tsx,mdx}",
    // Note: This is a monorepo, so the path need to point to the root directory
    "../../node_modules/@nextui-org/theme/dist/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      typography: (theme) => ({
        DEFAULT: {
          css: {
            fontFamily: ['Optima', 'Linux Biolinum O'],
            '--tw-prose-quote-borders': color_primary,
            '--tw-prose-invert-quote-borders': color_primary_dark
          },
        },
      }),
    }
  },
  plugins: [nextui({
    themes: {
      light: {
        colors: {
          background: "#FFFFFF", // or DEFAULT
          foreground: "#11181C", // or 50 to 900 DEFAULT
          primary: {
            //... 50 to 900
            foreground: "#FFFFFF",
            DEFAULT: color_primary,
          },
          // ... rest of the colors
        },
      },
      dark: {
        colors: {
          background: "#000000", // or DEFAULT
          foreground: "#ECEDEE", // or 50 to 900 DEFAULT
          primary: {
            //... 50 to 900
            foreground: "#FFFFFF",
            DEFAULT: color_primary_dark,
          },
        },
        // ... rest of the colors
      }
    }
  }), typo()],
  darkMode: 'class',
}

