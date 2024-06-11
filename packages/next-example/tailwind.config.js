/** @type {import('tailwindcss').Config} */
import { nextui } from '@nextui-org/react';
import typo from "@tailwindcss/typography";

const color_primary = "#ec4899";
const color_primary_dark = "#d946ef";
const englishFont = "'Optima', 'Linux Biolinum O'";
const chineseMainFont = "'Noto Serif CJK SC', 'Songti SC', 'SimSun'";
const chineseKaiFont = "'KaiTi', 'STKaiti', 'AR PL UKai CN'";

const headingFont = {
  fontFamily: `${englishFont}, ${chineseKaiFont}`
};

const mainFont = {
  fontSize: "18px"
}

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
            fontFamily: `${englishFont}, ${chineseMainFont}`,
            '--tw-prose-quote-borders': color_primary,
            '--tw-prose-invert-quote-borders': color_primary_dark,
            '--tw-prose-links': color_primary,
            '--tw-prose-invert-links': color_primary_dark,
            h1: headingFont,
            h2: headingFont,
            h3: headingFont,
            h4: headingFont,
            h5: headingFont,
            h6: headingFont,

            p: {
              marginTop: '1em',
              marginBottom: '1em',
              ...mainFont
            },

            li: {
              p: {
                marginTop: 0,
                marginBottom: 0
              },
              ...mainFont
            },

            blockQuote: {
              ...mainFont
            },

            cite: {
              fontStyle: "normal",
              a: {
                textDecoration: "none"
              }
            }
          }
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

