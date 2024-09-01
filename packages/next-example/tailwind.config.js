/** @type {import('tailwindcss').Config} */
import { nextui } from '@nextui-org/theme';
import typo from "@tailwindcss/typography";
import plugin from 'tailwindcss/plugin';


const solarizedColors = {
  brblack: '#002b36',
  black: '#073642',
  brgreen: '#586e75',
  bryellow: '#657b83',
  brblue: '#839496',
  brcyan: '#93a1a1',
  white: '#eee8d5',
  brwhite: '#fdf6e3',
  yellow: '#b58900',
  brred: '#cb4b16',
  red: '#dc322f',
  magenta: '#d33682',
  brmagenta: '#6c71c4',
  blue: '#268bd2',
  cyan: '#2aa198',
  green: '#859900'
};

const color_primary = solarizedColors.brred;
const color_primary_dark = solarizedColors.magenta;

const englishFont = "Palatino, Georgia, 'Linux Libertine O'";
const chineseMainFont = `'Noto Serif CJK SC', 'Songti SC', 'SimSun'`;
const chineseKaiFont = "'KaiTi', 'STKaiti', 'AR PL UKai CN'";

const chineseHeadingFont = `'Noto Sans CJK', 'Microsoft YaHei', 'PingFang SC'`;
const englishHeadingFont = `Optima, 'Linux Biolinum O', Candara`;

const jpMainFont = `'Noto Serif CJK JP', 'Toppan Bunkyu Midashi Min Std', 'MS Mincho', YuMincho`;
const jpKaiFont = "YuKyokasho, 'UD Digi Kyokasho'";

const mainNode = ["p", "li", "cite"]
const itNode = ["em", "i", "blockQuote .cjk"]

function mkFont(languageName, mainFont, itFont) {
  const res = {}
  for (const mn of mainNode) {
    res[`html[lang="${languageName}"] .prose ${mn}`] = {
      fontFamily: mainFont
    }
  }

  for (const itn of itNode) {
    res[`html[lang="${languageName}"] .prose ${itn}`] = {
      fontFamily: itFont
    }
  }
  return res
}

const chineseFontSetting =
      mkFont("zh-cn", `${englishFont}, ${chineseMainFont}`, `${englishFont}, ${chineseKaiFont}`);
const jpFontSetting =
      mkFont("ja", `${englishFont}, ${jpMainFont}`, `${englishFont}, ${jpKaiFont}`);
const enFontSetting =
      mkFont("en", englishFont, englishFont);

const postFontSetting = {
  ...chineseFontSetting, ...jpFontSetting, ...enFontSetting
}

const monoFont = {
  fontFamily: `Mononoki, 'Mononoki Nerd Font',
FiraCode, 'FiraCode Nerd Font', JetBrainsMono,
'JetBrainsMono Nerd Font', Menlo, Monaco, monospace`
}

const headingFont = `${englishHeadingFont}, ${chineseHeadingFont}`;

const mainFont = {
  fontSize: "18px"
}

const chineseItalic = {
  ".cjk": {
    fontStyle: "normal"
  }
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
    "../../node_modules/@nextui-org/theme/dist/components/(button|dropdown|navbar|card|listbox|image).js"
  ],
  theme: {
    extend: {
      fontFamily: {
        'mainpage': `${englishFont}, ${chineseMainFont}`,
        'mono': monoFont.fontFamily,
        'heading': headingFont
      },
      typography: (theme) => ({
        DEFAULT: {
          css: {
            '--tw-prose-quote-borders': color_primary,
            '--tw-prose-links': color_primary,
            '--tw-prose-body': solarizedColors.black,
            '--tw-prose-headings': solarizedColors.brblack,
            '--tw-prose-quotes': solarizedColors.brblack,
            '--tw-prose-bullets': solarizedColors.brred,
            '--tw-prose-counters': solarizedColors.brred,

            '--tw-prose-invert-quote-borders': color_primary_dark,
            '--tw-prose-invert-links': color_primary_dark,
            '--tw-prose-invert-headings': solarizedColors.brcyan,
            // '--tw-prose-invert-body': solarizedColors.brblue,
            // '--tw-prose-invert-quotes': solarizedColors.brcyan,
            '--tw-prose-invert-bullets': color_primary_dark,
            '--tw-prose-invert-counters': color_primary_dark,

            div: mainFont,
            h2: {
              marginTop: '1em',
              marginBottom: '0.75em',
            },

            p: {
              marginTop: '1em',
              marginBottom: '1em',
              lineHeight: '1.7em',
              ...mainFont
            },

            li: {
              p: {
                marginTop: 0,
                marginBottom: 0,
                lineHeight: '1.25em',
              },
              ...mainFont
            },

            blockQuote: {
              ...mainFont,
              ...chineseItalic
            },

            cite: {
              fontStyle: "normal",
              a: {
                textDecoration: "none"
              }
            },

            i: {
              ...chineseItalic
            },

            em: {
              ...chineseItalic
            },

            code: monoFont,
            kdb: monoFont,
            samp: monoFont
          }
        },
      }),
    }
  },
  plugins: [nextui({
    themes: {
      light: {
        colors: {
          background: solarizedColors.white, // or DEFAULT
          foreground: solarizedColors.black, // or 50 to 900 DEFAULT
          primary: {
            DEFAULT: color_primary,
          },
          lightwhite: solarizedColors.brwhite,
          // ... rest of the colors
        },
      },
      dark: {
        colors: {
          background: solarizedColors.black,
          foreground: solarizedColors.brcyan,
          primary: {
            DEFAULT: color_primary_dark,
          },
          lightblack: solarizedColors.brblack
        },
        // ... rest of the colors
      }
    }
  }), typo(), plugin(function({ addBase, config }) {
      addBase(postFontSetting)
  })],
  darkMode: 'class',
}
