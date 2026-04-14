/** @type {import('tailwindcss').Config} */
import { heroui } from "@heroui/theme";
import typo from "@tailwindcss/typography";

/*
 * "Manuscript" palette
 * Light — warm ivory paper, graphite ink, sage-green accent
 * Dark  — deep charcoal, pearl-gray text, muted teal accent
 */
const manuscript = {
  // Light mode
  paper:      '#f5f3ee',   // warm ivory background
  paperLight: '#faf9f6',   // elevated surface
  ink:        '#2b2d30',   // graphite foreground
  inkDeep:    '#1a1c1e',   // headings
  inkMuted:   '#6b6d70',   // secondary text

  // Dark mode
  slate:      '#1a1c20',   // deep charcoal background
  slateLight: '#22252a',   // elevated surface
  pearl:      '#c4c1b8',   // warm pearl foreground
  pearlBright:'#e0ded8',   // headings
  pearlMuted: '#7a7870',   // secondary text

  // Accents
  sage:       '#5b7f6a',   // primary (light)
  teal:       '#6aab8e',   // primary (dark)

  // Semantic
  red:        '#b85450',
  green:      '#6a8f5b',
  blue:       '#5879a5',
  amber:      '#a68a4c',
  violet:     '#7c6b9e',
};

const color_primary = manuscript.sage;
const color_primary_dark = manuscript.teal;

const englishFont = "Palatino, Georgia, 'Linux Libertine O'";
const chineseMainFont = `'Noto Serif CJK SC', 'Songti SC', 'SimSun'`;
const chineseKaiFont = "'KaiTi', 'STKaiti', 'AR PL UKai CN'";

const chineseHeadingFont = `'Noto Sans CJK', 'Microsoft YaHei', 'PingFang SC'`;
const englishHeadingFont = `Optima, 'Linux Biolinum O', Candara`;

const jpMainFont = `'Noto Serif CJK JP', 'Toppan Bunkyu Midashi Min Std', 'MS Mincho', YuMincho`;
const jpKaiFont = "YuKyokasho, 'UD Digi Kyokasho'";

const monoFont = {
  fontFamily: `Mononoki, 'Mononoki Nerd Font',
FiraCode, 'FiraCode Nerd Font', JetBrainsMono,
'JetBrainsMono Nerd Font', Menlo, Monaco, monospace`
}

const headingFont = `${englishHeadingFont}, ${chineseHeadingFont}`;

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
    "../../node_modules/@heroui/theme/dist/components/(button|dropdown|navbar|card|listbox|image).js"
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
            '--tw-prose-body': manuscript.ink,
            '--tw-prose-headings': manuscript.inkDeep,
            '--tw-prose-quotes': manuscript.inkDeep,
            '--tw-prose-bullets': color_primary,
            '--tw-prose-counters': color_primary,

            '--tw-prose-invert-quote-borders': color_primary_dark,
            '--tw-prose-invert-links': color_primary_dark,
            '--tw-prose-invert-headings': manuscript.pearlBright,
            '--tw-prose-invert-bullets': color_primary_dark,
            '--tw-prose-invert-counters': color_primary_dark,

            div: mainFont,
            h2: {
              marginTop: '1em',
              marginBottom: '0.75em',
            },

            p: {
              fontFamily: 'var(--prose-body-font)',
              marginTop: '1em',
              marginBottom: '1em',
              lineHeight: '1.7em',
              ...mainFont
            },

            li: {
              fontFamily: 'var(--prose-body-font)',
              p: {
                marginTop: 0,
                marginBottom: 0,
                lineHeight: '1.25em',
              },
              ...mainFont
            },

            blockQuote: {
              fontFamily: 'var(--prose-body-font)',
              ...mainFont,
              '.cjk': { fontStyle: 'normal' }
            },

            cite: {
              fontFamily: 'var(--prose-body-font)',
              fontStyle: "normal",
              a: {
                textDecoration: "none"
              }
            },

            i: {
              fontFamily: 'var(--prose-italic-font)',
              '.cjk': { fontStyle: 'normal' }
            },

            em: {
              fontFamily: 'var(--prose-italic-font)',
              '.cjk': { fontStyle: 'normal' }
            },

            code: monoFont,
            kdb: monoFont,
            samp: monoFont
          }
        },
      }),
    }
  },
  plugins: [heroui({
    themes: {
      light: {
        colors: {
          background: manuscript.paper,
          foreground: manuscript.ink,
          primary: {
            DEFAULT: color_primary,
          },
          lightwhite: manuscript.paperLight,
        },
      },
      dark: {
        colors: {
          background: manuscript.slate,
          foreground: manuscript.pearl,
          primary: {
            DEFAULT: color_primary_dark,
          },
          lightblack: '#141618'
        },
      }
    }
  }), typo()],
  darkMode: 'class',
}
