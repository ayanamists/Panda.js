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
  paper:      '#f5f3ee',
  paperLight: '#faf9f6',
  ink:        '#2b2d30',
  inkDeep:    '#1a1c1e',
  inkMuted:   '#6b6d70',

  // Dark mode
  slate:      '#1a1c20',
  slateLight: '#22252a',
  pearl:      '#c4c1b8',
  pearlBright:'#e0ded8',
  pearlMuted: '#7a7870',

  // Accents
  sage:       '#5b7f6a',
  teal:       '#6aab8e',

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

const chineseHeadingFont = `'Noto Sans CJK', 'Microsoft YaHei', 'PingFang SC'`;
const englishHeadingFont = `Optima, 'Linux Biolinum O', Candara`;

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
    "./src/**/*.{js,ts,jsx,tsx,mdx}",
    "../../node_modules/@heroui/theme/dist/**/*.{js,mjs}"
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
