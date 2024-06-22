/** @type {import('next').NextConfig} */
import path from 'path';
import { fileURLToPath } from 'url';

import createNextIntlPlugin from 'next-intl/plugin';
 
const withNextIntl = createNextIntlPlugin();

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const nextConfig = {
  output: 'export',
  pageExtensions: ['js', 'jsx', 'ts', 'tsx', 'md', 'mdx'
                  , 'org'],
  webpack: (config, options) => {
    config.module.rules.push({
      test: /\.(md|org)$/,
      use: [
        options.defaultLoaders.babel,
        {
          loader: path.resolve(__dirname, 'loader/panda.mjs'),
        },
      ],
    })
    return config
  },
  // there's a bug in swc minifier
  // see: https://github.com/swc-project/swc/issues/8931
  swcMinify: false
};

export default withNextIntl(nextConfig);
