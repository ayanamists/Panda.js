/** @type {import('next').NextConfig} */
import path from 'path';
import { fileURLToPath } from 'url';

import createNextIntlPlugin from 'next-intl/plugin';
import createNextBundleAnalyzer from '@next/bundle-analyzer'
 
const withNextIntl = createNextIntlPlugin();

const withBundleAnalyzer = createNextBundleAnalyzer({
  enabled: process.env.ANALYZE === 'true',
})

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
};

export default withBundleAnalyzer(withNextIntl(nextConfig));
