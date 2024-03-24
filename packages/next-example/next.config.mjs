/** @type {import('next').NextConfig} */
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const nextConfig = {
  pageExtensions: ['js', 'jsx', 'ts', 'tsx', 'md', 'mdx'],
  webpack: (config, options) => {
    config.module.rules.push({
      test: /\.md/,
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

export default nextConfig;
