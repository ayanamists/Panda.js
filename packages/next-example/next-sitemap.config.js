/** @type {import('next-sitemap').IConfig} */
const config = {
  siteUrl: process.env.SITE_URL || 'https://ayayaya.org',
  generateRobotsTxt: true, // (optional)
  output: "export",
  outDir: "out"
}

export default config;
