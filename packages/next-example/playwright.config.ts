import fs from 'node:fs';
import { defineConfig, devices } from '@playwright/test';

const port = Number(process.env.PORT || 4173);
const host = process.env.HOST || '127.0.0.1';
const baseURL = `http://${host}:${port}`;

function systemChromePath(): string | undefined {
  if (process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH) {
    return process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH;
  }
  // Playwright's downloaded Chromium is a generic Linux binary and will not
  // start on NixOS (no stub ld). Prefer a packaged Chrome/Chromium.
  const home = process.env.HOME ?? '';
  const candidates = [
    '/run/current-system/sw/bin/google-chrome-stable',
    '/run/current-system/sw/bin/google-chrome',
    '/run/current-system/sw/bin/chromium',
    `${home}/.nix-profile/bin/google-chrome-stable`,
    `${home}/.nix-profile/bin/google-chrome`,
    `${home}/.nix-profile/bin/chromium`,
  ];
  return candidates.find((file) => fs.existsSync(file));
}

const executablePath = systemChromePath();

export default defineConfig({
  testDir: './tests/e2e',
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  reporter: process.env.CI ? 'github' : 'list',
  use: {
    baseURL,
    trace: 'on-first-retry',
    launchOptions: executablePath
      ? { executablePath, args: ['--no-sandbox'] }
      : undefined,
  },
  webServer: {
    command: 'node ./scripts/serve-export.mjs',
    url: `${baseURL}/zh-cn`,
    reuseExistingServer: !process.env.CI,
    timeout: 30_000,
  },
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
  ],
});
