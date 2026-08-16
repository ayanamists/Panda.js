import { expect, test, type Page } from '@playwright/test';

async function expectNoNextError(page: Page) {
  await expect(page.locator('#__next_error__')).toHaveCount(0);
  await expect(page.getByText('This page could not be found.')).toHaveCount(0);
  await expect(page.getByText('Application error')).toHaveCount(0);
}

async function opacity(locator: ReturnType<Page['locator']>) {
  return locator.evaluate((el) => getComputedStyle(el).opacity);
}

test('GET / is an HTTP redirect, not a JS NEXT_REDIRECT shell', async ({ page, request }) => {
  const response = await request.get('/', { maxRedirects: 0 });
  expect(response.status(), 'static export must not serve the __next_error__ 200').toBe(302);
  expect(response.headers()['location']).toBe('/zh-cn');

  await page.goto('/');
  await expect(page).toHaveURL(/\/zh-cn\/?$/);
  await expectNoNextError(page);
  await expect(page.getByLabel('李晨曦')).toBeVisible();
});

test('homepage name is visible on first paint (no fade-from-zero LCP)', async ({ page }) => {
  await page.goto('/zh-cn', { waitUntil: 'domcontentloaded' });
  const name = page.getByLabel('李晨曦');
  await expect(name).toBeVisible();
  expect(await opacity(page.locator('h1'))).toBe('1');
  await expectNoNextError(page);
});

test('archive lists posts and opens an article', async ({ page }) => {
  await page.goto('/zh-cn/posts');
  await expect(page.getByRole('heading', { name: 'Archive' })).toBeVisible();
  await expectNoNextError(page);

  await page.getByRole('link', { name: /十分钟搞懂 Hindley-Milner/ }).click();
  await expect(page).toHaveURL(/\/zh-cn\/posts\/hindley-milner\/?$/);
  await expect(page.locator('h1')).toContainText('Hindley-Milner');
});

test('article body is in the first document and not opacity 0', async ({ page }) => {
  const pageErrors: string[] = [];
  page.on('pageerror', (error) => pageErrors.push(error.message));

  await page.goto('/zh-cn/posts/hindley-milner', { waitUntil: 'domcontentloaded' });
  await expect(page.locator('h1')).toContainText('十分钟搞懂 Hindley-Milner 类型系统');
  await expect(page.locator('article')).toBeVisible();
  await expect(page.locator('article')).toContainText('类型推理');
  expect(await opacity(page.locator('article'))).toBe('1');

  const fadedAncestor = await page.locator('article').evaluate((el) => {
    for (let node: HTMLElement | null = el; node; node = node.parentElement) {
      if (getComputedStyle(node).opacity === '0') return node.tagName;
    }
    return null;
  });
  expect(fadedAncestor).toBeNull();
  await expectNoNextError(page);
  expect(pageErrors, pageErrors.join('\n')).toEqual([]);
});

test('language switcher reaches the Japanese article', async ({ page }) => {
  await page.goto('/zh-cn/posts/hindley-milner');
  await page.getByLabel('Select Language', { exact: true }).click();
  await page.getByRole('link', { name: '日本語' }).click();
  await expect(page).toHaveURL(/\/ja\/posts\/hindley-milner\/?$/);
  await expect(page.locator('h1')).toContainText('Hindley-Milner');
  await expect(page.locator('article')).toBeVisible();
  await expectNoNextError(page);
});

test('garden menu opens mottos', async ({ page }) => {
  await page.goto('/zh-cn');
  await page.getByText('园地', { exact: true }).click();
  await page.getByRole('link', { name: '格言' }).click();
  await expect(page).toHaveURL(/\/zh-cn\/favorites\/mottos\/?$/);
  await expect(page.locator('h1')).toContainText('格言');
  await expect(page.getByText(/Edsger W\.Dijkstra/)).toBeVisible();
  await expectNoNextError(page);
});
