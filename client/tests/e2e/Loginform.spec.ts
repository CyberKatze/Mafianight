import { test } from '@playwright/test';

test('test', async ({ page }) => {
  await page.pause();
  await page.goto('http://localhost:3001/');
  await page.getByRole('link', { name: 'Log in' }).click();
  await page.getByRole('link', { name: 'Sign up' }).click();
  await page.getByPlaceholder('name@.com').click();
  await page.getByPlaceholder('name@.com').fill('zara@gmail.com');
  await page.getByPlaceholder('Your username').click();
  await page.getByPlaceholder('Your username').fill('zara');
  await page.getByLabel('Password', { exact: true }).click();
  await page.getByLabel('Password', { exact: true }).fill('qwerty');
  await page.getByLabel('Confirm Password').click();
  await page.getByLabel('Confirm Password').fill('qwerty');
  await page.getByRole('button', { name: 'Sign up' }).click();
});
