import { expect, test } from '@playwright/test';

test('test', async ({ page }) => {
  await page.pause();
  await page.goto('http://localhost:3001/');
  await page.getByRole('link', { name: 'Log in' }).click();
  await page.getByRole('link', { name: 'Sign up' }).click();
  await page.getByPlaceholder('name@.com').click();
  await page.getByPlaceholder('name@.com').fill('userr123@gmail.com');
  await page.getByPlaceholder('Your username').click();
  await page.getByPlaceholder('Your username').fill('userr');
  await page.getByLabel('Password', { exact: true }).click();
  await page.getByLabel('Password', { exact: true }).fill('1234567');
  await page.getByLabel('Confirm Password').click();
  await page.getByLabel('Confirm Password').fill('1234567');
  await page.getByRole('button', { name: 'Sign up' }).click();
  await page.getByPlaceholder('name@.com').click();
  await page.getByPlaceholder('name@.com').fill('userr123@gmail.com');
  await page.getByPlaceholder('••••••••').click();
  await page.getByPlaceholder('••••••••').fill('1234567');
  await page.getByRole('button', { name: 'Sign in' }).click();
  await expect(page).toHaveURL('http://localhost:3001/');

  await expect(page.getByRole('button', { name: 'Logout' })).toBeVisible();

});