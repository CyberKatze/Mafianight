
import { expect, test } from '@playwright/test';

test('User sign-up and login', async ({ page }) => {
  await test.step('Sign-up', async () => {
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
    await expect(page).toHaveURL('http://localhost:3001/login');
  });

  await test.step('Login', async () => {
    await page.goto('http://localhost:3001/');
    await page.getByRole('link', { name: 'Log in' }).click();
    await page.getByPlaceholder('name@.com').click();
    await page.getByPlaceholder('name@.com').fill('userr123@gmail.com');
    await page.getByLabel('Password', { exact: true }).click();
    await page.getByLabel('Password', { exact: true }).fill('1234567');
    await page.getByRole('button', { name: 'Sign in' }).click();

    await expect(page.locator('text=Log out')).toBeVisible();
  });
});
