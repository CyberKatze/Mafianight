import { defineConfig, devices } from '@playwright/test';
import path from 'path';


export default defineConfig({
  /* Global setup and teardown */
  globalSetup: path.resolve('./tests/global-setup.ts'),
  globalTeardown: path.resolve('./tests/global-teardown.ts'),
  /* Run tests in files in parallel */
  fullyParallel: true,
  /* Fail the build on CI if you accidentally left test.only in the source code. */
  forbidOnly: !!process.env.CI,
  /* Retry on CI only */
  retries: process.env.CI ? 2 : 0,
  /* Opt out of parallel tests on CI. */
  workers: process.env.CI ? 1 : undefined,
  /* Reporter to use. See https://playwright.dev/docs/test-reporters */
  reporter: 'html',
  /* Shared settings for all the projects below. See https://playwright.dev/docs/api/class-testoptions. */
  use: {
    /* Base URL to use in actions like `await page.goto('/')`. */
    // baseURL: 'http://127.0.0.1:3000',

    /* Collect trace when retrying the failed test. See https://playwright.dev/docs/trace-viewer */
    trace: 'on-first-retry',
  },

  /* Configure projects for major browsers */
  projects: [
    {
      name: 'unit',
      testDir: 'tests/unit',
      use: { ...devices['Desktop Chrome'] },
    },
    {
      name: 'e2e',
      testDir: 'tests/e2e',
      use: { ...devices['Desktop Chrome'] },
    },
  ],


});
