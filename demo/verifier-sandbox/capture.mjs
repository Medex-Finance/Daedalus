#!/usr/bin/env node
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { chromium } from 'playwright';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const args = process.argv.slice(2);
const outputIndex = args.indexOf('--output');
const output =
  outputIndex !== -1 && args[outputIndex + 1]
    ? args[outputIndex + 1]
    : 'current.png';

const absoluteOutput = path.isAbsolute(output)
  ? output
  : path.join(__dirname, output);

const htmlPath = path.join(__dirname, 'demo.html');
const url = `file://${htmlPath}`;

const viewport = { width: 1280, height: 720 };

async function main() {
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage({ viewport });
  await page.goto(url);
  await page.waitForTimeout(500);
  await page.screenshot({ path: absoluteOutput, fullPage: true });
  await browser.close();
  // eslint-disable-next-line no-console
  console.log(`Captured screenshot to ${absoluteOutput}`);
}

main().catch((err) => {
  console.error('[capture] Failed to produce screenshot:', err);
  process.exit(1);
});
