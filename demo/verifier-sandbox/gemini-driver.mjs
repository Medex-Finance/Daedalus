#!/usr/bin/env node
import fs from 'node:fs';
import path from 'node:path';

const __dirname = path.dirname(new URL(import.meta.url).pathname);

const BASE_URL =
  process.env.VERIFIER_AUTOMATION_URL ||
  `file://${path.join(__dirname, 'demo.html')}`;

const MAX_STEPS = Number(process.env.VERIFIER_MAX_STEPS || 5);
const VIEWPORT_WIDTH = Number(process.env.VERIFIER_VIEWPORT_WIDTH || 640);
const VIEWPORT_HEIGHT = Number(process.env.VERIFIER_VIEWPORT_HEIGHT || 360);

const REPORT_PATH = path.join(__dirname, 'automation-report.json');
const LOG_PATH = path.join(__dirname, 'automation-log.json');
const CURRENT_SCREENSHOT = path.join(__dirname, 'current.png');

const encodeScreenshot = (shotPath) => {
  const data = fs.readFileSync(shotPath);
  return Buffer.from(data).toString('base64');
};

const gatherClickables = async (page) => {
  const clickables = await page.$$eval('a, button, [role="button"]', (nodes) =>
    nodes.map((el) => ({
      label: el.innerText?.trim() || el.getAttribute('aria-label') || el.textContent?.trim() || 'Unnamed element',
      selector: el.outerHTML.slice(0, 80).replace(/\s+/g, ' '),
    })),
  );
  return clickables.slice(0, 10);
};

const gatherCompletionState = async (page) => {
  try {
    const result = await page.$eval('#completion-status', (el) => ({
      completed: el.dataset.complete === 'true',
      message: el.textContent?.trim() || '',
    }));
    return result;
  } catch (err) {
    return { completed: false, message: 'no status element' };
  }
};

const buildGeminiPrompt = (clickables, history, completion) => {
  const instructions = [
    'You are Gemini QA. Decide the next UI action to reach completion. Respond with JSON:',
    '{"action":"click"|"stop","selector":"playwright selector or null","notes":["..."],"issues":["..."]}',
    '',
    `Completion state: ${completion.completed ? 'completed' : 'incomplete'} (${completion.message || 'no status text'})`,
    '',
    'Clickable elements:',
    clickables.map((el, idx) => `${idx + 1}. ${el.label} -> ${el.selector}`).join('\n'),
    '',
    'Previous actions:',
    history.map((step, idx) => `${idx + 1}. ${step.action} (${step.selector || 'n/a'})`).join('\n') || '(none)',
    '',
    'Choose a click to continue toward completion, or stop when done or blocked. Include any issues.',
  ];
  return instructions.join('\n');
};

const callGemini = async ({ prompt, screenshotPath }) => {
  if (process.env.GEMINI_FAKE_MODE === '1') {
    return { action: 'stop', summary: 'Fake Gemini pass verdict.', issues: [], notes: [] };
  }
  const apiKey = process.env.GEMINI_API_KEY;
  if (!apiKey) throw new Error('GEMINI_API_KEY not set');
  const model = process.env.GEMINI_MODEL || 'gemini-1.5-pro';
  const body = {
    contents: [
      {
        parts: [
          { text: prompt },
          {
            inline_data: {
              mime_type: 'image/png',
              data: encodeScreenshot(screenshotPath),
            },
          },
        ],
      },
    ],
  };
  const response = await fetch(
    `https://generativelanguage.googleapis.com/v1beta/models/${model}:generateContent?key=${apiKey}`,
    {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
    },
  );
  if (!response.ok) {
    throw new Error(`Gemini action request failed: ${response.statusText}`);
  }
  const data = await response.json();
  const text = data?.candidates?.[0]?.content?.parts?.[0]?.text;
  if (!text) throw new Error('Gemini response missing text');
  const cleaned = text.trim().replace(/^```(?:json)?/i, '').replace(/```$/, '').trim();
  const jsonSnippet = cleaned.includes('{') ? cleaned.slice(cleaned.indexOf('{')) : cleaned;
  return JSON.parse(jsonSnippet);
};

const run = async () => {
  const { chromium } = await import('playwright');
  const executablePath =
    process.env.PLAYWRIGHT_LAUNCH_OPTIONS_EXECUTABLE_PATH ||
    process.env.CHROMIUM_PATH ||
    undefined;
  const browser = await chromium.launch({ headless: true, executablePath });
  const page = await browser.newPage({
    baseURL: BASE_URL,
    viewport: { width: VIEWPORT_WIDTH, height: VIEWPORT_HEIGHT },
  });
  const steps = [];
  const issues = [];
  let summary = 'All criteria satisfied.';
  let lastShot = '';
  try {
    await page.goto(BASE_URL);
    for (let i = 0; i < MAX_STEPS; i++) {
      const shotPath = path.join(__dirname, `automation-step-${i}.png`);
      await page.screenshot({ path: shotPath, fullPage: false });
      lastShot = shotPath;
      const completion = await gatherCompletionState(page);
      const clickables = await gatherClickables(page);
      const prompt = buildGeminiPrompt(clickables, steps, completion);
      const command = await callGemini({ prompt, screenshotPath: shotPath });
      steps.push({ step: i + 1, ...command });
      if (command.action === 'click' && command.selector) {
        await page.click(command.selector);
      }
      if (command.issues && command.issues.length > 0) {
        issues.push(...command.issues);
      }
      if (command.summary) {
        summary = command.summary;
      }
      const refreshed = await gatherCompletionState(page);
      if (command.action === 'stop' || (refreshed.completed && issues.length === 0)) {
        break;
      }
    }
    if (lastShot) fs.copyFileSync(lastShot, CURRENT_SCREENSHOT);
  } finally {
    await browser.close();
  }
  const verdict = {
    passed: issues.length === 0,
    summary,
    issues,
    steps,
  };
  fs.writeFileSync(REPORT_PATH, JSON.stringify(verdict, null, 2));
  fs.writeFileSync(LOG_PATH, JSON.stringify(steps, null, 2));
};

try {
  await run();
} catch (err) {
  console.error('[gemini-driver] failed:', err.message);
  process.exit(1);
}
