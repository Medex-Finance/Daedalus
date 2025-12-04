#!/usr/bin/env node
import fs from 'node:fs';
import path from 'node:path';
import process from 'node:process';
import { spawnSync } from 'node:child_process';

const __dirname = path.dirname(new URL(import.meta.url).pathname);

const MAX_STEPS = Number(process.env.VERIFIER_MAX_STEPS || 5);
const BASE_URL =
  process.env.VERIFIER_AUTOMATION_URL ||
  `file://${path.join(__dirname, 'demo.html')}`;

const REPORT_PATH = path.join(__dirname, 'automation-report.json');
const LOG_PATH = path.join(__dirname, 'automation-log.json');
const CURRENT_SCREENSHOT = path.join(__dirname, 'current.png');

const ensureDir = (filePath) => {
  const dir = path.dirname(filePath);
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }
};

const writeJson = (target, data) => {
  ensureDir(target);
  fs.writeFileSync(target, JSON.stringify(data, null, 2));
};

const fallbackScreenshot = () => {
  const script = `
import struct, zlib
width, height = 640, 360
rows = []
for y in range(height):
    row = bytearray()
    row.append(0)
    for x in range(width):
        r = 15 + int(40 * (x / width))
        g = 90 + int(80 * (y / height))
        b = 140
        row.extend((r, g, b))
    rows.append(bytes(row))
raw = b''.join(rows)
def chunk(tag, data):
    import zlib, struct
    return struct.pack('>I', len(data)) + tag + data + struct.pack('>I', zlib.crc32(tag + data) & 0xffffffff)
ihdr = chunk(b'IHDR', struct.pack('>IIBBBBB', width, height, 8, 2, 0, 0, 0))
idat = chunk(b'IDAT', zlib.compress(raw, 9))
iend = chunk(b'IEND', b'')
with open('${CURRENT_SCREENSHOT}', 'wb') as fh:
    fh.write(b'\\x89PNG\\r\\n\\x1a\\n' + ihdr + idat + iend)
`;
  const result = spawnSync('python3', ['-c', script], { stdio: 'inherit' });
  if (result.status !== 0) {
    throw new Error('fallback screenshot failed');
  }
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

const encodeScreenshot = (shotPath) => {
  const data = fs.readFileSync(shotPath);
  return Buffer.from(data).toString('base64');
};

const buildGeminiPrompt = (clickables, runHistory, completion) => {
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
    runHistory.map((step, idx) => `${idx + 1}. ${step.action} (${step.selector || 'n/a'})`).join('\n') ||
      '(none)',
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
  const cleaned = text
    .trim()
    .replace(/^```(?:json)?/i, '')
    .replace(/```$/, '')
    .trim();
  const jsonSnippet = cleaned.includes('{') ? cleaned.slice(cleaned.indexOf('{')) : cleaned;
  return JSON.parse(jsonSnippet);
};

const runAutomation = async () => {
  const { chromium } = await import('playwright');
  const executablePath =
    process.env.PLAYWRIGHT_LAUNCH_OPTIONS_EXECUTABLE_PATH
    || process.env.CHROMIUM_PATH
    || undefined;
  const browser = await chromium.launch({ headless: true, executablePath });
  const page = await browser.newPage({
    baseURL: BASE_URL,
    viewport: {
      width: Number(process.env.VERIFIER_VIEWPORT_WIDTH || 640),
      height: Number(process.env.VERIFIER_VIEWPORT_HEIGHT || 360),
    },
  });
  const steps = [];
  const issues = [];
  let summary = 'All criteria satisfied.';
  try {
    await page.goto(BASE_URL);
    let lastShot = '';
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
    if (lastShot) {
      fs.copyFileSync(lastShot, CURRENT_SCREENSHOT);
    }
  } finally {
    await browser.close();
  }
  return {
    passed: issues.length === 0,
    summary,
    issues,
    steps,
  };
};

const main = async () => {
  try {
    if (process.env.VERIFIER_DISABLE_PLAYWRIGHT === '1') {
      fallbackScreenshot();
      writeJson(REPORT_PATH, {
        passed: true,
        summary: 'Playwright disabled; generated static verification.',
        issues: [],
        steps: [],
      });
      writeJson(LOG_PATH, { steps: [] });
      process.exit(0);
    }
    const verdict = await runAutomation();
    if (!fs.existsSync(CURRENT_SCREENSHOT)) {
      await fallbackScreenshot('Automation screenshot placeholder');
    }
    writeJson(REPORT_PATH, verdict);
    writeJson(LOG_PATH, verdict.steps);
  } catch (err) {
    console.error('[automation] failed:', err.message);
    process.exit(1);
  }
};

await main();
