## Verifier Sandbox

This sample directory acts as a fixture for the automated verifier. During the `StepSpecVerification` phase the backend runs `capture.sh`, which first executes the Playwright + Gemini automation harness to explore the UI, produce a final screenshot, and dump an `automation-report.json`. The backend copies `current.png` into the active worktree, compares it against `reference.png`, and stores both the diff summary and the automation verdict as artifacts. When Playwright or Chromium are unavailable the capture script falls back to a deterministic PNG generator so automated tests still work in minimal environments.

Files:

- `automation.mjs` – launches Chromium, points it at `VERIFIER_AUTOMATION_URL` (defaults to `demo.html`), and repeatedly captures screenshots. Each screenshot is fed to Gemini, which responds with the next click or reported issues.
- `automation-report.json` / `automation-log.json` – structured verdict + step-by-step log emitted by `automation.mjs`. The backend stores the report verbatim as an `ArtifactVerifierReport`.
- `demo.html` / `styles.css` – the surface rendered in the screenshot.
- `capture.sh` – regenerates screenshots. It installs `playwright` locally the first time it runs.
- `reference.png` – expected UI snapshot checked in for reproducibility.
- `current.png` – ignored by git; regenerated for each verifier run.

Usage:

```
cd demo/verifier-sandbox
./capture.sh            # writes current.png via Playwright when possible
./capture.sh --output reference.png   # refreshes the baseline
```

If the host is missing GUI libraries you can force the fallback renderer with `VERIFIER_DISABLE_PLAYWRIGHT=1 ./capture.sh`.

Keep both images the same dimensions so the lightweight pixel diff can quantify changes. In a real project this directory would be replaced with a scripted capture pipeline (e.g. Playwright against your preview deployment) that dumps evidence for the verifier agent.

## Automation workflow

- `automation.mjs` honors `VERIFIER_AUTOMATION_URL` (defaults to the local `demo.html`) and `VERIFIER_MAX_STEPS` (defaults to `3`). Each step captures a PNG, asks Gemini what to do next, and either performs a click or reports issues.
- Set `GEMINI_API_KEY` and optionally `GEMINI_MODEL` (default `gemini-1.5-pro`) so the harness can call the Generative Language API. Use `GEMINI_FAKE_MODE=1` to short-circuit the request while still producing deterministic JSON for tests.
- When `VERIFIER_DISABLE_PLAYWRIGHT=1` is set the automation harness skips launching Chromium, writes a static screenshot, and emits a passing fake verdict. This keeps CI deterministic while still touching every code path.
- You can override the entire capture pipeline with `VERIFIER_CAPTURE_COMMAND` (runs inside this directory) or point the backend at a different sandbox via `VERIFIER_SANDBOX_ROOT`.
