# AI Project Manager Platform

This repository provides a fault-tolerant orchestration layer that coordinates multiple Codex agents to deliver end-to-end feature work. It couples a Yesod/SQLite backend with a TypeScript frontend, provides customizable prompts, and manages git worktrees, tests, QA, and preview deployments.

## Getting Started

1. Install [Nix](https://nixos.org) with flakes enabled.
2. Run `nix develop` to enter the dev shell (includes GHC, cabal, pnpm, HLS).
3. In one terminal: `cd backend && cabal run backend` to start the API (defaults to port 4000).
4. (Optional) Seed demo data with `cabal run seed`.
5. Generate shared TypeScript definitions with `../scripts/generate-types.sh` from the repo root.
6. (Optional) Override the backend URL by setting `VITE_BACKEND_URL` in `frontend/.env.local` (defaults to `http://localhost:4000` in dev via Vite proxy).
7. In another terminal: `cd frontend && pnpm install && pnpm dev` to start the UI (Vite proxies `/api` to the backend port).
8. For a bundled frontend served directly by the backend, run `pnpm build` (outputs to `frontend/dist`, which the backend serves under `/` and `/static/*`).
9. Visit `http://localhost:4173` to access the dashboard during dev, or `http://localhost:4000` when using the backend-served bundle.

## Project Layout

- `backend/` — Yesod application, orchestration workflow, database models.
- `frontend/` — React + Vite SPA showing tasks, timelines, settings.
- `scripts/` — helper scripts for type generation, agent execution, testing.
- `migrations/` — Persistent migrations and seed data.
- `assets/` — Static assets consumed by frontend/backend.

## Type Sharing

Run `./scripts/generate-types.sh` from the repo root to regenerate TypeScript declarations using `aeson-typescript` and backend `App.Types` definitions.

## Development Notes

- Orchestrator queue runs in-process; stuck tasks can receive human nudges via the task detail page.
- Preview links default to localhost ports; pinging records health artifacts.
- Configure the preview launch command in the Settings page (e.g. `pnpm dev -- --port $PORT`). The orchestrator starts it inside the task worktree and tears it down when the task is merged or discarded.
- Successful runs automatically commit to `task/<id>` branches and push to `origin`; failures block the workflow and surface details in the task timeline.
- QA verdicts drive automatic fix iterations when issues are reported, so the implementation agent reruns with the recorded findings as context.
- Agent runs look for a `codex` executable (override with `CODEX_CLI`); if it's missing they fall back to `npx @openai/codex` so having `npm` on the PATH avoids manual setup.
- The frontend is now implemented in Elm. `pnpm install` pulls `elm-tooling`, and `pnpm build`/`pnpm dev` automatically compile Elm via Vite. If the Elm binary is missing run `pnpm elm-tooling install`.
- Prompt templates are editable in-app and rewritable to defaults with one click.
- `cabal run seed` provides three example tasks for UI testing.
- Agent execution uses the Codex CLI (`codex exec --full-auto`). Install `@openai/codex`, run `codex login --api-key <OPENAI_KEY>`, and ensure the `codex` binary is on `PATH` (override with `CODEX_CLI` if needed). Codex writes transcripts into task artifacts so you can review each step.
- The task timeline streams Codex stdout/stderr in real time via SSE; the UI auto-refreshes status, artifacts, and logs without manual reloads.
- The `StepSpecVerification` phase now runs a dedicated verifier agent. Before the agent is invoked the backend captures UI evidence from `demo/verifier-sandbox` (or a project specific harness), stores a screenshot + diff summary artifact, and feeds it into the verifier prompt. Run `demo/verifier-sandbox/capture.sh` to regenerate screenshots (uses Playwright when available and falls back to an in-repo PNG generator so CI still passes).
- Tasks have a first-class Acceptance Criteria checklist. Capture the requirements in the task sidebar, sync them to the verifier prompt automatically, and mark each line as satisfied once the evidence proves it.
- The capture pipeline is overridable: set `VERIFIER_CAPTURE_COMMAND` to point at your own screenshot command, `VERIFIER_SANDBOX_ROOT` to point at the directory containing `verifier-sandbox` (defaults to this repo’s `demo/verifier-sandbox`), or `VERIFIER_DISABLE_PLAYWRIGHT=1` to force the fallback PNG generator when Playwright dependencies are unavailable.
- Spec verification leans on a Playwright + Gemini automation harness. `demo/verifier-sandbox/automation.mjs` boots Chromium against `VERIFIER_AUTOMATION_URL` (defaults to the bundled `demo.html`), lets Gemini reason about each screenshot to decide the next click, and emits `automation-report.json`, `automation-log.json`, and the final screenshot (`current.png`). Configure `VERIFIER_MAX_STEPS` to control how many decisions Gemini is allowed to make per run.
- The backend stores `automation-report.json` as an `ArtifactVerifierReport` and only falls back to the Gemini screenshot reviewer when no automation verdict is available. Configure `GEMINI_API_KEY` (and optionally `GEMINI_MODEL`, e.g. `gemini-1.5-pro`) so both the sandbox driver and the fallback reviewer can call the Generative Language API. For smoke tests or offline dev you can set `GEMINI_FAKE_MODE=1` to short-circuit both callers while keeping the workflow intact.
- A background repo-sync monitor now keeps the default repo (and every active task branch) aligned with `origin/<baseBranch>`. Every `REPO_SYNC_INTERVAL_SECONDS` (default 60s) it fetches `origin`, merges the latest base branch into each `task/<id>` branch, reruns the configured test command, and pushes the branch when tests pass. Failures automatically schedule a fix iteration with the failing test log attached, so implementers can pick up regressions caused by upstream changes immediately. Set `REPO_SYNC_INTERVAL_SECONDS` to tune the cadence.
- Repository-specific wiring lives in `config/repo-profiles.json`. Each profile can inject environment variables (e.g. `VERIFIER_AUTOMATION_URL`), override the verifier sandbox path/command, and run custom setup commands before evidence capture. The default profile targets `/nvme/medex` but reuses this repo’s bundled sandbox (`demo/verifier-sandbox`) to capture evidence; it launches the Medex Coding Copilot demo via `./scripts/run_coding_copilot_demo.sh start` and points the Playwright verifier at `http://127.0.0.1:5173/app/billing/modern`. Extend the JSON file (or set `REPO_PROFILES_PATH`) to onboard additional repos without code changes—only the preview URL and setup command need to differ.
  - Repo profile env is passed through verbatim, so leave `GEMINI_FAKE_MODE` unset (or set to `0`) to get real Gemini verdicts, or set it to `1` in the profile/env when you need offline runs.

## Repo Profiles

- **Location:** `config/repo-profiles.json` (override with `REPO_PROFILES_PATH`).
- **Fields:**
  - `match`: canonical path (or parent directory) to match against `settingsDefaultRepoRoot`.
  - `setup`: commands that must succeed before spec verification runs (designed for launching preview stacks). Commands run from `workingDir` (default: repo root) with optional `timeoutSeconds`.
  - `sandbox` / `captureCommand`: overrides for the screenshot harness.
  - `env`: key/value pairs injected into the capture pipeline (e.g. `VERIFIER_AUTOMATION_URL`).
- The bundled Medex profile launches `scripts/launch_copilot_stack.sh` (a thin wrapper around `scripts/run_coding_copilot_demo.sh`) so the backend, worker, and frontend are live before the Playwright automation explores the UI. Stop the stack with `scripts/launch_copilot_stack.sh stop` when you’re done, or add your own repo profile pointing to a different script.

## Status

The codebase is scaffolded with end-to-end architecture for task orchestration, prompt management, worktree control, and preview verification.
