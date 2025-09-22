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

## Status

The codebase is scaffolded with end-to-end architecture for task orchestration, prompt management, worktree control, and preview verification.
