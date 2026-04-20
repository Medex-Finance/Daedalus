import { createRequire } from 'node:module';
import { fileURLToPath } from 'node:url';
import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';

const require = createRequire(import.meta.url);
const getElmExecutable = require('elm-tooling/getExecutable') as (options: {
  name: string;
  version: string;
  cwd?: string;
  env?: Record<string, string | undefined>;
  onProgress: (percentage: number) => void;
}) => Promise<string>;

const backendTarget = process.env.VITE_BACKEND_URL ?? 'http://localhost:4000';
const frontendRoot = fileURLToPath(new URL('.', import.meta.url));

async function resolveElmBinary(): Promise<string> {
  const configuredBinary = process.env.ELM_BINARY?.trim();
  if (configuredBinary) {
    return configuredBinary;
  }

  return await getElmExecutable({
    name: 'elm',
    version: '0.19.1',
    cwd: frontendRoot,
    env: process.env,
    onProgress: () => {},
  });
}

export default defineConfig(async () => {
  const elmBinary = await resolveElmBinary();

  return {
    plugins: [elmPlugin({ nodeElmCompilerOptions: { pathToElm: elmBinary } })],
    base: '/static/',
    server: {
      port: 4173,
      proxy: {
        '/api': {
          target: backendTarget,
          changeOrigin: true,
          secure: false,
        }
      }
    },
    preview: {
      port: 4174,
      proxy: {
        '/api': {
          target: backendTarget,
          changeOrigin: true,
          secure: false,
        }
      }
    }
  };
});
