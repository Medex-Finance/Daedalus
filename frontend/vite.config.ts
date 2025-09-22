import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';

const backendTarget = process.env.VITE_BACKEND_URL ?? 'http://localhost:4000';

export default defineConfig({
  plugins: [elmPlugin({ pathToElm: 'elm' })],
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
});
