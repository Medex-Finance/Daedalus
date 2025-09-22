import './index.css';
import { Elm } from './Main.elm';

type OpenStreamPayload = {
  taskId: number;
  path: string;
};

type ElmApp = {
  ports?: {
    openStatusStream?: { subscribe: (handler: (payload: OpenStreamPayload) => void) => void };
    closeStatusStream?: { subscribe: (handler: (taskId: number) => void) => void };
    receiveStatusEvent?: { send: (payload: unknown) => void };
  };
};

const backendBaseRaw = (import.meta.env.VITE_BACKEND_URL as string | undefined)?.trim() ?? '';
const backendBase = backendBaseRaw === '' ? '' : backendBaseRaw.replace(/\/+$/, '');

const app = (Elm.Main as any).init({
  node: document.getElementById('root'),
  flags: { backendBase }
}) as ElmApp;

const streams = new Map<number, EventSource>();

const resolveUrl = (path: string) => {
  if (backendBase === '') {
    return path;
  }
  if (path.startsWith('/')) {
    return `${backendBase}${path}`;
  }
  return `${backendBase}/${path}`;
};

const openStream = ({ taskId, path }: OpenStreamPayload) => {
  if (streams.has(taskId)) {
    return;
  }
  const url = resolveUrl(path);
  const source = new EventSource(url, { withCredentials: false });
  source.onmessage = (event) => {
    try {
      const data = JSON.parse(event.data);
      app.ports?.receiveStatusEvent?.send({ taskId, event: data });
    } catch (error) {
      console.warn('Failed to parse status event', error);
    }
  };
  source.onerror = () => {
    source.close();
    streams.delete(taskId);
  };
  streams.set(taskId, source);
};

const closeStream = (taskId: number) => {
  const existing = streams.get(taskId);
  if (existing) {
    existing.close();
    streams.delete(taskId);
  }
};

app.ports?.openStatusStream?.subscribe(openStream);
app.ports?.closeStatusStream?.subscribe(closeStream);
