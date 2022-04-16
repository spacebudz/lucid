if (typeof window === 'undefined') {
  const fetch = await import('node-fetch');
  // @ts-ignore
  global.fetch = fetch.default;
  // @ts-ignore
  global.Headers = fetch.Headers;
  // @ts-ignore
  global.Request = fetch.Request;
  // @ts-ignore
  global.Response = fetch.Response;
}

export * from './types';
export * from './utils';
export * from './core';
