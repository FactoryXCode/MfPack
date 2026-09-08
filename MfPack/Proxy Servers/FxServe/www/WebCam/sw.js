const CACHE_NAME = 'mf-webcam-shell-v2';
const SHELL_FILES = [
  './webcam_stream.html',
  './webcam-manifest.json',
  './webcam-icon-192.png',
  './webcam-icon-512.png'
];

self.addEventListener('install', event => {
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then(cache => cache.addAll(SHELL_FILES))
      .then(() => self.skipWaiting())
  );
});

self.addEventListener('activate', event => {
  event.waitUntil(
    caches.keys()
      .then(names => Promise.all(
        names.filter(name => name !== CACHE_NAME).map(name => caches.delete(name))
      ))
      .then(() => self.clients.claim())
  );
});

function isLiveMedia(url) {
  const name = url.pathname.toLowerCase();
  return name.endsWith('/live.json') ||
         name.endsWith('/viewers.json') ||
         name.endsWith('/init.mp4') ||
         name.endsWith('.m4s');
}

self.addEventListener('fetch', event => {
  if (event.request.method !== 'GET') return;

  const url = new URL(event.request.url);
  if (url.origin !== self.location.origin) return;

  // A PWA must never replay old camera state or media from its offline cache.
  if (isLiveMedia(url)) {
    event.respondWith(fetch(new Request(event.request, { cache: 'no-store' })));
    return;
  }

  if (event.request.mode === 'navigate') {
    event.respondWith(
      fetch(event.request)
        .then(response => {
          const copy = response.clone();
          caches.open(CACHE_NAME).then(cache => cache.put('./webcam_stream.html', copy));
          return response;
        })
        .catch(() => caches.match('./webcam_stream.html'))
    );
    return;
  }

  event.respondWith(
    caches.match(event.request).then(cached => cached || fetch(event.request))
  );
});
