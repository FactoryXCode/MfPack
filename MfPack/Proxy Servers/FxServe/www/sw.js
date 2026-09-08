const CACHE_NAME = "factoryx-radio-pwa-v1";
const APP_SHELL = [
  "/",
  "/index.html",
  "/manifest.json",
  "/artwork/hero.jpg",
  "/artwork/cover_default.jpg",
  "/icons/icon-192.png",
  "/icons/icon-512.png"
];

self.addEventListener("install", function (event) {
  event.waitUntil(
    caches.open(CACHE_NAME).then(function (cache) {
      return cache.addAll(APP_SHELL);
    }).then(function () {
      return self.skipWaiting();
    })
  );
});

self.addEventListener("activate", function (event) {
  event.waitUntil(
    caches.keys().then(function (keys) {
      return Promise.all(keys.map(function (key) {
        if (key !== CACHE_NAME) {
          return caches.delete(key);
        }
      }));
    }).then(function () {
      return self.clients.claim();
    })
  );
});

function isLiveStreamRequest(url) {
  return url.pathname.indexOf("/stream/") === 0 ||
         url.pathname === "/nowplaying.json" ||
         url.pathname.endsWith("/live.json") ||
         url.pathname.endsWith(".m4s") ||
         url.pathname.endsWith(".mp4");
}

function networkFirst(request) {
  return fetch(request).then(function (response) {
    const copy = response.clone();
    caches.open(CACHE_NAME).then(function (cache) {
      cache.put(request, copy);
    });
    return response;
  }).catch(function () {
    return caches.match(request).then(function (cached) {
      return cached || caches.match("/index.html");
    });
  });
}

function cacheFirst(request) {
  return caches.match(request).then(function (cached) {
    if (cached) {
      return cached;
    }

    return fetch(request).then(function (response) {
      const copy = response.clone();
      caches.open(CACHE_NAME).then(function (cache) {
        cache.put(request, copy);
      });
      return response;
    });
  });
}

self.addEventListener("fetch", function (event) {
  if (event.request.method !== "GET") {
    return;
  }

  const url = new URL(event.request.url);
  if (url.origin !== self.location.origin) {
    return;
  }

  if (isLiveStreamRequest(url)) {
    event.respondWith(fetch(event.request));
    return;
  }

  if (event.request.mode === "navigate") {
    event.respondWith(networkFirst(event.request));
    return;
  }

  if (url.pathname.indexOf("/artwork/") === 0 ||
      url.pathname.indexOf("/icons/") === 0 ||
      url.pathname === "/manifest.json") {
    event.respondWith(cacheFirst(event.request));
    return;
  }

  event.respondWith(networkFirst(event.request));
});