---
title: "Network.HTTPS"
date: "2012-06-10T18:14:37+03:00"
published: true
tags: "haskell, программирование"
thread: 720123261
---

Обнаружил, что нативная реализация http-протокола для Haskell (`Network.HTTP`) не поддерживает https. Придется, видимо,
использовать совсем не функциональные биндинги для libcurl (`Network.Curl`).