---
title: "Настройка клиента last.fm, продолжение"
date: "2012-08-01T18:44:36+03:00"
published: true
tags: "last.fm, linux, инструкция, музыка"
thread: 788509903
---

[Предыдущая статья о настройке lastfm](/post/lastfmclient/), вызвала широкий резонанс в виде просьбы
[Виктора](http://profiles.google.com/6alliapumob) сделать ему шелл-скрипт. Ну что-же, я не смог не внять этому крику
души и написал некоторое его подобие. Конечно, запускать его нужно с root-правами, например,
вот так: `sudo ./lastfm.sh`.

~~~~~bash
#!/bin/sh
apt-get install -y lastfm

cd /usr/lib/lastfm
perl -pe 's/&api_key/&\x{00}pi_key/g' < libLastFmTools.so.1.0.0 > libLastFmTools.so.1.0.0.patched
mv -f libLastFmTools.so.1.0.0.patched libLastFmTools.so.1.0.0
~~~~~

Проверено на моем компе, по идее должно работать на любом Debian-производном дистрибутиве. Замечания, предложения,
благодарности\ --- в комментарии.
