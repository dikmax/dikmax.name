---
title: "Планировщик маршрутов v2.0"
date: "2014-04-07T11:40:00+03:00"
published: true
tags: "блог, путешествие, планировщик маршрута, сервис"
thread: 2592689919
---

Я\ послушал ваши отзывы и\ комментарии по поводу [моего небольшого проекта][route-planner]. Реализовал некоторые
из\ высказанных идей и, конечно, добавил к\ ним часть своих.

1. Самое главное, самое сложное и\ самое незаметное: поменялся алгоритм поиска. Вместо точного и\ медленного [метода
   ветвей и\ границ][branch-and-bounds] теперь почти точный и\ метаэвристичекий [метод муравьиной колонии][ant-colony]. В\ основе проекта
   лежит [Travelling Salesman Problem][tsp] (Задача коммивояжёра), которая является [NP-полной][np]. А\ это означает,
   что нет другого пути решения её, кроме как перебрать все варианты. Количество вариантов можно посчитать по\ формуле
   $n! \over 2$, где $n$\ --- количество промежуточных городов. Если подставить в\ эту формулу, например, 15\ получим
   653\ 837\ 184\ 000 вариантов, а\ это уже очень много. Поэтому пришлось воспользоваться приближенным алгоритмом,
   который работает быстро, но\ и\ решение выдаёт не\ всегда оптимальное. Кстати, так как внутри алгоритма задействован
   генератор случайных чисел, решение может меняться от\ запуска к\ запуску, достаточно нажать кнопку обновить.

2. Я\ убрал из\ проекта [AngularDart], что позволило уменьшить размер JavaScript-файла в\ 3\ раза.

3. Теперь можно скопировать ссылку с\ маршрутом и\ отправить кому-нибудь. Только осторожно, чем больше городов
   будет в\ маршруте, тем длиннее будет ссылка и\ некоторые браузеры могут не\ справиться с\ такой длиной. Вот вам,
   например, ссылка на\ [маршрут по\ всем европейским столицам][europe].

4. Упростилось добавление городов в\ маршрут. Из\ вариантов объектов теперь убираются лишние, если вариант только один,
   то\ он\ добавляется автоматически. И\ самое главное: можно добавлять города, даже не\ прикасаясь к\ мышке, используя
   для выбора варианта клавиши “Вверх”, “Вниз” и\ “Enter”.

5. А\ ещё теперь можно развернуть карту на\ весь экран, чтобы было удобнее её\ изучать.

6. Исправлены ошибки.

[![Маршрут по Европе](/images/screenshots/route-planner-europe.png "Маршрут по Европе")][europe]

Прошу любить и жаловать. Замеченные недочёты можно описывать в\ комментариях к\ этому посту, ну\ или присылать любым
другим способом.

[AngularDart]: https://angulardart.org/
[ant-colony]: http://ru.wikipedia.org/wiki/%D0%9C%D1%83%D1%80%D0%B0%D0%B2%D1%8C%D0%B8%D0%BD%D1%8B%D0%B9_%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC
[branch-and-bounds]: http://ru.wikipedia.org/wiki/%D0%9C%D0%B5%D1%82%D0%BE%D0%B4_%D0%B2%D0%B5%D1%82%D0%B2%D0%B5%D0%B9_%D0%B8_%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86
[europe]: /route-planner/#QlpoOTFBWSZTWQ+3zPEAAdUb+hAHf+AACgIAFgR/TfSd8gB/f+AAUAMm1SKJvIBqYEmTIp5RsgTamk0yAAaBsp6g1M0m0KSgAAAAAAAAIp4aAlUA09RoaAaGJowIaMgcwAEwABMAAAAABoVPKekHqBiAaAPSAA0NGRpW2JSQ6kgSlAVFdyBlCpGwcgam5EcYuc4AQuKEALYCEB2QHz9CunO6eCBaIZszslYtc96MdjokpQ5CpdXIwauqgMGNkp0mTKWDmiCgjh6WGJQ3cPz4k4pPQanfcNZlPKmRHG3ZXfAZ4VzbTTMQskMd9VNIJzdwNpULiSG2FjDY6VQ0CYNyoDBtPuMWMbTOgzu4NvZ1ytZyUVfFjAWrba2sqeQ6B41iZFvKOTAbLbLhMuYS3F5KimlyeL1LGx5hVFRejxG1sIzHbYIpGWKpWnUsZFwzScwXAIpZK3jCdmHZ94x1WZEAFKKypZacJtOGHHBJw6O9nUPxEaWG0jOG6vjasxcND7G5q9F7A4h3TnTaxEOIDmrId0NDVyeLo7BLW0yA8GBmMrXNVrGowuSejLCTNbVgoUyXD2EdpRMsY2Gfdl6MVyaIbAyWry1RtYi9EWOT01tdaVzGRsJZcO6fdRctiyYnNwnVqyGRIwMqJkUQXZ4EuKKzKWbk0Oc3AFHejgtZbNhaxBZwHGtIhqejNYgloNDyTUVqJRZiBd3ZVIFEcP4/Ju3b27U4dAKqaQVL1VAUDqhkyshK1x9baapD8N6fk/WRcoaP5rdV8uYsSyotjKTUEzkUFI2H4zBdFAIEBQRmACBFGQKMwCMwCH7dA2Ew+cCA32cnlSJ6kSok+OdaJ3meY44/fz00/TIO3wSxdMDfq1IxPTjYFHezpyrs54nsPpwcTvW3C1uRcJrZCdH1/3TgH5vQn2SvLPPLTt6eEZY095yDWuVDDm29eaPInDEpdyKROoES27cK9+W/h1cd+3bR2bZh2ZHldY2I045s1CErGBpHdaLs8o16SOyfqn3azOPW/S/WWO+/UL8ikWO4biAy3BY5xJ1/4u5IpwoSAfb5niA=
[np]: http://ru.wikipedia.org/wiki/NP-%D0%BF%D0%BE%D0%BB%D0%BD%D0%B0%D1%8F_%D0%B7%D0%B0%D0%B4%D0%B0%D1%87%D0%B0
[route-planner]: /route-planner/
[tsp]: http://ru.wikipedia.org/wiki/%D0%97%D0%B0%D0%B4%D0%B0%D1%87%D0%B0_%D0%BA%D0%BE%D0%BC%D0%BC%D0%B8%D0%B2%D0%BE%D1%8F%D0%B6%D1%91%D1%80%D0%B0
