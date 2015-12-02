---
title: "Семантический веб"
date: "2012-09-03T12:03:53+03:00"
published: true
tags: "html, microdata, программирование, семантическая разметка"
---

Когда я опубликовал предыдущий пост и решил добавить ссылку на него в Google+, я обратил внимание на то, что Google
выбирает не тот текст со страницы, который я хотел бы видеть. Ну кому интересно каждый раз смотреть на облако тегов?

![](/images/screenshots/semantic-web-1.png "Скриншот с неправильной выборкой")

В итоге я решил разобраться и попытаться указать гуглу (а может и еще кому) нужный кусок. С гуглом получилось,
с другими пока нет. И заодно я поднял целый пласт недостающих знаний по HTML5, а именно
[Microdata](http://www.w3.org/TR/2011/WD-microdata-20110525/). Я, конечно, не раз встречал это слово в спецификации,
но никак не мог сообразить, где именно это может использоваться.

Итак, представляю вашему вниманию HTML Microdata. Эта спецификация отвечает за дополнительную семантическию разметку
страницы. Но для начала разберемся, что такое семантическая разметка. Как подсказывает нам
[википедия](http://ru.wikipedia.org/wiki/%D0%A1%D0%B5%D0%BC%D0%B0%D0%BD%D1%82%D0%B8%D0%BA%D0%B0_(%D0%B7%D0%BD%D0%B0%D1%87%D0%B5%D0%BD%D0%B8%D1%8F)),
семантика\ --- серия научных дисциплин и концепций о смысловой интерпретации в различных символьных системах. Таким
образом, семантическая разметка\ --- это способ придания смысла кускам страницы.

Как вы, должно быть, знаете, в HTML5 появился целый набор специальных тегов для семантичекой разметки. Например,
`header` пригодится, чтобы объявить кусок страницы заголовком, а `article`\ --- статьей, новостью или чем-то
подобным. Но что, если нам нужно точнее указывать значение элементов на странице? Например, вот это\ --- имя автора
статьи, рядом список ключевых слов, а потом еще какая-то информация, которая к статье относится весьма опосредованно.
Зачем? Ну мы-то визуально всё это различим, а вот поисковики и скрипты на всяких сайтах наверняка нет. И вот в этом
случае придется иметь дело с Microdata.

По спецификации у нас есть буквально несколько дополнительных атрибутов для тегов\ --- `itemscope`, `itemtype` и
`itemprop`. Но сами по себе эти атрибуты никакой семантики не добавляют. Создатели спецификации решили, что не дело им
указывать какие типы/значения могут быть у контента. Оно и правильно, пусть другие этим занимаются. Так ведь нашлось
кому заняться: в 2011 году Google, Bing и Yahoo объединились, чтобы создать список типов данных на странице, а еще
через некоторое время к ним присоединился Яндекс. Так что теперь у нас есть [Schema.org](http://schema.org/), прошу
любить и жаловать.

Теперь можно со спокойной душой добавить всякой семантической красоты на свою страницу и наслаждаться жизнью.
Практически весь специализированный вывод на страницах результатов поисковиков получается благодаря этой разметке.
Скажу честно, это не единственный способ, есть еще [microformats](http://microformats.org/), но, по утверждению того же
Google, предпочтительно использовать Microdata, т.к. этот формат гораздо более гибкий.
Кстати, у Google и Яндекса есть инструменты для тестирования подобной разметки:
[раз](http://www.google.com/webmasters/tools/richsnippets) и [два](http://webmaster.yandex.ru/microtest.xml).

Ладно, может, это всё и никому не нужная ерунда, но по крайней мере у меня получилось указать Google+, какая именно
часть страницы является статьей и что нужно показывать.

![](/images/screenshots/semantic-web-2.png "Скриншот с правильной выборкой")

А еще хочется ~~бросить камень в огород~~ сказать пару слов по поводу Facebook. Эти ребята решили выделиться: для
поддержки Facebook нужно использовать свой формат данных\ ---
[Open Graph](http://developers.facebook.com/docs/opengraph/). И использовать его нужно только одним заранее
определенным способом: с помощью добавления тегов `meta` в заголовок страницы. Придется, видимо, подумать, как это
сделать, если я хочу красивое описание у ссылок на мой сайт в Facebook.

Пример использования Microdata можно посмотреть прям тут (Ctrl+U в хроме или Firefox). Вопросы и пожелания\ ---
добро пожаловать в комментарии.