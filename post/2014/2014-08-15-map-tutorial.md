---
title: "Как сделать свою карту"
date: "2014-08-15T17:55:00+03:00"
published: true
tags: "d3, javascript, svg, карта, программирование"
thread: 2927760853
---

![](/images/map-tutorial/cover.png)

Сегодня я\ расскажу вам, как с\ помощью JavaScript и\ [d3] нарисовать карту, [подобную моей][map].

<!--more-->

# Утилиты (Prerequisites)

Для начала работы нам понадобятся две утилиты `ogr2ogr` и\ `topojson`. `ogr2ogr` входит в\ состав [GDAL]\ --- библиотеки
и\ набора утилит для работы с\ гео-данными. `topojson`\ --- небольшая утилита, написанная на\ JavaScript
и\ работающая на\ [node.js].

Установить GDAL (и, соответственно, `ogr2ogr`) на\ Mac OS можно, например, через MacPorts:

~~~~~sh
sudo port install gdal
~~~~~

Под Linux тоже есть соответствующий пакет:

~~~~~sh
sudo apt-get install gdal
~~~~~

Ребятам с\ Windows я\ не\ помогу. Где-то на\ сайте проекта должен быть нужный архив. В\ комментарии приглашаются
знающие люди.

Вторая утилита ставится через npm (не забудьте сначала установить node.js):

~~~~~sh
sudo npm install -g topojson
~~~~~

Всё, теперь можно приступать к\ работе.

# Данные

Вся суть любой карты\ --- это данные. Не\ будет данных, нечего будет рисовать. Я\ рекомендую данные [Natural
Earth][earth] из-за их\ доступности и\ открытости. Итак, идём в\ [раздел 1:10m, Cultural][earth-cultural]
и\ скачиваем карту с\ делением по\ странам ([первый раздел, Admin\ 0\ --- Countries, любая
ссылка][countries-download-link]).

Скачали, распаковали. Теперь с\ этим добром нужно что-то делать, данные-то в\ формате [Shapefile]. Для
преобразования данных в\ читаемый вид нам и\ понадобится `ogr2ogr`\ --- утилита, которая преобразует данные из\ одного
векторного формата в\ другой. У\ неё огромное количество различных параметров, но\ я\ облегчу вам задачу.

~~~~~sh
ogr2ogr -f GeoJSON countries.json ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp
~~~~~

После вызова этой команды должен появиться файл `countries.json`, содержащий те\ же данные, но\ в\ формате [GeoJSON].
Размер файла, конечно, не\ маленький\ --- 24\ Мб. Но\ не\ стоит отчаиваться: для работы понадобится его более
оптимизированный собрат\ --- [TopoJSON].

~~~~~sh
topojson -o world.json countries.json
~~~~~

В\ результате имеем файл размером 2.3\ Мб, с\ ним и\ будем работать. Этот размер можно ещё значительно уменьшить,
но\ об\ этом я\ расскажу чуть позже.

# Первая картинка

Начнём с\ создания новой страницы:

~~~~~html
<!doctype html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Map Tutorial 01</title>
    <link rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/normalize/3.0.1/normalize.min.css"/>
    <script src="http://d3js.org/d3.v3.min.js"></script>
    <script src="http://d3js.org/topojson.v1.min.js"></script>
</head>
<body>
<script>
    // Здесь будет код
</script>
</body>
</html>
~~~~~

Теперь можно открывать всё это в\ браузере. Подойдёт любой статический сервер. Например, в\ PhpStorm, который
я\ использую, можно просто нажать на\ файл правой клавишей и\ выбрать "Открыть в\ браузере"\ --- выбранный файл
отобразится с\ помощью небольшого встроенного в\ IDE сервера.

Создаём элемент `<svg>` размером с\ окно, в\ котором и\ будет происходить вся отрисовка. Для этого заменяем комментарий
на\ подходящий код:

~~~~~javascript
var width = window.innerWidth;
var height = window.innerHeight;
var svg = d3.select("body").append("svg")
        .attr("width", width)
        .attr("height", height);
~~~~~

Эта инструкция создаёт html-элемент `<svg>`, добавляет его к\ `<body>` и\ прописывает подходящие размеры
в\ атрибуты `width` и `height`.

Пришла пора загрузить данные. Дописываем в конец:

~~~~~javascript
d3.json("world.json", function (error, world) {
    if (error) {
        console.log(error);
        return;
    }

    // Тут будет отображение
});
~~~~~

Я\ думаю, тут не\ надо ничего объяснять.

Теперь можно отобразить данные!

~~~~~javascript
svg.append("path")
        .datum(topojson.feature(world, world.objects.countries))
        .attr("d", d3.geo.path().projection(d3.geo.mercator()));
~~~~~

Красота!

![[Результат][result01], [Исходники][source01]](/images/map-tutorial/map-01.png)

На\ самом деле это я\ быстро набросал, чтобы было что показать. Теперь будем постепенно менять, приводя к\ виду, который
на\ самом деле хочется получить.

*После каждого логического куска поста есть две ссылки: на\ результат, который должен был получиться, и\ на\ написанный
мной код. Первые две можете наблюдать под картинкой чуть выше. Если вдруг что-то не\ получается, всегда можно
подсмотреть или даже скопировать мою версию.*

# Проекция

Пока не\ начинаешь рисовать карту, даже не\ задумываешься, какое огромное количество проекций придумали люди, чтобы
покрасивее отобразить объёмную землю на\ плоской бумаге. [Меркатор][merkator], [Winkel Tripel][winkel], [Aitoff],
[Dymaxion], [проекция Гуда][goode]... Их\ десятки, если не\ сотни. В\ контексте d3 проекция\ --- это просто функция,
которая переводит географические координаты (широту и\ долготу) в\ координаты на\ экране.

На\ картинке выше используется Меркатор. Это можно увидеть и\ в\ коде.

Вынесем код создания проекции в\ самый верх и\ поместим проекцию в\ отдельную переменную, она нам ещё пригодится.

~~~~~javascript
var projection = d3.geo.winkel3();
~~~~~

Код отрисовки карты чуть поменялся и\ стал ссылаться на\ новую переменную.

~~~~~javascript
svg.append("path")
        .datum(topojson.feature(world, world.objects.countries))
        .attr("d", d3.geo.path().projection(projection));
~~~~~

Как, может быть, вы\ уже заметили, я\ заменил Меркатора на\ более аккуратную Winkel Tripel (именно эту проекцию
использует National Geographic для своих изданий). К\ сожалению, проекция не\ входит в\ стандартную поставку d3, поэтому
придётся подключить ещё отдельный плагин с\ её\ определением.

~~~~~html
<script src="http://d3js.org/d3.geo.projection.v0.min.js"></script>
~~~~~

Обновляем страницу и\ смотрим на\ новую картинку.

![[Результат][result02], [Исходники][source02]](/images/map-tutorial/map-02.png)

# Страны и\ GeoJSON

Возвращаемся к\ данным. Помните, что мы\ преобразовали наши данные в\ компактный TopoJSON? Перед отрисовкой эти данные
всё равно придётся преобразовать обратно в\ GeoJSON, для этого у\ нас и\ подключена библиотека TopoJSON, посмотрите
в\ заголовке нашего html.

~~~~~javascript
var countries = topojson.feature(data, data.objects.countries).features;
~~~~~

В\ переменной `countries` теперь содержится массив с\ описанием стран в\ формате GeoJSON. Нарисуем их\ с\ помощью
раздельных элементов `<path>`, благо d3 позволяет сделать это быстро и\ просто.

Создаём группу (элемент `<g>`), которая очень пригодится нам в\ будущем. Это нужно вставить сразу после объявления
переменной `svg`.

~~~~~javascript
var g = svg.append('g');
~~~~~

Заменяем код, ответственный за\ рисование.

~~~~~javascript
svg.selectAll('.country').data(countries).enter()
        .append('path')
        .attr('class', 'country')
        .attr('d', path);
~~~~~

Если вы\ ещё не\ знаете, что делает `enter` c\ выборкой из\ `selectAll`, то\ быстренько [сходите и\ восполните этот
пробел][enter].

Переменная `path` в\ последней строчке\ --- это функция, которая переводит описание страны из\ GeoJSON в\ строку,
подходящую для отображения в\ svg. Её можно объявить в\ самом верху, сразу после объявления проекции.

~~~~~javascript
var path = d3.geo.path().projection(projection);
~~~~~

Обновляем страницу ([Результат][result03], [Исходники][source03]). Теперь границы государств стали более различимыми.
Сделаем их\ ещё более видимыми с\ помощью css. Добавим в\ заголовок страницы блок `<style>`:

~~~~~html
<style>
    .country {
        fill: #ffffdd;
        stroke: #226688;
    }
</style>
~~~~~

![[Результат][result04], [Исходники][source04]](/images/map-tutorial/map-03.png)

# Фон и\ сетка координат

Следующий этап. Подсветим водные пространства синим. Для этого добавляем сразу после создания группы элемент `<path>`,
который будет отвечать за\ фон.

~~~~~javascript
g.append('path')
        .datum({'type': 'Sphere'})
        .attr('class', 'background')
        .attr('d', path);
~~~~~

Добавим также стиль фона к\ нашему css вверху документа.

~~~~~css
.background {
    fill: #ddeeff;
}
~~~~~

![[Результат][result05], [Исходники][source05]](/images/map-tutorial/map-04.png)

Теперь добавим координатную сетку на\ фон.

~~~~~javascript
g.append('path')
        .datum(d3.geo.graticule().minorStep([5, 5]))
        .attr('class', 'graticule')
        .attr('d', path);
~~~~~

И\ стиль для сетки:

~~~~~css
.graticule {
    fill: none;
    stroke: #000;
    stroke-opacity: .3;
    stroke-dasharray: 3, 1;
}
~~~~~

![[Результат][result06], [Исходники][source06]](/images/map-tutorial/map-05.png)

И\ последний штрих\ --- граница карты, просто для красоты. Создаётся точно так\ же, как и\ фон, разве что стиль другой.

~~~~~javascript
g.append('path')
        .datum({'type': 'Sphere'})
        .attr('class', 'graticule outline')
        .attr('d', path);
~~~~~

~~~~~css
.graticule.outline {
    stroke: #333;
    stroke-opacity: 1;
    stroke-width: 1.5px;
    stroke-dasharray: initial;
}
~~~~~

![[Результат][result07], [Исходники][source07]](/images/map-tutorial/map-06.png)

# Масштабирование

Пришла пора добавить интерактива к\ карте. Но\ для начала разместим карту по\ центру и\ растянем на\ весь экран.

Для растягивания проекции нужно указать параметр `scale`, который по-умолчанию равен 150. К\ сожалению, все
соотношения меняются от\ проекции к\ проекции, поэтому просто посчитаем нужные параметры для нашей. Для этого
в\ инспекторе браузера нужно глянуть ширину и\ высоту элемента `path`, соответствующего фону. В\ случае Winkel Triple
ширина равна 772, а\ высота\ --- 472.

Максимальное увеличение, при котором карта будет помещаться на\ экран, по\ горизонтали равно `150 / 772 * width`,
по\ вертикали\ --- `150 / 472 * height`. Выбираем меньшее из\ двух.

~~~~~javascript
var projection = d3.geo.winkel3()
        .scale(Math.min(150 / 772 * width, 150 / 472 * height));
~~~~~

Перемещаем проекцию в\ центр:

~~~~~javascipt
var projection = d3.geo.winkel3()
        .scale(Math.min(150 / 772 * width, 150 / 472 * height))
        .translate([width / 2, height / 2]);
~~~~~

[Результат][result08], [Исходники][source08].

Всё, теперь можно добавлять интерактив. Как и\ положено, в\ таких библиотеках уже много всего придумано и\ реализовано
за\ вас, остаётся только вызвать нужную функцию в\ нужный момент.

Создаём объект `behavior.zoom`, который будет отвечать за\ отслеживание событий:

~~~~~javascript
var zoom = d3.behavior.zoom()
        .scaleExtent([1, 60])
        .size([width, height])
        .on('zoom', onZoom);
~~~~~

Присоединяем созданный объект к\ нашему элементу `<svg>`:

~~~~~javascript
var svg = d3.select("body").append("svg")
        .attr("width", width)
        .attr("height", height)
        .call(zoom);
~~~~~

И\ пишем обработчик для события `zoom`.

~~~~~javascript
function onZoom () {
    var t = d3.event.translate;
    var s = d3.event.scale;

    t[0] = Math.max(Math.min(t[0], 0), width * (1 - s));
    t[1] = Math.max(Math.min(t[1], 0), height * (1 - s));

    zoom.translate(t);
    g.style("stroke-width", 1 / s)
            .attr('transform', 'translate(' + t + ')scale(' + s + ')');
}
~~~~~

В\ d3 параметры события сохраняются в\ специальном объекте `d3.event`, в\ случае события `zoom` нас интересуют два
параметра: `translate`\ --- смещение карты и\ `scale`\ --- текущее увеличение. После получения этих параметров
мы\ накладываем ограничение на\ смещение, чтобы пользователь не\ мог сдвинуть карту дальше её\ границ. Затем
мы\ записываем новое смещение обратно в\ объект `zoom`: без этого он\ будет продолжать считать, что текущее смещение
другое и сообразно обрабатывать новые события от\ мыши. И\ последнее действие\ --- применить новые параметры смещения
и\ увеличения к\ карте: нужно\ же, чтобы картинка менялась соответственно нашим действиям.

Кстати, именно для правильной работы масштабирования мы\ и\ создавали элемент `<g>` выше. Атрибут `transform`
не\ работает с\ элементом `<svg>`.

[Результат][result09], [Исходники][source09].

# Дополнительные данные

Идём дальше. Добавим к\ нашей карте немножко больше данных. Я\ создавал карту посещённых стран и\ городов, о\ чём 
и\ буду рассказывать. Я\ думаю, некоторые идеи можно применить и\ для других целей и\ наборов данных.

Такие совсем специфические данные, как список посещённых стран, взять негде, их\ придётся создавать самостоятельно.
Я\ для этих целей создал файл `data.json` в\ таком формате:

~~~~~json
{ // Список стран
    "AUT": { // Код страны
        "name": "Австрия", // Название страны по-русски
        "color": "turquoise", // Цвет подсветки: blue, green, orange, pink, purple, red, turquoise, yellow
        "cities": [ // Список городов внутри страны
            {
                "name": "Вена", // Название города по-русски
                "lat": 48.216667, // Широта
                "lon": 16.373333 // Долгота
            },
            {
                "name": "Грац",
                "lat": 47.066667,
                "lon": 15.433333
            }
        ]
    },
    ...
}
~~~~~

Чтобы вам не\ заморачиваться с\ созданием собственного файла, я\ предлагаю пока воспользоваться моим, потом переделаете
всё так, как нравится. Скачать файлик можно [по\ этой ссылке][data-link].

Загружаем данные из\ `data.json` сразу после загрузки карты и\ добавляем правильный фон к\ странам.

~~~~~javascript
var visitedData = {};
d3.json("world.json", function (error, world) {
    if (error) {
        console.log(error);
        return;
    }

    d3.json("data.json", function (error, data) {
        if (error) {
            console.log(error);
        } else {
            visitedData = data;
        }

        var countries = topojson.feature(world, world.objects.countries).features;
        g.selectAll('.country').data(countries).enter()
                .append('path')
                .attr('class', 'country')
                .attr('d', path)
                .style('fill', function (d) {
                    var color = visitedData[d.id] && visitedData[d.id].color;
                    return color && COLORS[color] || '#ffffdd';
                });
    });
});
~~~~~

В\ d3, если в\ методы `attr` и\ `style` вторым параметром передать функцию, то\ значения атрибутов и\ стилей
соответственно будут браться из\ результатов вызова этой функции. Первым параметром передаётся значение элемента
массива, ранее заданного с\ помощью метода `data`.

Остаётся добавить массив констант с\ вариантами цвета фона где-нибудь в\ начале файла:

~~~~~javascript
var COLORS = {
    'blue': '#a3cec5',
    'green': '#d3e46f',
    'orange': '#fdc663',
    'pink': '#f3c1d3',
    'purple': '#ceb5cf',
    'red': '#fdaf6b',
    'turquoise': '#aadb78',
    'yellow': '#fae364'
};
~~~~~

Запускаем и\ видим, что ничего не\ поменялось ([Результат][result10], [Исходники][source10]). На\ самом деле причина
проста: в\ файле `world.json` нет информации о\ кодах стран. Ошибка исправляется передачей дополнительного параметра
в\ вызов `topojson`. Перегенерировать `world.json` нужно такой командой:

~~~~~sh
topojson -o world.json --id-property ADM0_A3 countries.json
~~~~~

![[Результат][result11], [Исходники][source11]](/images/map-tutorial/map-07.png)

Теперь можно нарисовать города. Для этого сразу после кода, ответственного за\ рисование стран, добавляем код для
рисования городов:

~~~~~javascript
// Собираем все города в один массив.
for (var i in visitedData) {
    if (visitedData.hasOwnProperty(i) && visitedData[i].cities) {
        cities.push.apply(cities, visitedData[i].cities);
    }
}

// Рисуем города
g.selectAll('.city').data(cities).enter()
        .append('path')
        .attr('class', 'city')
        .attr('d', function (d) {
            return path({
                'type': 'Point',
                'coordinates': [d.lon, d.lat]
            });
        });
~~~~~

Обратите внимание, что координаты в\ `path` передаются в\ формате `[широта, долгота]`.

Не\ забываем в\ начале объявить массив `cities`:


~~~~~javascript
var cities = [];
~~~~~

И\ добавить стиль для городов:

~~~~~css
.city {
    fill: #dd3d30;
}
~~~~~

Смотрим результат:

![[Результат][result12], [Исходники][source12]](/images/map-tutorial/map-08.png)

Да, точки для городов хотелось\ бы сделать поменьше. Добавляем правило рисования точек в\ `path`:

~~~~~javascript
var path = d3.geo.path().projection(projection).pointRadius(1);
~~~~~

А\ ещё неплохо было\ бы сделать так, чтобы размер точек менялся с\ масштабом. Значит, добавляем код в\ конец функции
`onZoom`.

~~~~~javascript
path.pointRadius(Math.max(1/4, 1/s));
g.selectAll('.city')
        .attr('d', function(d) {
            return path({
                'type': 'Point',
                'coordinates': [d.lon, d.lat]
            })
        });
~~~~~

[Результат][result13], [Исходники][source13].

# Подсветка названий

Теперь добавим подпись для объектов при наведении мышкой. Для этого сначала создаём в\ DOM элемент `<div>` с\ классом
`tooltip`.

~~~~~javascript
var tooltip = d3.select('body').append('div')
        .attr('class', 'tooltip')
        .style('display', 'none');
~~~~~

И\ создаём соответствующий ему стиль.

~~~~~css
.tooltip {
    color: #222;
    background: #fff;
    padding: .5em;
    text-shadow: #f5f5f5 0 1px 0;
    border-radius: 2px;
    box-shadow: 0px 0px 2px 0px #a6a6a6;
    opacity: 0.9;
    position: absolute;
}
~~~~~

Осталось написать обработчики событий мыши `mousemove` и\ `mouseout`. Для этого в\ операцию создания стран добавляем
новые инструкции (новый код начинается с\ вызова метода `on`).

~~~~~javascript
g.selectAll('.country').data(countries).enter()
        .append('path')
        .attr('class', 'country')
        .attr('d', path)
        .style('fill', function (d) {
            var color = visitedData[d.id] && visitedData[d.id].color;
            return color && COLORS[color] || '#ffffdd';
        })
        .on('mousemove', function (d) {
            var mouse = d3.mouse(svg.node());

            var name = visitedData[d.id] && visitedData[d.id].name;
            if (!name) {
                return;
            }
            tooltip.style("display", "block")
                    .style("left", mouse[0] + "px")
                    .style("top", mouse[1] + "px")
                    .html(name);
        })
        .on('mouseout', function () {
            tooltip.style('display', 'none');
        });
~~~~~

Из-за того, что метод `on` вызван после вызова `enter`, события вешаются на\ каждый элемент `<path>`
в\ отдельности, а\ в\ параметры передаётся соответствующий элемент из\ массива `countries`.

Метод `d3.mouse` возвращает текущие координаты мыши относительно контейнера, переданного в\ параметрах. В\ нашем случае
это элемент `<svg>`. Дальше все просто: записываем в\ `tooltip` нужное содержимое и\ устанавливаем стили `left`, `top`
и\ `display`.

В\ обработчике события `mouseout` прячем наш элемент с\ подписью. И\ всё работает как надо!

Добавим аналогичный код для отображения подписей к\ городам.

~~~~~javascript
g.selectAll('.city').data(cities).enter()
        .append('path')
        .attr('class', 'city')
        .attr('d', function (d) {
            return path({
                'type': 'Point',
                'coordinates': [d.lon, d.lat]
            });
        })
        .on('mousemove', function (d) {
            var mouse = d3.mouse(svg.node());

            tooltip.style("display", "block")
                    .style("left", mouse[0] + "px")
                    .style("top", mouse[1] + "px")
                    .html(d.name);
        })
        .on('mouseout', function () {
            tooltip.style('display', 'none');
        });
~~~~~

Тут всё абсолютно аналогично.

[Результат][result14], [Исходники][source14].

# Исправление ошибок подсветки

Некоторые из\ вас, должно быть, заметили, что, например, Франция имеет территорию в\ Южной Америке, которая тоже
подсвечивается. А\ от\ этого хочется избавиться, ведь нас там не\ было. Чтобы получить правильный результат, можно
воспользоваться другим набором данных с\ Natural Earth. Снова идём в\ [раздел 1:10m, Cultural][earth-cultural]
и\ скачиваем карту с\ делением по\ subunits ([Admin\ 0\ --- Details, Download map subunits][subunits-download-link]).

Если мы\ просто заменим файл с\ исходными данным, то\ быстро заметим, что кроме Франции изменениям подверглись ещё
несколько стран. Например, Бельгия оказалась разделена на\ три части: Фландрию, Валлонию и\ Брюссель,
да\ и\ на\ Великобритании тоже появились границы между Англией, Шотландией и\ Уэльсом. Получается, чтобы получить
желаемый результат, придётся составить карту из\ двух источников.

Возьмём Францию из\ `ne_10m_admin_0_map_subunits`. Обратите внимание на\ параметр `-where`, который накладывает нужные
ограничения на выборку.

~~~~~sh
ogr2ogr -f GeoJSON subunits.json -where "ADM0_A3 = 'FRA'" ne_10m_admin_0_map_subunits/ne_10m_admin_0_map_subunits.shp
~~~~~

Вторая часть выборки:

~~~~~sh
ogr2ogr -f GeoJSON countries.json -where "ADM0_A3 != 'FRA'" ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp
~~~~~

Теперь собираем это всё в\ один TopoJSON-файл.

~~~~~sh
topojson -o world.json --id-property ADM_A3,SU_A3 -- countries.json subunits.json
~~~~~

Обратите внимание на\ то, как изменился вызов `topojson`, особенно параметр `id-property`. Все дело в\ том, что атрибут
`ADM_A3` содержит код страны, то есть у\ всех частей Франции код будет один и\ тот\ же. Поэтому для неё нужен другой
атрибут в\ качестве идентификатора. Атрибуты в\ параметре `id-property` берутся в\ обратном порядке: сначала утилита
пытается взять `SU_A3`, а\ в\ случае, если его нет, берет предыдущий, то\ есть\ `ADM_A3`.

Теперь для правильной отрисовки нужно добавить в\ массив `countries` новые элементы.

~~~~~javascript
var countries = topojson.feature(world, world.objects.countries).features;
countries.push.apply(countries, topojson.feature(world, world.objects.subunits).features);
~~~~~

Конструкция `push.apply` добавляет все элементы одного массива в\ другой. Более подробно про `this`, `call` и\ `apply`
можно почитать в\ моей старой статье про\ [ООП в\ JavaScript][oop] (раздел "Ключевое слово this").

Осталось исправить в\ нашем файле с\ данными код Франции (новый код\ --- "FXX") и\ наслаждаться результатом.

![[Результат][result15], [Исходники][source15]](/images/map-tutorial/map-09.png)

# Разбиение стран на\ регионы

Следующее пожелание: крупные страны, например, США и\ Россию, поделить на\ штаты и\ субъекты соответственно. Для этого
нам понадобится новый источник данных. Снова идём [на\ привычный адрес][earth-cultural] и\ [скачиваем файл из\ раздела
Admin\ 1\ --- States, Provinces][states-download-link].

Пересобираем файлы, не\ забыв предварительно удалить старые (`org2org` отказывается перезаписывать файлы, если вы\ ещё
не\ заметили).

~~~~~sh
ogr2ogr -f GeoJSON subunits.json -where "ADM0_A3 = 'FRA'" ne_10m_admin_0_map_subunits/ne_10m_admin_0_map_subunits.shp
ogr2ogr -f GeoJSON countries.json -where "ADM0_A3 != 'FRA' and ADM0_A3 != 'RUS' and ADM0_A3 != 'USA'" ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp
ogr2ogr -f GeoJSON regions.json -where "ADM0_A3 = 'RUS' or ADM0_A3 = 'USA'" ne_10m_admin_1_states_provinces_lakes/ne_10m_admin_1_states_provinces_lakes.shp
topojson -o world.json --id-property ADM_A3,SU_A3,adm1_code -- countries.json subunits.json regions.json
~~~~~

Так\ же как и\ раньше, добавляем новые данные для рисования в\ массив `countries`.

~~~~~javascript
var countries = topojson.feature(world, world.objects.countries).features;
countries.push.apply(countries, topojson.feature(world, world.objects.subunits).features);
countries.push.apply(countries, topojson.feature(world, world.objects.regions).features);
~~~~~

Вообще говоря, подсветку штатов и\ субъектов я\ писал отдельно, но\ можно просто добавить нужные идентификаторы в\ файл
`data.json`.

![[Результат][result16], [Исходники][source16]](/images/map-tutorial/map-10.png)

# Атрибуты

В\ файлах, скачанных с\ Natural Earth, есть множество дополнительных данных, не\ только форма границ и\ код страны.
Большая часть этих данных лежит в\ файле `.dbf`, который представляет собой файл базы данных [dBase]. И\ если у\ вас
есть просмотрщик для этого формата, то\ можете смело его открывать. Если\ же нет, то\ можно воспользоваться
[Online-конвертером из\ dbf в\ csv][dbfconv]. Я\ использовал именно этот способ. После конвертации сsv-файл
можно открыть LibreOffice или\ же Microsoft Excel.

`ogr2ogr` тоже копирует все данные из\ файлов атрибутов внутрь результирующего файла, а\ `topojson` может копировать
некоторые необходимые атрибуты в\ файл TopoJSON. Вот таким образом можно добавить атрибут `NAME`, который содержит
английское название объекта:

~~~~~sh
topojson -o world.json --id-property ADM_A3,SU_A3,adm1_code --properties name,NAME -- countries.json subunits.json regions.json
~~~~~

Теперь можно использовать значение поля, например, для отображения названий непосещённых стран. Вот эту строчку нужно
заменить в\ обработчике события `mousemove` для стран:

~~~~~javascript
var name = visitedData[d.id] && visitedData[d.id].name || d.properties.name || d.properties.NAME;
~~~~~

[Результат][result17], [Исходники][source17].

# Уменьшение размера файла с\ картой

Последний нерешенный вопрос: размер файла с\ картой. После всех наших манипуляций он\ стал весить 2.6\ Мб, что,
согласитесь, несколько многовато. `topojson` предоставляет несколько параметров для оптимизации. Один из\ них так
и\ называется\ --- `simplify`. Для получения оптимального результата придётся, конечно, немного поэкспериментировать
с\ величиной, в\ нашем\ же случае вполне подойдёт `1e-6`.

~~~~~sh
topojson -o world.json --id-property ADM_A3,SU_A3,adm1_code --properties name,NAME --simplify 1e-6 -- countries.json subunits.json regions.json
~~~~~

Получившийся файл имеет размер 444\ Кб и\ всё ещё приемлемую детализацию.

[Результат][result18], [Исходники][source18].

# Благодарности

Спасибо [Mike Bostock][mike] за\ такую прекрасную библиотеку. И\ отдельное спасибо ему\ же за\ прекрасные записи
в\ блоге, некоторые идеи из\ которых перекочевали в\ этот пост.

Спасибо всем тем, кто дочитал до\ конца. Надеюсь, этот пост поможет вам.

[Aitoff]: http://en.wikipedia.org/wiki/Aitoff_projection
[countries-download-link]: http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries_lakes.zip
[d3]: http://d3js.org/
[data-link]: https://raw.githubusercontent.com/dikmax/dikmax.name-demos/master/map-tutorial/step10/data.json
[dBase]: http://en.wikipedia.org/wiki/DBase
[dbfconv]: http://dbfconv.com/
[Dymaxion]: https://ru.wikipedia.org/wiki/%D0%9F%D1%80%D0%BE%D0%B5%D0%BA%D1%86%D0%B8%D1%8F_%D0%94%D0%B8%D0%BC%D0%B0%D0%BA%D1%81%D0%B8%D0%BE%D0%BD
[earth]: http://www.naturalearthdata.com/downloads/
[earth-cultural]: http://www.naturalearthdata.com/downloads/10m-cultural-vectors/
[enter]: https://github.com/mbostock/d3/wiki/Selections#enter
[GDAL]: http://www.gdal.org/index.html
[goode]: https://ru.wikipedia.org/wiki/%D0%9F%D1%80%D0%BE%D0%B5%D0%BA%D1%86%D0%B8%D1%8F_%D0%93%D1%83%D0%B4%D0%B0
[GeoJSON]: http://geojson.org/
[map]: /map/
[merkator]: https://ru.wikipedia.org/wiki/%D0%9F%D1%80%D0%BE%D0%B5%D0%BA%D1%86%D0%B8%D1%8F_%D0%9C%D0%B5%D1%80%D0%BA%D0%B0%D1%82%D0%BE%D1%80%D0%B0
[mike]: http://bost.ocks.org/mike/
[node.js]: http://nodejs.org/
[oop]: /post/oopjs-2/
[result01]: /demos/map-tutorial/step01/
[result02]: /demos/map-tutorial/step02/
[result03]: /demos/map-tutorial/step03/
[result04]: /demos/map-tutorial/step04/
[result05]: /demos/map-tutorial/step05/
[result06]: /demos/map-tutorial/step06/
[result07]: /demos/map-tutorial/step07/
[result08]: /demos/map-tutorial/step08/
[result09]: /demos/map-tutorial/step09/
[result10]: /demos/map-tutorial/step10/
[result11]: /demos/map-tutorial/step11/
[result12]: /demos/map-tutorial/step12/
[result13]: /demos/map-tutorial/step13/
[result14]: /demos/map-tutorial/step14/
[result15]: /demos/map-tutorial/step15/
[result16]: /demos/map-tutorial/step16/
[result17]: /demos/map-tutorial/step17/
[result18]: /demos/map-tutorial/step18/
[Shapefile]: http://en.wikipedia.org/wiki/Shapefile
[source01]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step01
[source02]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step02
[source03]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step03
[source04]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step04
[source05]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step05
[source06]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step06
[source07]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step07
[source08]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step08
[source09]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step09
[source10]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step10
[source11]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step11
[source12]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step12
[source13]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step13
[source14]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step14
[source15]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step15
[source16]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step16
[source17]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step17
[source18]: https://github.com/dikmax/dikmax.name-demos/tree/master/map-tutorial/step18
[states-download-link]: http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_1_states_provinces_lakes.zip
[subunits-download-link]: http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_map_subunits.zip
[TopoJSON]: https://github.com/topojson/topojson-specification/blob/master/README.md
[winkel]: http://en.wikipedia.org/wiki/Winkel_tripel_projection
