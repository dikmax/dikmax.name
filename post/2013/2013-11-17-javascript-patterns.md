---
title: "JavaScript patterns"
date: "2013-11-17T18:00:00+03:00"
published: true
tags: "javascript, patterns, программирование"
thread: 1971897301
---

Просматривал старые заметки в\ Evernote и\ набрел на\ список паттернов написания кода для\ JavaScript.
[Вот они][patterns].

Для повышения качества кода и\ улучшения понимания специфики JavaScript привожу тут весьма вольный перевод первой,
не\ связанной с\ jQuery, части. Некоторые вещи могут показаться тривиальными, но\ большая часть программистов
почему-то забывает о\ них, когда они действительно нужны.

Очень надеюсь, что этот пост кому-нибудь пригодится и\ вы\ найдёте для себя что-нибудь новое.



# 1. Объявление функций

Создание анонимных функций и\ присваивание их\ переменным.

## Плохо

~~~~~javascript
function getData() {
}
~~~~~

## Хорошо

~~~~~javascript
var getData = function () {
};
~~~~~

Преимущества:

1. Улучшает понимание "функций как объектов".
2. Навязывает хорошую привычку ставить точки с\ запятой.
3. Нет навязанного предыдущим опытом представления о\ функциях и\ областях видимости.

## Именованное функциональное выражение

~~~~~javascript
var getData = function getData () {
};
~~~~~

Преимущества:

1. Даёт отладчику определённое имя функции. Это упрощает изучение стека вызовов.
2. Позволяет делать рекурсивные вызовы: `getData` может вызывать саму себя по\ имени.

Недостатки: не\ работает в\ IE, и\ [CoffeeScript] не\ понимает подобные выражения
(<https://github.com/jashkenas/coffee-script/issues/366>).

## Именованное функциональное выражение + "F"

~~~~~javascript
var getData = function getDataF () {
};
~~~~~

Преимущества:

1. Избавление от\ `(anonymous function)` в\ стеке вызовов.
2. Возможность рекурсивного вызова при использовании имя + "F".
3. Работает в\ IE (по\ крайней мере до\ тех пор пока нет коллизии имён, как описано здесь:
<https://github.com/jashkenas/coffee-script/issues/366#issuecomment-242134>).

## Ссылки

1. <http://ejohn.org/blog/javascript-as-a-first-language/>
2. <http://kangax.github.com/nfe/>



# 2. Условия

Cпособы использования if-else.

## Стандартный способ

~~~~~javascript
if (type === 'foo' || type === 'bar') {
}
~~~~~

## Альтернативный способ 1: регулярное выражение

~~~~~javascript
if (/^(foo|bar)$/.test(type)) {
}
~~~~~

## Альтернативный способ 2: поиск в\ объекте

Этот способ будет короче, когда в\ условии менее пяти элементов.

~~~~~javascript
if (({foo:1, bar:1})[type]) {
}
~~~~~

## Альтернативный способ 3: подход как в\ двоичном поиске

Этот подход лучше применять, когда нужно проверять диапазоны значений.

До:

~~~~~javascript
if (value == 0) {
    return result0;
} else if (value == 1) {
    return result1;
} else if (value == 2) {
    return result2;
} else if (value == 3) {
    return result3;
} else if (value == 4) {
    return result4;
} else if (value == 5) {
    return result5;
} else if (value == 6) {
    return result6;
} else if (value == 7) {
    return result7;
} else if (value == 8) {
    return result8;
} else if (value == 9) {
    return result9;
} else {
    return result10;
}
~~~~~

После:

~~~~~javascript
if (value < 6) {
    if (value < 3) {
        if (value == 0) {
            return result0;
        } else if (value == 1) {
            return result1;
        } else {
            return result2;
        }
    } else {
        if (value == 3) {
            return result3;
        } else if (value == 4) {
            return result4;
        } else {
            return result5;
        }
    }
} else {
    if (value < 8) {
        if (value == 6) {
            return result6;
        } else {
            return result7;
        }
    } else {
        if (value == 8) {
            return result8;
        } else if (value == 9) {
            return result9;
        } else {
            return result10;
        }
    }
}
~~~~~

## Альтернативный способ 4: Таблицы поиска

Таблицы поиска наиболее полезны, когда есть соответствие между ключом и значением.

До:

~~~~~javascript
if (value == 0) {
    return result0;
} else if (value == 1) {
    return result1;
} else if (value == 2) {
    return result2;
}
~~~~~

После:

~~~~~javascript
// Определяем массив результатов.
var results = [result0, result1, result2];
// Возвращаем правильный результат.
return results[value];
~~~~~

## Альтернативный способ 5: только логические операторы

Более короткий способ записи операторов.

~~~~~javascript
var
    type = 'foo',
    type2 = 'bar',
    result = 0;

type == 'foo' && result++;
console.log(result); // 1
!type == 'foo' || result++;
console.log(result); // 2
type == 'foo' && type2 == 'bar' && result++;
console.log(result); // 3
type == 'foo' && type2 == 'bar' && result == 3 && (result=0);
// Скобки нужны, чтобы избежать ошибки с неверной левой частью у присваивания
console.log(result); // 0
type == 'OOF' || result++; // Эквивалентно type != 'OOF' && result++;
console.log(result); // 1
~~~~~

## Ссылки

<http://paulirish.com/2009/perf/>.

Paul Irish отмечает, что первый вариант (стандартный способ) не\ слишком походит, когда требуется уменьшить размер
исходного кода, например, для кода-закладки (bookmarklet). Стандартный способ для небольшого количества условий
обычно в\ цикле работает быстрее, чем регулярное выражение (альтернативный способ 1), и\ быстрее поиска по\ объекту
(альтернативный способ 2). Скорость выравнивается примерно на\ десяти условиях. Смотрите
<http://jsperf.com/if-this-or-that>.



# 3. Доступ к глобальному объекту

Доступ к глобальному объекту без указания идентификатора `window` напрямую. Должно работать в\ ES3, ES5
и\ ES5-strict.

~~~~~javascript
var global = (function () {
    return this || (1, eval)('this');
}());
~~~~~

Тесты: <http://jsperf.com/globalx>.



# 4. Одно объявление var

Нужно использовать одно определение `var` на функцию.

Преимущества:

1. Единственное место в\ коде, где будут объявлены все локальные переменные, требуемые для функции.
2. Защищает от\ логических ошибок, когда переменная используется до\ того, как она объявлена.
3. Напоминает о\ том, что нужно объявлять переменные, и\ таким образом уменьшает количество глобальных переменных.
4. Меньше букв (чтобы набирать и передавать).

~~~~~javascript
function func() {
    var a = 1
        , b = 2
        , sum = a + b
        , myobject = {}
        , i
        , j;

    // Тело функции...
}

function updateElement() {
    var el = document.getElementById("result")
        , style = el.style;

    // Сделать что-нибудь с el и style...
}
~~~~~

> От\ себя добавлю, что я\ не\ пользуюсь этим паттерном. Мне кажется, что современные IDE вроде WebStore могут
> позаботиться о\ локальных переменных вместо тебя, главное обращать внимание на\ их\ замечания.



# 5. Hoisting (подъём)

Объявление `var` где-либо в\ функции действует так, как если\ бы переменные были объявлены в\ самом верху.

Неправильно:

~~~~~javascript
myname = "global"; // глобальная переменная
function func() {
    alert(myname); // "undefined"
    var myname = "local";
    alert(myname); // "local"
}
func();
~~~~~

Этот кусок кода будет себя вести будто написан так:

~~~~~javascript
myname = "global"; // глобальная переменная
function func() {
    var myname; // то же, что и var myname = undefined;
    alert(myname); // "undefined"
    myname = "local";
    alert(myname); // "local"
}
func();
~~~~~



# 6. Оптимизация циклов for

## Стандартный вариант

~~~~~javascript
for (var i = 0; i < myarray.length; i++) {
    // сделать что-нибудь с myarray[i]
}
~~~~~

## Оптимизация 1

Кешировать размер массива, используя `max`.

~~~~~javascript
for (var i = 0, max = myarray.length; i < max; i++) {
    // сделать что-нибудь с myarray[i]
}
~~~~~

## Оптимизация 2

Использовать только одно объявление `var` для соответствия предыдущим советам.

Замечание: недостаток в\ том, что станет немного сложнее копировать циклы целиком во\ время рефакторинга.

~~~~~javascript
var i = 0,
    max,
    myarray = [];

for (i = 0, max = myarray.length; i < max; i++) {
    // сделать что-нибудь с myarray[i]
}
~~~~~

## Оптимизация 3

Заменить `i++` на\ `i = i + 1` или `i += 1`, чтобы избежать излишней сложности.

~~~~~javascript
var i = 0,
    max,
    myarray = [];

for (i = 0, max = myarray.length; i < max; i += 1) {
    // сделать что-нибудь с myarray[i]
}
~~~~~

## Предпочтительный вариант 1

~~~~~javascript
var i, myarray = [];
for (i = myarray.length; i--;) {
    // сделать что-нибудь с myarray[i]
}
~~~~~

## Предпочтительный вариант 2

~~~~~javascript
var myarray = [],
    i = myarray.length;
while (i--) {
    // сделать что-нибудь с myarray[i]
}
~~~~~



# 7. Оптимизация циклов for-in

Объект:

~~~~~javascript
var man = {
    hands:2,
    legs:2,
    heads:1
};
~~~~~

Где-то в коде был добавлен метод ко всем объектам:

~~~~~javascript
if (typeof Object.prototype.clone === 'undefined') {
    Object.prototype.clone = function () {
    };
}
~~~~~

## Неправильно

Цикл for-in без проверки `hasOwnProperty`.

~~~~~javascript
for (var i in man) {
    console.log(i, ":", man[i]);
}
~~~~~

Результат в\ консоли:

~~~~~text
hands : 2
legs : 2
heads : 1
clone: function()
~~~~~

## Предпочтительный вариант 1

~~~~~javascript
for (var i in man) {
    if (man.hasOwnProperty(i)) { // фильтр
        console.log(i, ":", man[i]);
    }
}
~~~~~

Результат в\ консоли:

~~~~~text
hands : 2
legs : 2
heads : 1
~~~~~

## Предпочтительный вариант 2

Преимущество этого варианта в\ том, что можно избежать коллизий имён, если в\ объекте `man` переопределено свойство
`hasOwnProperty`.

~~~~~javascript
for (var i in man) {
    if (Object.prototype.hasOwnProperty.call(man, i)) { // фильтр
        console.log(i, ":", man[i]);
    }
}
~~~~~

## Предпочтительный вариант 3

Используем локальную переменную, чтобы закешировать `Object.prototype.hasOwnProperty`.

~~~~~javascript
var i,
    hasOwn = Object.prototype.hasOwnProperty;
for (i in man) {
    if (hasOwn.call(man, i)) { // фильтр
        console.log(i, ":", man[i]);
    }
}
~~~~~



# 8. (Не) изменение встроенных прототипов

Этот паттерн может сильно навредить поддерживаемости кода, потому что сделает его менее предсказуемым. Но\ можно сделать
исключение, когда следующие условия выполняются:

1. Ожидается, что будущие версии ECMAScript или JavaScript добавят эту функциональность. Например, можно добавить
  методы, описанные в\ ECMAScript\ 5, пока не\ все браузеры реализовали эту функциональность. В\ этом случае вы\ просто
  определяете необходимые методы заранее.
2. Вы\ убеждаетесь, что ваше новое свойство ещё не\ существует. Ведь оно может являться частью JavaScript-движка
  в\ одном из\ браузеров или, возможно, было уже где-то добавлено в\ код.
3. Вы\ задокументируете и\ обсудите с\ командой это изменение.

~~~~~javascript
if (typeof Object.prototype.myMethod !== "function") {
    Object.prototype.myMethod = function () {
        // реализация...
    };
}
~~~~~


# 9. `switch`

Улучшение читаемости и надёжности операторов `switch`.

Правила:

1. Выравнивайте каждый `case` с\ `switch` (исключение к\ правилам отступов после фигурной скобки)
2. Делайте отступы внутри каждого `case`.
3. Заканчивайте каждый `case` явным `break;`.
4. Избегайте fall-through (случай, когда вы\ специально убираете `break`). Если вы\ совершенно уверены, что
  fall-through\ --- наилучшее решение, убедитесь, что задокументировали подобные случаи. Они могут выглядеть как
  ошибки для тех, кто будет читать код впоследствии.
5. Пишите `default` в\ конце `switch`, чтобы конструкция возвращала нормальный результат даже если ни\ один из\ `case`
  не\ сработал.

~~~~~javascript
var inspect_me = 0,
    result = '';
switch (inspect_me) {
case 0:
    result = "zero";
    break;
case 1:
    result = "one";
    break;
default:
    result = "unknown";
}
~~~~~



# 10. Неявное приведение типов

Нужно избегать неявного приведения типов.

~~~~~javascript
var zero = 0;
~~~~~

## Плохо

JavaScript неявно приводит типы переменных, когда сравнивает их. Поэтому сравнения вроде `false == 0` или `"" == 0`
возвращают `true`.

~~~~~javascript
if (zero == false) {
    // Этот блок выполнится...
}
~~~~~

## Хорошо

Чтобы избежать непонятных ситуаций, связанных с\ неявным приведением типов, всегда используйте операторы `===` и `!==`.
Эти операторы сравнивают не\ только значения, но\ и\ типы.

~~~~~javascript
if (zero === false) {
    // Не выполнится, потому что zero равно 0, а не false
}
~~~~~

Замечание: есть и\ другое мнение, заключающееся в\ том, что использование `===` является излишним, когда `==`
достаточно. Например, когда вы\ используете `typeof`, вы\ знаете, что оно возвращает строку, поэтому нет необходимости
в\ стогом сравнении. Однако [JSLint] требует строгого равенства. Это делает код более последовательным и\ уменьшает
умственное усилие, требуемое для чтения кода: это `==` специально поставили, или забыли поставить `===`.



# 11. Избегайте `eval`

## Плохо 1

~~~~~javascript
var property = "name";
alert(eval("obj." + property));
~~~~~

## Хорошо 1

~~~~~javascript
var property = "name";
alert(obj[property]);
~~~~~

## Плохо 2

Важно понимать, что передача строк в\ `setInterval()`, `setTimeout()` и\ конструктор `Function()` в\ большей части схожи
с\ использованием `eval`, и, следовательно, нужно этого избегать.

~~~~~javascript
setTimeout("myFunc()", 1000);
setTimeout("myFunc(1, 2, 3)", 1000);
~~~~~

## Хорошо 2

~~~~~javascript
setTimeout(myFunc, 1000);
setTimeout(function () {
    myFunc(1, 2, 3);
}, 1000);
setTimeout(myFunc, 1000, 1, 2, 3); // в некоторых браузерах (т.е. не в IE)
~~~~~



# 12. Конвертирование чисел с помощью `parseInt`

Нужно использовать второй параметр\ --- основание.

## Вариант 1

Если опустить в\ этом примере второй параметр (написать `parseInt(year)`), то\ в\ результате получится 0. Это потому что
“09” предполагает восьмеричное число, как если бы вы выполняли `parseInt(year, 8)`, а\ "09" не\ является допустимым
числом по\ основанию\ 8.

~~~~~javascript
var month = "06",
    year = "09";
month = parseInt(month, 10);
year = parseInt(year, 10);
~~~~~

## Вариант 2

Если вы\ ожидаете данные вроде “08 hello”, то `parseInt()` вернёт число, в\ то\ время как остальные варианты вернут
`NaN`.

~~~~~javascript
+"08" // Результат: 8
Number("08") // 8
~~~~~



# 13. Глобальные переменные

Глобальные переменные\ --- это переменные, объявленные вне всех функций, или просто использованные без объявления.

~~~~~javascript
myglobal = "hello"; // плохо
console.log(myglobal); // "hello"
console.log(window.myglobal); // "hello"
console.log(window["myglobal"]); // "hello"
console.log(this.myglobal); // "hello"
~~~~~



# 14. Проблемы с глобальными переменными

## Плохо 1

~~~~~javascript
function sum(x, y) {
    // Неявная глобальная переменная
    result = x + y;
    return result;
}
~~~~~

## Хорошо 1

~~~~~javascript
function sum(x, y) {
    // Переменная, объявленная внутри функции, недоступна вне ее.
    var result = x + y;
    return result;
}
~~~~~~

## Плохо 2

~~~~~javascript
function foo() {
    var a = b = 0;
    // ...
}
~~~~~

Этот код будет вести себя так, как если\ бы вы\ написали:

~~~~~javascript
var a = (b = 0);
~~~~~

## Хорошо 2

~~~~~javascript
function foo() {
    var a, b;
    // ...
    a = b = 0; // обе переменные локальные
}
~~~~~


[patterns]: http://shichuan.github.io/javascript-patterns/
[CoffeeScript]: http://coffeescript.org/
[JSLint]: http://www.jslint.com/
