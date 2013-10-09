---
title: "inherited.js v0.1"
date: "2012-08-14T22:50:37+03:00"
published: true
tags: "inheritedjs, javascript, программирование"
thread: 805349803
---

Как я и обещал, выкладываю в общий доступ библиотеку для создания классов в JavaScript. Исходный код можно
[посмотреть на Github](https://github.com/dikmax/inheritedjs).

Пример использования:

~~~~~javascript
TestA = createClass(
    /** @lends TestA */
    {
        name: 'st.a.very.long.namespace.TestA',

        /** @constructs */
        constructor: function () {
            console.log('Constructor TestA');
        },

        testMethod1: function () {
            console.log('TestA.testMethod1');
        },

        /**
         * @param {number} param
         */
        testMethod2: function (param) {
            console.log('TestA.testMethod2 ' + param);
        }
    }
);

/**
 * @extends TestA
 */
TestB = createClass(
    /** @lends TestB */
    {
        name: 'st.TestB',

        /** @constructs */
        constructor: function TestB() {
            console.log('Constructor TestB');
            this.inherited();
        },
        extend: TestA,

        testMethod1: function () {
            this.inherited();
            console.log('TestB.testMethod1');
        },

        /**
         * @param {number} param
         */
        testMethod2: function (param) {
            this.inherited(param + 1);
            console.log('TestB.testMethod2 ' + param);
        }
    }
);

var testB = new TestB;
console.log(testB);
testB.testMethod1();
testB.testMethod2(1);
~~~~~

Как видно, функция `createClass` возвращает класс. В нее передается объект с описанием класса. На данный момент у этого
объекта есть три зарезервированных поля: `name`, `constructor` и `extend`. Всё остальное переносится в результирующий
класс в своем первоначальном виде. В будущем я планирую расширить список зарезервированных полей еще несколькими,
например, `static`, `properties` и `singleton`.

Теперь немного пройдемся по этим самым специальным полям. Поле `name`\ --- это имя класса, которое будет
отображаться в веб-инспекторе (смотрите [предыдущий пост](/post/inheritedjsnames/));
`constructor`\ --- функция, которая будет выполнять роль конструктора. Ну и `extend`\ --- класс-предок.
К каждому классу добавляется служебный метод `inherited`, с помощью которого можно вызвать соответствующий метод предка.

Кажется всё. Если есть вопросы, мне всегда можно написать.

Чтобы вы имели представление, как это выглядит, вот несколько скриншотов:

![Консоль Web Inspector в Google Chrome](/images/screenshots/inheritedjs-0.1-chrome.png "Консоль Web Inspector в Google Chrome")

![Консоль Firebug в Firefox](/images/screenshots/inheritedjs-0.1-firebug.png "Консоль Firebug в Firefox")

![Просмотр объекта в Firebug](/images/screenshots/inheritedjs-0.1-firebug-2.png "Просмотр объекта в Firebug")

В самом ближайшем обновлении библиотеки я планирую подружить ее с
[JsTestDriver](http://code.google.com/p/js-test-driver/). Слишком уж много у библиотеки мелких деталей, с которыми
приходится постоянно ладить.
