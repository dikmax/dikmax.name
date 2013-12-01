import 'app.dart';

import 'package:intl/date_symbols.dart';
import "package:intl/src/date_format_internal.dart";

void main () {
  var app = new App();
  initializeDateSymbols(dateTimeSymbolMap);
  initializeDatePatterns(dateTimePatternMap);
  app.init();
}

Map dateTimeSymbolMap () => {
    "ru": new DateSymbols(
        NAME: "ru",
        ERAS: const [ 'до н.э.', 'н.э.'],
        ERANAMES: const [ 'до н.э.', 'н.э.'],
        NARROWMONTHS: const [ 'Я', 'Ф', 'М', 'А', 'М', 'И', 'И', 'А',
        'С', 'О', 'Н', 'Д'],
        STANDALONENARROWMONTHS: const [ 'Я', 'Ф', 'М', 'А', 'М', 'И', 'И',
        'А', 'С', 'О', 'Н', 'Д'],
        MONTHS: const [ 'января', 'февраля', 'марта',
        'апреля', 'мая', 'июня', 'июля', 'августа',
        'сентября', 'октября', 'ноября',
        'декабря'],
        STANDALONEMONTHS: const [ 'Январь', 'Февраль', 'Март',
        'Апрель', 'Май', 'Июнь', 'Июль', 'Август',
        'Сентябрь', 'Октябрь', 'Ноябрь',
        'Декабрь'],
        SHORTMONTHS: const [ 'янв.', 'февр.', 'марта', 'апр.',
        'мая', 'июня', 'июля', 'авг.', 'сент.', 'окт.',
        'нояб.', 'дек.'],
        STANDALONESHORTMONTHS: const [ 'Янв.', 'Февр.', 'Март',
        'Апр.', 'Май', 'Июнь', 'Июль', 'Авг.', 'Сент.',
        'Окт.', 'Нояб.', 'Дек.'],
        WEEKDAYS: const [ 'воскресенье', 'понедельник',
        'вторник', 'среда', 'четверг', 'пятница',
        'суббота'],
        STANDALONEWEEKDAYS: const [ 'Воскресенье',
        'Понедельник', 'Вторник', 'Среда',
        'Четверг', 'Пятница', 'Суббота'],
        SHORTWEEKDAYS: const [ 'вс', 'пн', 'вт', 'ср', 'чт', 'пт',
        'сб'],
        STANDALONESHORTWEEKDAYS: const [ 'Вс', 'Пн', 'Вт', 'Ср', 'Чт',
        'Пт', 'Сб'],
        NARROWWEEKDAYS: const [ 'В', 'Пн', 'Вт', 'С', 'Ч', 'П', 'С'],
        STANDALONENARROWWEEKDAYS: const [ 'В', 'П', 'В', 'С', 'Ч', 'П',
        'С'],
        SHORTQUARTERS: const [ '1-й кв.', '2-й кв.', '3-й кв.',
        '4-й кв.'],
        QUARTERS: const [ '1-й квартал', '2-й квартал',
        '3-й квартал', '4-й квартал'],
        AMPMS: const [ 'до полудня', 'после полудня'],
        DATEFORMATS: const [ 'EEEE, d MMMM y \'г\'.', 'd MMMM y \'г\'.',
        'dd.MM.yyyy', 'dd.MM.yy'],
        TIMEFORMATS: const [ 'H:mm:ss zzzz', 'H:mm:ss z', 'H:mm:ss', 'H:mm'],
        FIRSTDAYOFWEEK: 0,
        WEEKENDRANGE: const [5, 6],
        FIRSTWEEKCUTOFFDAY: 6)
};

Map dateTimePatternMap() => const {
    'ru' : const {
        'd': 'd', // DAY
        'E': 'ccc', // ABBR_WEEKDAY
        'EEEE': 'cccc', // WEEKDAY
        'LLL': 'LLL', // ABBR_STANDALONE_MONTH
        'LLLL': 'LLLL', // STANDALONE_MONTH
        'M': 'L', // NUM_MONTH
        'Md': 'd.M', // NUM_MONTH_DAY
        'MEd': 'EEE, dd.MM', // NUM_MONTH_WEEKDAY_DAY
        'MMM': 'LLL', // ABBR_MONTH
        'MMMd': 'd MMM', // ABBR_MONTH_DAY
        'MMMEd': 'ccc, d MMM', // ABBR_MONTH_WEEKDAY_DAY
        'MMMM': 'LLLL', // MONTH
        'MMMMd': 'd MMMM', // MONTH_DAY
        'MMMMEEEEd': 'cccc, d MMMM', // MONTH_WEEKDAY_DAY
        'QQQ': 'QQQ', // ABBR_QUARTER
        'QQQQ': 'QQQQ', // QUARTER
        'y': 'y', // YEAR
        'yM': 'MM.y', // YEAR_NUM_MONTH
        'yMd': 'dd.MM.y', // YEAR_NUM_MONTH_DAY
        'yMEd': 'EEE, dd.MM.y', // YEAR_NUM_MONTH_WEEKDAY_DAY
        'yMMM': 'LLL y', // YEAR_ABBR_MONTH
        'yMMMd': 'd MMM y \'г\'.', // YEAR_ABBR_MONTH_DAY
        'yMMMEd': 'EEE, d MMM y', // YEAR_ABBR_MONTH_WEEKDAY_DAY
        'yMMMM': 'LLLL y', // YEAR_MONTH
        'yMMMMd': 'd MMMM y \'г\'.', // YEAR_MONTH_DAY
        'yMMMMEEEEd': 'EEEE, d MMMM y \'г\'.', // YEAR_MONTH_WEEKDAY_DAY
        'yQQQ': 'y QQQ', // YEAR_ABBR_QUARTER
        'yQQQQ': 'y QQQQ', // YEAR_QUARTER
        'H': 'H', // HOUR24
        'Hm': 'H:mm', // HOUR24_MINUTE
        'Hms': 'H:mm:ss', // HOUR24_MINUTE_SECOND
        'j': 'H', // HOUR
        'jm': 'H:mm', // HOUR_MINUTE
        'jms': 'H:mm:ss', // HOUR_MINUTE_SECOND
        'jmv': 'H:mm v', // HOUR_MINUTE_GENERIC_TZ
        'jmz': 'H:mm z', // HOUR_MINUTETZ
        'jz': 'H z', // HOURGENERIC_TZ
        'm': 'm', // MINUTE
        'ms': 'mm:ss', // MINUTE_SECOND
        's': 's', // SECOND
        'v': 'v', // ABBR_GENERIC_TZ
        'z': 'z', // ABBR_SPECIFIC_TZ
        'zzzz': 'zzzz', // SPECIFIC_TZ
        'ZZZZ': 'ZZZZ'  // ABBR_UTC_TZ
    }
};

