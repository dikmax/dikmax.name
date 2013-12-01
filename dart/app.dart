library app;

import 'dart:html';
import 'package:intl/intl.dart';

class App {
  void init() {
    _fixTimeZones();
  }

  void _fixTimeZones() {
    List<Node> spans = document.getElementsByTagName('span');

    DateFormat format = new DateFormat('cccc, d MMMM yyyy, HH:mm', 'RU_ru');

    var dt = new DateTime.now();
    var tz = dt.timeZoneOffset;
    print(tz);

    spans.forEach((Element el) {
      String postDate = el.dataset['postDate'];
      if (postDate == null) {
        return;
      }

      DateTime date = DateTime.parse(postDate);
      if (date == null) {
        return;
      }

      el.text = format.format(date.add(tz));
    });
  }
}