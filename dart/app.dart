library app;

import 'dart:html';
import 'package:intl/intl.dart';
import 'package:cookie/cookie.dart' as cookie;

class App {
  void init() {
    _setupJumbotron();
    _fixTimeZones();
  }

  void _setupJumbotron() {
    Element closeButton = query('.close-jumbotron-button');
    Element openButton = query('.open-jumbotron-button');
    if (closeButton == null || openButton == null) {
      return;
    }

    Element jumbotron = query('.jumbotron');
    Element jumbotronFolded = query('.jumbotron-folded');

    closeButton.onClick.listen((event) {
      jumbotron.style.display = 'none';
      jumbotronFolded.style.display = 'block';
      cookie.set('closeJumbotron', '1', expires: 180);
      event.stopPropagation();
    });
    openButton.onClick.listen((event) {
      jumbotron.style.display = 'block';
      jumbotronFolded.style.display = 'none';
      cookie.set('closeJumbotron', '1', expires: 180);
      event.stopPropagation();
    });

    if (cookie.get('closeJumbotron') == '1') {
      jumbotron.style.display = 'none';
      jumbotronFolded.style.display = 'block';
    }
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