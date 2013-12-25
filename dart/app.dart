library app;

import 'dart:async';
import 'dart:html';
import 'package:intl/intl.dart';
import 'package:cookie/cookie.dart' as cookie;

class App {
  void init() {
    _setupJumbotron();
    _setupTopNavBar();
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

  void _setupTopNavBar() {
    Element toggleButton = query('.navbar-toggle-button');
    Element collapsibleBlock = query('.navbar-collapsible-block');

    bool visible = false;

    var complete = (_) {
      if (visible) {
        collapsibleBlock.classes
            ..add('in')
            ..remove('collapse')
            ..remove('collapsing');
      } else {
        collapsibleBlock.classes
            ..add('collapse')
            ..remove('collapsing')
            ..remove('in');
        collapsibleBlock.style.height = '0';
      }
    };

    collapsibleBlock.onTransitionEnd.listen(complete);

    toggleButton.onClick.listen((event) {
      visible = !visible;

      collapsibleBlock.classes
        ..add('collapsing')
        ..remove('collapse')
        ..remove('in');
      if (visible) {
        int height = collapsibleBlock.scrollHeight + 1;
        collapsibleBlock.style.height = "${height}px";
      } else {
        collapsibleBlock.style.height = "0";
      }
    });
  }

  void _fixTimeZones() {
    List<Node> spans = document.getElementsByTagName('span');

    DateFormat format = new DateFormat('cccc, d MMMM yyyy, HH:mm', 'RU_ru');

    var dt = new DateTime.now();
    var tz = dt.timeZoneOffset;

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