library app;

import 'dart:async';
import 'dart:html';
import 'dart:js';
import 'package:intl/intl.dart';
import 'package:cookie/cookie.dart' as cookie;

class App {
  void init() {
    _setupJumbotron();
    _setupTopNavBar();
    _fixTimeZones();
    _updateCommentsText();
    _updateCodeListings();
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

  void _updateCommentsText() {
    ElementList elements = queryAll('span.post-comments');
    if (elements.length == 0) {
      return;
    }

    new Timer.periodic(new Duration(microseconds: 100), (timer) {
      if (elements[0].text.startsWith('Считаем')) {
        return;
      }

      timer.cancel();
      elements.forEach((Element el) {
        Element link = el.query('a');
        if (link == null) {
          return;
        }

        link.text = link.text.replaceAllMapped(new RegExp('^(\\d+) комментариев\$'), (Match match) {
          int count = int.parse(match[1]);
          if ((count % 100 / 10).round() != 1) {
            int count10 = count % 10;
            if (count10 == 1) {
              return '$count комментарий';
            } else if (count10 >= 2 && count10 <= 4) {
              return '$count комментария';
            }
          }
          return match[0];
        });
      });
    });
  }

  void _updateCodeListings() {
    if (_highlightBlocks()) {
      _handleCodeTooltips();
    }
  }

  bool _highlightBlocks() {
    final NodeValidatorBuilder _htmlValidator=new NodeValidatorBuilder.common()
      ..allowElement('span', attributes: ['data-linenum']);

    ElementList blocks = queryAll('pre > code.sourceCode');
    if (blocks.length > 0) {
      JsObject hljs = context['hljs'];
      blocks.forEach((HtmlElement block) {
        hljs.callMethod('highlightBlock', [block]);
        // TODO better html parser in case spans are taking 2 or more rows
        List<String> html = block.innerHtml.split('\n');
        RegExp open = new RegExp(r'<span[\s\S]*?>');
        RegExp close = new RegExp(r'</span>');
        List<String> spans = [];
        for (int i = 0; i < html.length; ++i) {
          String result = '<span class="line" data-linenum="${i + 1}">';
          spans = [];
          if (html[i] == '') {
            result += '&nbsp;';
          } else {
            String line = spans.join('') + html[i];
            result += line;
            Iterable<Match> openMatches = open.allMatches(line);
            Iterable<Match> closeMatches = close.allMatches(line);
            for (int j = closeMatches.length; j < openMatches.length; ++j) {
              spans.add(openMatches.elementAt(j).group(0));
              result += '</span>';
            }
          }
          result += '</span>';

          html[i] = result;
        }
        block.setInnerHtml(html.join('\n'), validator: _htmlValidator);
        block.classes.add('highlighted');
      });
      return true;
    } else {
      return false;
    }
  }

  Element _codeTooltip;

  void _handleCodeTooltips() {
    if (_codeTooltip == null) {
      _codeTooltip = new DivElement()
          ..append(new DivElement()
              ..classes.add('tooltip-arrow'))
          ..append(new DivElement()
              ..classes.add('tooltip-inner'))
          ..classes.addAll(['tooltip', 'fade', 'left', 'in']);
      _codeTooltip.style.display = 'none';

      document.body.append(_codeTooltip);
    }

    Element tooltipTarget;

    queryAll('span.line').onClick.listen((MouseEvent event) {
      if (tooltipTarget == event.currentTarget) {
        // Hide on second click
        _codeTooltip.style.display = 'none';
        tooltipTarget = null;
        return;
      }

      tooltipTarget = event.currentTarget;
      _codeTooltip.query('.tooltip-inner').text = '#' + tooltipTarget.getAttribute('data-linenum');
      _codeTooltip.style
          ..visibility = 'hidden'
          ..display = 'block';

      _codeTooltip.style
          ..left = "${tooltipTarget.offsetLeft - _codeTooltip.clientWidth}px"
          ..top = "${tooltipTarget.offsetTop - 2}px"
          ..visibility = "visible";
    });
  }
}