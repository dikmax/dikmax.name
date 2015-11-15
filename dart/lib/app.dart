library app;

import 'dart:async';
import 'dart:html';
import 'dart:js';
import 'dart:math';

class App {
  void init() {
    _setupTranslation();
    _setupMaxImageHeight();
    _setupTopNavBar();
    _fixTimeZones();
    _updateCommentsText();
    _updateCodeListings();
    _setupPano();
    _inlineFootnotes();
    _setupKeyboardNavigation();
    _setupMath();
  }

  void _setupMaxImageHeight() {
    StyleElement styleElement = new StyleElement();
    styleElement.type = 'text/css';
    document.head.append(styleElement);
    final CssStyleSheet styleSheet = styleElement.sheet as CssStyleSheet;

    var height = window.innerHeight - querySelector('.navbar').clientHeight - 60;
    if (height < 200) {
      height = 200;
    }

    final rule = '.img-responsive, .thumbnail>img, .thumbnail a>img, .post-body .figure .figure-inner>img, '
      '.post-body .figure .figure-inner a>img { max-height: ${height}px; }';
    styleSheet.insertRule(rule, 0);

    final panoRule = '.post-body .figure.figure-pano .figure-inner>img, '
        '.post-body .figure.figure-pano .figure-inner a>img { height: ${height}px; }';
    styleSheet.insertRule(panoRule, 1);
  }

  void _setupTopNavBar() {
    bool visible = false;

    Element jumbotron = querySelector('.jumbotron');
    Element navBar = querySelector('.navbar');

    var prevOpacity = 0.9;
    void setNavBarOpacity([num opacity]) {
      if (opacity == null) {
        opacity = prevOpacity;
      } else {
        prevOpacity = opacity;
      }
      if (visible) {
        opacity = 0.9;
      }
      navBar.style.backgroundColor = 'rgba(51,51,51,${opacity})';
    }

    Element toggleButton = querySelector('.navbar-toggle-button');
    Element collapsibleBlock = querySelector('.navbar-collapsible-block');

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
        setNavBarOpacity();
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
        setNavBarOpacity();
      } else {
        collapsibleBlock.style.height = "0";
      }
    });


    // Jumbotron

    if (jumbotron == null) {
      return;
    }

    var navBarHeight = navBar.offsetHeight;
    var jumbotronHeight = jumbotron.offsetHeight;
    setNavBarOpacity(0);

    jumbotron.style.paddingTop = "${48 + navBarHeight}px";
    DivElement jumbotronParent = new DivElement();
    jumbotronParent.style
      ..position = 'relative'
      ..marginTop = "-${navBarHeight}px"
      ..transform = "translateZ(0)";

    jumbotron.replaceWith(jumbotronParent);
    jumbotronParent.append(jumbotron);

    int lastScroll = 0;
    void animationFrame(_) {
      var scroll;
      if (document.documentElement != null) { // IE workaround: IE doesn't have document.body.scrollTop
        scroll = document.documentElement.scrollTop;
      }
      if (scroll == null || scroll == 0) {
        scroll = document.body.scrollTop;
      }
      if (scroll == lastScroll) {
        window.animationFrame.then(animationFrame);
        return;
      }
      lastScroll = scroll;
      if (scroll < 0) {
        scroll = 0;
      }
      if (scroll > jumbotronHeight) {
        scroll = jumbotronHeight;
      }

      num opacity = log(scroll / jumbotronHeight + 1) / log(2);
      setNavBarOpacity(opacity * 0.9);
      jumbotronParent.style.opacity = '${1 - opacity}';

      window.animationFrame.then(animationFrame);
    }

    window.animationFrame.then(animationFrame);
  }

  void _setupTranslation() {
    List<String> languages = [];


    bool _forEachLanguage(bool handler(JsObject)) {
      // Hack
      IFrameElement iframe = document.querySelector('iframe.goog-te-menu-frame');
      if (iframe == null) {
        return false;
      }
      JsObject iframeProxy = new JsObject.fromBrowserObject(iframe);
      JsObject contentDocumentProxy = iframeProxy['contentDocument'];
      JsObject items = contentDocumentProxy.callMethod('querySelectorAll',
          ['.goog-te-menu2-item span.text']);
      var length = items['length'];
      if (length == 0) {
        return false;
      }
      for (var i = 0; i < length; ++i) {
        if (handler(items[i]) == false) {
          return true;
        }
      }

      return true;
    }

    void _clickElement(JsObject item) {
      if (item['fireEvent'] != null) {
        item.callMethod('fireEvent', ['onclick']);
      } else {
        var clickEvent = new Event.eventType('MouseEvent', 'click');
        item.callMethod('dispatchEvent', [clickEvent]);
      }
    }

    void _googleTranslateElementInit() {
      context.deleteProperty('googleTranslateElementInit');
      var languages = window.navigator.languages;
      if (languages == null) {
        languages = [window.navigator.language];
      }
      if (!languages.contains('en')) {
        languages.add('en');
      }
      new JsObject(context['google']['translate']['TranslateElement'],
          [new JsObject.jsify({
            'pageLanguage': 'ru',
            'layout': context['google']['translate']['TranslateElement']['InlineLayout']['SIMPLE'],
            'includedLanguages': languages.join(','),
            'gaTrack': true,
            'gaId': 'UA-32213724-1',
            'autoDisplay': false
          }), 'google_translate_element']);
      //

      Element translationSelector = document.querySelector('.translation-selector');
      translationSelector.querySelector('.translate-text').text =
          context['google']['translate']['m'][0]; // Here google puts localized "Translate" string
      translationSelector.classes.remove('hidden');

      // Show/hide dropdown
      Element dropdown = translationSelector.querySelector('.dropdown');
      translationSelector.querySelector('.dropdown-toggle').onClick.listen((ev) {
        ev.preventDefault();
        ev.stopPropagation();
        dropdown.classes.toggle('open');
      });

      Element dropdownMenu = translationSelector.querySelector('.dropdown-menu');

      List<Element> menuItems = [
        new LIElement()
          ..attributes['role'] = 'separator'
          ..classes.add('divider')
      ];

      new Timer.periodic(new Duration(milliseconds: 100), (Timer timer) {
        bool res = _forEachLanguage((item) {
          String text = item['textContent'];
          menuItems.add(
              new LIElement()
                ..append(
                    new AnchorElement(href: '#')
                      ..dataset['language'] = text
                      ..text = text
                )
          );
        });
        if (!res) {
          return;
        }
        timer.cancel();

        var count = 0;
        new Timer.periodic(new Duration(milliseconds: 100), (Timer timer) {
          if (document.body.style.top != '0px') {
            document.body.style.top = '0';
            timer.cancel();
            return;
          }

          ++count;
          if (count > 600) { // Wait 1 minute if body going to be adjusted by translation
            timer.cancel();
          }
        });

        dropdownMenu.insertAllBefore(menuItems,
            dropdownMenu.querySelector('.divider'));


        dropdownMenu.onClick.matches('li>a')
            .listen((MouseEvent ev) {
          String language = ev.matchingTarget.dataset['language'];
          if (language == null) {
            return;
          }

          ev.preventDefault();
          ev.stopPropagation();

          if (language == "") {
            // Restoring default (russian)

            IFrameElement iframe = document.querySelector('iframe.goog-te-banner-frame');
            if (iframe == null) {
              dropdown.classes.remove('open');
              return;
            }
            JsObject iframeProxy = new JsObject.fromBrowserObject(iframe);
            JsObject contentDocumentProxy = iframeProxy['contentDocument'];
            JsObject items = contentDocumentProxy.callMethod('querySelectorAll',
                ['.goog-te-button button']);
            var length = items['length'];
            for (var i = 0; i < length; ++i) {
              var item = items[i];
              if (item['id'].contains('.restore')) {
                _clickElement(item);
                break;
              }
            }
          } else {
            // Changing language

            _forEachLanguage((item) {
              String text = item['textContent'];
              if (text == language) {
                _clickElement(item);

                return false;
              }

              return true;
            });
          }

          dropdown.classes.remove('open');
        });
      });
    }

    void _addGoogleTranslateWidget() {
      if (languages.firstWhere((String s) => s.startsWith('ru'), orElse: () => null) == null) {
        if (!languages.contains('en')) {
          languages.add('en');
        }

        // Russian not supported. Translation required.
        context['googleTranslateElementInit'] = _googleTranslateElementInit;
        document.body.append(new DivElement()..id = 'google_translate_element');
        document.body.append(
            new ScriptElement()
              ..type = 'text/javascript'
              ..src = '//translate.google.com/translate_a/element.js?cb=googleTranslateElementInit');
      }
    }

    void _setHeaders(data) {
      context.deleteProperty('_setHttpHeaders');
      String accepts = data['Accept-Language'];
      if (accepts != null) {
        languages = new List.from(accepts.split(',').map((String s) => s.split(';')[0]));
      }

      _addGoogleTranslateWidget();
    }

    if (window.navigator.languages == null) {
      // If window.navigator.languages do not exists use external service
      context['_setHttpHeaders'] = _setHeaders;
      document.body.append(
          new ScriptElement()
            ..type = 'text/javascript'
            ..src = '//ajaxhttpheaders2.appspot.com/?callback=_setHttpHeaders');
    } else {
      languages = window.navigator.languages;
      _addGoogleTranslateWidget();
    }
  }

  void _fixTimeZones() {
    List<Node> spans = document.getElementsByTagName('span');

    DateTime dt = new DateTime.now();
    Duration tz = dt.timeZoneOffset;

    List<String> weekdays = const ['Понедельник', 'Вторник', 'Среда', 'Четверг', 'Пятница', 'Суббота', 'Воскресенье'];
    List<String> months = const [ 'января', 'февраля', 'марта', 'апреля', 'мая', 'июня', 'июля', 'августа', 'сентября',
      'октября', 'ноября', 'декабря'];

    spans.forEach((Element el) {
      String postDate = el.dataset['postDate'];
      if (postDate == null) {
        return;
      }

      DateTime date = DateTime.parse(postDate);
      if (date == null) {
        return;
      }

      DateTime localDate = date.add(tz);
      el.text = "${weekdays[localDate.weekday - 1]}, ${localDate.day} ${months[localDate.month - 1]} ${localDate.year}"
        ", ${localDate.hour < 10 ? '0' : ''}${localDate.hour}:${localDate.minute < 10 ? '0' : ''}${localDate.minute}";
    });
  }

  void _updateCommentsText() {
    ElementList elements = querySelectorAll('span.post-comments');
    if (elements.length == 0) {
      return;
    }

    new Timer.periodic(new Duration(milliseconds: 100), (timer) {
      if (elements[0].text.startsWith('Считаем')) {
        return;
      }

      timer.cancel();
      elements.forEach((Element el) {
        Element link = el.querySelector('a');
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

    ElementList blocks = querySelectorAll('pre > code.sourceCode');
    if (blocks.length > 0) {
      JsObject hljs = context['hljs'];
      blocks.forEach((HtmlElement block) {
        hljs.callMethod('highlightBlock', [block]);
        List<String> html = block.innerHtml.split('\n');
        RegExp open = new RegExp(r'<span[\s\S]*?>');
        RegExp close = new RegExp(r'</span>');
        List<String> spans = [];
        for (int i = 0; i < html.length; ++i) {
          String result = '<span class="line" data-linenum="${i + 1}">';
          if (html[i] == '') {
            result += '&nbsp;';
          } else {
            String line = spans.join('') + html[i];
            result += line;
            Iterable<Match> openMatches = open.allMatches(line);
            Iterable<Match> closeMatches = close.allMatches(line);
            spans = [];
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

  void _handleCodeTooltips() {
    // Generate layout
    Element inner = new DivElement()
      ..classes.add('tooltip-inner');
    Element codeTooltip = new DivElement()
        ..append(new DivElement()
            ..classes.add('tooltip-arrow'))
        ..append(inner)
        ..classes.addAll(['tooltip', 'left', 'fade']);
    codeTooltip.classes.add(CssStyleDeclaration.supportsTransitions ? 'out' : 'in');

    codeTooltip.style.display = 'none';
    document.body.append(codeTooltip);

    codeTooltip.onTransitionEnd.listen((TransitionEvent event) {
      if (codeTooltip.classes.contains('out')) {
        codeTooltip.style.display = 'none';
      }
    });

    void hideTooltip() {
      if (!CssStyleDeclaration.supportsTransitions) {
        codeTooltip.style.display = 'none';
      } else {
        codeTooltip.classes
            ..remove('in')
            ..add('out');
      }
    }

    void showTooltip(target) {
      codeTooltip.style
          ..visibility = 'hidden'
          ..display = 'block';

      codeTooltip.style
          ..left = "${target.offsetLeft - codeTooltip.clientWidth}px"
          ..top = "${target.offsetTop - 2}px"
          ..visibility = "visible";

      if (CssStyleDeclaration.supportsTransitions) {
        codeTooltip.classes
          ..remove('out')
          ..add('in');
      }
    }

    Element tooltipTarget;

    bool isTapTriggered = false;

    // Clicks handler
    var lines = querySelectorAll('span.line');
    lines.onClick.listen((MouseEvent event) {
      if (tooltipTarget == event.currentTarget) {
        if (!isTapTriggered) {
          isTapTriggered = true;
          return;
        }

        // Hide on second click
        hideTooltip();
        tooltipTarget = null;
        isTapTriggered = false;
        return;
      }

      isTapTriggered = true;
      tooltipTarget = event.currentTarget;
      inner.text = '#' + tooltipTarget.getAttribute('data-linenum');

      showTooltip(tooltipTarget);
    });

    // Mouse hover handlers
    lines.onMouseMove.listen((MouseEvent event) {
      if (isTapTriggered || tooltipTarget == event.currentTarget) {
        return;
      }

      tooltipTarget = event.currentTarget;
      inner.text = '#' + tooltipTarget.getAttribute('data-linenum');

      showTooltip(tooltipTarget);
    });

    lines.onMouseOut.listen((MouseEvent event) {
      if (isTapTriggered || tooltipTarget != event.currentTarget) {
        return;
      }

      hideTooltip();
      tooltipTarget = null;
    });
  }


  void _handlePano(Element figure, ImageElement pano) {
    Element inner = figure.querySelector('.figure-inner');
    var paragraph = new ParagraphElement()
      ..classes.add('figure-description')
      ..text = 'Нажмите для увеличения';
    if (inner != null) {
      inner.append(paragraph);
    }

    bool collapsed = true;
    String collapsedName = pano.src;
    String fullName = collapsedName.replaceAll(new RegExp(r"-pano\.jpg$"), "-pano-full.jpg");
    figure.onClick.listen((_) {
      collapsed = !collapsed;
      pano.src = collapsed ? collapsedName : fullName;
      figure.classes.toggle('figure-pano', !collapsed);
      paragraph.classes.toggle('hidden', !collapsed);
    });
    figure.style.cursor = 'pointer';
  }


  void _setupPano() {
    ElementList figures = querySelectorAll('.figure');
    figures.forEach((Element figure) {
      Element image = figure.querySelector('img');
      if (image != null && image is ImageElement) {
        if (image.src.endsWith("-pano.jpg")) {
          _handlePano(figure, image);
        }
      }
    });
  }

  void _inlineFootnotes() {
    var links = querySelectorAll('.note-link');
    if (links.length == 0) {
      return;
    }

    Element title = new HeadingElement.h3()
        ..classes.add('popover-title');
    DivElement content = new DivElement()
        ..classes.add('popover-content');
    Element footnote = new DivElement()
        ..append(new DivElement()
            ..classes.add('arrow'))
        ..append(title)
        ..append(content)
        ..classes.addAll(['popover', 'top', 'fade']);
    footnote.classes.add(CssStyleDeclaration.supportsTransitions ? 'out' : 'in');

    footnote.style.display = 'none';
    document.body.append(footnote);

    footnote.onTransitionEnd.listen((TransitionEvent event) {
      if (footnote.classes.contains('out')) {
        footnote.style.display = 'none';
      }
    });

    var currentFootnote;

    final NodeValidatorBuilder _htmlValidator = new NodeValidatorBuilder.common()
      ..allowNavigation(new AllowedUriPolicy());

    links.onClick.listen((MouseEvent event) {
      var target = event.currentTarget;

      if (currentFootnote == target) {
        // hide
        if (!CssStyleDeclaration.supportsTransitions) {
          footnote.style.display = 'none';
        } else {
          footnote.classes
            ..remove('in')
            ..add('out');
        }
        currentFootnote = null;
        return;
      }

      title.text = "Примечание ${target.text}";
      var dataEl = querySelector('.footnotes li[data-for=${target.id}]');
      if (dataEl == null) {
        return;
      }
      content.setInnerHtml(dataEl.innerHtml, validator: _htmlValidator);
      currentFootnote = target;

      footnote.style
        ..visibility = 'hidden'
        ..display = 'block';

      footnote.style
        ..left = "${target.offsetLeft + (target.clientWidth - footnote.clientWidth) / 2 + 2}px"
        ..top = "${target.offsetTop - footnote.clientHeight}px"
        ..visibility = "visible";

      if (CssStyleDeclaration.supportsTransitions) {
        footnote.classes
          ..remove('out')
          ..add('in');
      }
    });
  }

  void _setupKeyboardNavigation() {
    Element previousLink = querySelector('.pager .previous');
    Element nextLink = querySelector('.pager .next');

    bool isMac = window.navigator.platform.indexOf('Mac') != -1;

    if (previousLink != null) {
      previousLink.attributes['title'] += " (${isMac ? '⌥←' : 'Ctrl + ←'})";
    }
    if (nextLink != null) {
      nextLink.attributes['title'] += " (${isMac ? '⌥→' : 'Ctrl + →'})";
    }

    if (previousLink != null || nextLink != null) {
      document.onKeyDown.listen((KeyboardEvent event) {
        if (event.altKey || event.ctrlKey) {
          Element link;
          if (event.keyCode == KeyCode.LEFT) {
            link = previousLink;
          } else if (event.keyCode == KeyCode.RIGHT) {
            link = nextLink;
          }

          if (link != null) {
            window.location.replace(link.querySelector('a').getAttribute('href'));
          }
        }
      });
    }
  }

  void _setupMath() {
    // Load MathJax only if there's math on page
    if (querySelectorAll('span.math').length > 0) {
      Element script = new ScriptElement()
          ..type = "text/javascript"
          ..async = true
          ..src = "//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML";
      document.body.append(script);
    }
  }
}

class AllowedUriPolicy implements UriPolicy {
  bool allowsUri(String uri) {
    return true;
  }
}