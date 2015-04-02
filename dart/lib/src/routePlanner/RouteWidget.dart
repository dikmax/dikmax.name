part of routePlanner;

class RouteWidget {
  Model _model;
  Element _element;

  CalcWidget calcWidget;

  RouteWidget(this._model, this._element) {
    _generateMarkup();
    calcWidget = new CalcWidget(_model, _element.querySelector('.route-header'));
    _model.onPathChange.listen(pathChange);
    _model.onCalcProgress.listen(calcProgress);
  }

  void _generateMarkup() {
    UListElement ul = new UListElement()
      ..classes.add('list-group')
      ..append(
        new LIElement()
          ..classes.addAll(['list-group-item', 'route-header'])
          ..append(
            new HeadingElement.h4()
              ..classes.addAll(['list-group-item-heading', 'route-title'])
          )
      );

    _element.append(ul);
  }

  void calcProgress(bool isCalc) {
    if (isCalc) {
      ElementList routeItems = _element.querySelectorAll('.route-item');
      routeItems.forEach((item) => item.remove());
      Element parent = _element.children.first;
      parent.append(
        new LIElement()
          ..classes.addAll(['list-group-item', 'route-item', 'calc-progress'])
          ..append(
            new DivElement()
              ..classes.addAll(['progress', 'progress-striped', 'active'])
              ..append(
                new DivElement()
                  ..classes.add('progress-bar')
                  ..style.width = '100%'
              )
          )
      );
    } else {
      ElementList routeItems = _element.querySelectorAll('.route-item.calc-progress');
      routeItems.forEach((item) => item.remove());
    }
  }

  void pathChange(ChangeEvent<Path> event) {
    ElementList routeItems = _element.querySelectorAll('.route-item');
    routeItems.forEach((item) => item.remove());

    Path path = event.newValue;
    if (path == null) {
      _element.querySelector('.route-title').text = 'Маршрут не существует'; // TODO l10n
      return;
    }

    _element.querySelector('.route-title').text = 'Маршрут ${path.distanceRound} км'; // TODO l10n

    Element parent = _element.children.first;
    path.path.forEach((List<City> segment) =>
      parent.append(
        new LIElement()
          ..classes.addAll(['list-group-item', 'route-item'])
          ..append(
            new SpanElement()
              ..classes.addAll(['badge', 'distance', 'pull-right'])
              ..text = "${segment[0].distanceTo(segment[1]).round()} км" // TODO l10n
          )
          ..append(
            new ButtonElement()
              ..classes.addAll(['btn', 'btn-default', 'btn-xs'])
              ..title = 'Исключить' // TODO l10n
              ..append(
                new SpanElement()
                  ..classes.addAll([Icons.commonClass, Icons.remove])
              )
              ..onClick.listen((_) => _model.exclude(segment))
          )
          ..appendHtml(' ${segment[0].name} &mdash; ${segment[1].name}')
      )
    );
  }
}