part of routePlanner;

class ExclusionsWidget {
  Element _element;
  Model _model;

  ExclusionsWidget(this._model, this._element) {
    _generateMarkup();
    _updateExclusions();

    _model.onExclude.listen((_) => _updateExclusions());
    _model.onInclude.listen((_) => _updateExclusions());
  }

  void _generateMarkup() {
    UListElement ul = new UListElement()
      ..classes.add('list-group')
      ..append(
        new LIElement()
          ..classes.add('list-group-item')
          ..append(
            new HeadingElement.h4()
              ..classes.add('list-group-item-heading')
              ..text = 'Исключения'  // TODO l10n
          )
      );

    _element.append(ul);
  }

  LIElement _getEmpty() => new LIElement()
    ..classes.addAll(['list-group-item', 'exclusions-item'])
    ..text = 'Нет исключений'; // TODO l10n

  void _updateExclusions() {
    ElementList list = _element.querySelectorAll(".exclusions-item");
    list.forEach((el) => el.remove());

    Element parent = _element.children[0];
    if (_model.exclusions.length == 0) {
      parent.append(_getEmpty());
      return;
    }

    for (List<City> segment in _model.exclusions) {
      parent.append(
        new LIElement()
          ..classes.addAll(['list-group-item', 'exclusions-item'])
          ..append(
            new ButtonElement()
              ..classes.addAll(['btn', 'btn-default', 'btn-xs'])
              ..title = "Включить"  // TODO l10n
              ..append(
                new SpanElement()
                  ..classes.addAll(['glyphicon', 'glyphicon-remove'])
              )
              ..onClick.listen((_) => _model.include(segment))
          )
          ..appendHtml(" ${segment[0].name} &mdash; ${segment[1].name}")
      );
    }
  }
}