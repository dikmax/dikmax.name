part of routePlanner;

class CitiesListWidget {
  Model _model;
  Element _element;
  Map<City, LIElement> _cities = <City, LIElement>{}; // TODO check for multiple instances

  CitiesListWidget(this._model, this._element) {
    _generateMarkup();

    _model
      ..onEndCityChange.listen(endCityChange)
      ..onCityAdd.listen(addCity)
      ..onAddManyCities.listen(addManyCities)
      ..onClearCities.listen(clearCities)
      ..onCityRemove.listen(removeCity);
  }

  void _generateMarkup() {
    LIElement addCityElement = new LIElement()
      ..classes.add('list-group-item');
    new AddCityWidget(_model, addCityElement);

    UListElement ul = new UListElement()
      ..classes.add('list-group')
      ..append(
        new LIElement()
          ..classes.add('list-group-item')
          ..append(
            new HeadingElement.h4()
              ..classes.add('list-group-item-heading')
              ..text = 'Города'  // TODO l10n
          )
      )
      ..append(addCityElement)
      ..append(
        new LIElement()
          ..classes.addAll(['list-group-item', 'first-city'])
      )
      ..append(
        new LIElement()
          ..classes.addAll(['list-group-item', 'last-city'])
      );

    _element.append(ul);
  }

  void endCityChange(ChangeEvent<City> event) {
    if (_model.firstCity != null) {
      _updateNode(_element.querySelector('.first-city'), false, true, _model.firstCity);
    }
    if (_model.lastCity != null) {
      _updateNode(_element.querySelector('.last-city'), true, false, _model.lastCity);
    }
  }

  void addCity(City city) {
    LIElement item = new LIElement()
      ..classes.addAll(['list-group-item']);
    _updateNode(item, true, true, city);

    _element.children.first.insertBefore(item, _element.querySelector('.last-city'));
    _cities[city] = item;
  }

  void addManyCities(List<City> cities) => cities.forEach(addCity);

  void removeCity(City city) {
    LIElement item = _cities[city];
    if (item == null) {
      return;
    }

    item.remove();
    _cities.remove(city);
  }

  void clearCities(List<City> cities) => cities.forEach(removeCity);

  ButtonElement _createButton(String title, String icon) {
    return new ButtonElement()
      ..classes.addAll(["btn", "btn-default", "btn-xs"])
      ..title = title
      ..append(
        new SpanElement()
          ..classes.addAll(["glyphicon", "glyphicon-$icon"])
    );
  }

  void _updateNode(Element element, bool makeFirst, bool makeLast, City city) {
    element.innerHtml = '';

    ButtonElement removeButton = _createButton("Удалить", "remove"); // TODO l10n
    if (!makeFirst || !makeLast) {
      removeButton.disabled = true;
    } else {
      removeButton.onClick.listen((_) => _model.removeCity(city));
    }

    ButtonElement makeFirstButton = _createButton("В начало", "chevron-up"); // TODO l10n
    if (!makeFirst) {
      makeFirstButton.disabled = true;
    } else {
      makeFirstButton.onClick.listen((_) => _model.makeFirst(city));
    }

    ButtonElement makeLastButton = _createButton("В конец", "chevron-down"); // TODO l10n
    if (!makeLast) {
      makeLastButton.disabled = true;
    } else {
      makeLastButton.onClick.listen((_) => _model.makeLast(city));
    }

    DivElement div = new DivElement()
      ..classes.add("btn-group")
      ..append(removeButton)
      ..append(makeFirstButton)
      ..append(makeLastButton);

    element
      ..append(div)
      ..appendText(" " + city.name);
  }
}
