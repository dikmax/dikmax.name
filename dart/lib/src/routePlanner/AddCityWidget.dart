part of routePlanner;

class AddCityWidget {
  Model _model;
  Element _element;

  List<City> _suggestions = null;
  int _activeSuggestion = -1;

  int get activeSuggestion => _activeSuggestion;
  void set activeSuggestion(int val) {
    Element el = _element.querySelector('.list-group-item-info');
    if (el != null) {
      el.classes.remove('list-group-item-info');
    }
    _activeSuggestion = val;
    el = _element.querySelector('.suggestion-$_activeSuggestion');
    if (el != null) {
      el.classes.add('list-group-item-info');
    }
  }

  List<City> get suggestions => _suggestions;
  void set suggestions(List<City> val) {
    _suggestions = val;

    Element searchResult = _element.querySelector('.suggestions-list');
    if (searchResult != null) {
      searchResult.remove();
    }
    if (_suggestions == null) {
      _element.querySelector('.hide-button').classes.add('hidden');
      return;
    }
    _element.querySelector('.hide-button').classes.remove('hidden');
    if (_suggestions.length == 0) {
      _element.append(
        new DivElement()
          ..classes.add('suggestions-list')
          ..text = 'Ничего не найдено.' // TODO l10n
      );

      return;
    }

    UListElement list = new UListElement()
      ..classes.addAll(['list-group', 'suggestions-list']);
    for (int i = 0; i < _suggestions.length; ++i) {
      list.append(
        new LIElement()
          ..classes.addAll(['list-group-item', 'suggestion-$i'])
          ..append(
            new AnchorElement()
              ..href = '#'
              ..text = _suggestions[i].fullName
              ..onClick.listen((e) {
                e.preventDefault();
                addCity(_suggestions[i]);
              })
          )
      );
    }

    _element.append(list);
  }

  AddCityWidget(this._model, this._element) {
    _generateMarkup();
  }

  void _generateMarkup() {
    FormElement form = new FormElement()
      ..classes.add('form-inline')
      ..append(
        new DivElement()
          ..classes.add('input-group')
          ..append(
            new LabelElement()
              ..classes.add('sr-only')
              ..htmlFor = "searchText"
              ..text = 'Добавить город' // TODO l10n
          )
          ..append(
            new SearchInputElement()
              ..classes.addAll(['form-control', 'input-sm', 'search-input'])
              ..id = 'searchText'
              ..autocomplete = 'off'
              ..placeholder = 'Добавить город' // TODO l10n
              ..onKeyDown.listen(onSearchInputKeyDown)
          )
          ..append(
            new SpanElement()
              ..classes.add('input-group-btn')
              ..append(
                new ButtonElement()
                  ..classes.addAll(['btn', 'btn-default', 'btn-sm', 'hidden', 'hide-button'])
                  ..title = "Скрыть результаты" // TODO l10n
                  ..append(
                    new SpanElement()
                      ..classes.addAll([Icons.COMMON_CLASS, Icons.REFRESH])
                  )
                  ..onClick.listen((_) {
                    suggestions = null;
                    (_element.querySelector('.search-input') as SearchInputElement).value = '';
                  })
              )
              ..append(
                new ButtonElement()
                  ..classes.addAll(['btn', 'btn-default', 'btn-sm'])
                  ..title = "Искать" // TODO l10n
                  ..append(
                    new SpanElement()
                      ..classes.addAll(['fa', 'fa-search'])
                  )
                  ..onClick.listen((_) => searchCity())
              )
          )
      );

    _element.append(form);
  }

  void onSearchInputKeyDown(KeyboardEvent event) {
    bool stop = false;
    switch (event.keyCode) {
      case KeyCode.ENTER:
        if (suggestions != null && activeSuggestion != -1) {
          addCity(suggestions[activeSuggestion]);
        } else {
          searchCity();
        }
        stop = true;
        break;

      case KeyCode.ESC:
        suggestions = null;
        (_element.querySelector('.search-input') as SearchInputElement).value = '';
        stop = true;
        break;

      case KeyCode.UP:
        if (suggestions == null) {
          break;
        }
        --activeSuggestion;
        if (activeSuggestion < 0) {
          activeSuggestion = suggestions.length - 1;
        }
        _scrollIntoView();
        stop = true;
        break;

      case KeyCode.DOWN:
        if (suggestions == null) {
          break;
        }
        ++activeSuggestion;
        if (activeSuggestion >= suggestions.length) {
          activeSuggestion = 0;
        }
        _scrollIntoView();
        stop = true;
        break;
    }

    if (stop) {
      event.preventDefault();
      event.stopPropagation();
    }
  }

  void addCity(City city) {
    _model.addCity(city);

    suggestions = null;
    (_element.querySelector('.search-input') as SearchInputElement).value = '';
  }

  void searchCity() {
    // TODO error handling

    GeoCoder.geoCode((_element.querySelector('.search-input') as SearchInputElement).value).then((value) {
      if (value.length == 1) {
        addCity(value[0]);
        return;
      }
      suggestions = value;
      activeSuggestion = -1;
    });
  }

  void _scrollIntoView() {
    Element element = querySelector('.suggestions-list .suggestion-${activeSuggestion}');
    Point offset = element.offsetTo(document.body);
    if (offset.y < window.scrollY + 70) {
      window.scrollTo(window.scrollX, offset.y - 70);
    }
    if (offset.y + element.clientHeight > window.scrollY + window.innerHeight) {
      window.scrollTo(window.scrollX, offset.y + element.clientHeight - window.innerHeight);
    }
  }
}
