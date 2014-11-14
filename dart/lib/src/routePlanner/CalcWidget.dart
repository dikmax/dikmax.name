part of routePlanner;

class CalcWidget {
  Element _element;
  Model _model;

  bool _autoUpdate;

  bool get autoUpdate => _autoUpdate;
  void set autoUpdate(bool val) {
    (_element.querySelector('.autoupdate-checkbox') as CheckboxInputElement).checked = val;
    _autoUpdate = val;
  }

  TSPAlgorithm _calcAlgorithm;

  CalcWidget(this._model, this._element) {
    _generateMarkup();
    autoUpdate = false;

    _model.onEndCityChange.listen(_modelChange);
    _model.onCityAdd.listen(_modelChange);
    _model.onCityRemove.listen(_modelChange);
    _model.onClearCities.listen(_modelChange);
    _model.onAddManyCities.listen(_modelChange);
    _model.onExclude.listen(_modelChange);
    _model.onInclude.listen(_modelChange);
  }

  void _generateMarkup() {
    _element.insertBefore(
      new ButtonElement()
        ..classes.addAll(['btn', 'btn-default', 'btn-xs', 'pull-right', 'refresh-button'])
        ..title = 'Обновить' // TODO l10n
        ..append(
          new SpanElement()
            ..classes.addAll(["fa", "fa-refresh"])
        ..onClick.listen((_) => calc())
      ), _element.children[0]
    );

    _element.append(
      new FormElement()
        ..append(
          new DivElement()
            ..classes.add("checkbox")
            ..append(
              new LabelElement()
                ..append(
                  new CheckboxInputElement()
                    ..classes.add('autoupdate-checkbox')
                    ..onChange.listen((e) =>
                      _autoUpdate = (_element.querySelector('.autoupdate-checkbox') as CheckboxInputElement).checked)
                )
                ..appendText("Автообновление") // TODO l10n
            )
        )
    );
  }

  Timer _calcTimer;

  static final Duration calcDelay = new Duration(milliseconds: 50);

  void _modelChange(_) {
    if (!autoUpdate) {
      return;
    }

    if (_calcTimer != null && _calcTimer.isActive) {
      _calcTimer.cancel();
    }

    _calcTimer = new Timer(calcDelay, calc);
  }

  void calc() {
    if (_calcAlgorithm != null) {
      _calcAlgorithm.cancel();
      _model.calcProgress = false;
      _calcAlgorithm = null;
    }
    Timer.run(_calc);
  }

  void _calc() {
    if (_calcAlgorithm != null) {
      return;
    }

    if (_model.cities.length == 0) {
      bool pathExcluded = false;
      _model.exclusions.forEach((List<City> element) {
        if (element[0] == _model.firstCity && element[1] == _model.lastCity ||
        element[1] == _model.firstCity && element[0] == _model.lastCity) {
          pathExcluded = true;
        }
      });

      if (pathExcluded) {
        _model.path = null;
        return;
      }

      _model.path = new Path(
              <List<City>>[<City>[_model.firstCity, _model.lastCity]],
              _model.firstCity.distanceTo(_model.lastCity)
      );
      return;
    }

    _model.calcProgress = true;

    // Prepare tables
    List<City> index = <City>[_model.firstCity];
    index.addAll(_model.cities);
    index.add(_model.lastCity);

    int iLen = index.length;

    List<List<double>> c = <List<double>>[];
    for (int i = 0; i < iLen; ++i) {
      c.add(new List<double>.filled(iLen, TSPAlgorithm.inf));
    }

    for (int i = 0; i < _model.cities.length; ++i) {
      c[0][i+1] = _model.firstCity.distanceTo(_model.cities[i]);
      c[i+1][iLen - 1] = _model.lastCity.distanceTo(_model.cities[i]);
    }
    c[iLen - 1][0] = 0.0; // Just for convenience

    for (int i = 0; i < _model.cities.length - 1; ++i) {
      for (int j = i + 1; j < _model.cities.length; ++j) {
        double distance = _model.cities[i].distanceTo(_model.cities[j]);
        c[i + 1][j + 1] = distance;
        c[j + 1][i + 1] = distance;
      }
    }

    // Apply exclusions
    if (_model.exclusions != null) {
      for (List<City> exclusion in _model.exclusions) {
        // Each city may appear many times in index. Especially in case first/last city
        int index0 = -1;
        while ((index0 = index.indexOf(exclusion[0], index0 + 1)) != -1) {
          int index1 = -1;
          while ((index1 = index.indexOf(exclusion[1], index1 + 1)) != -1) {
            c[index0][index1] = TSPAlgorithm.inf;
            c[index1][index0] = TSPAlgorithm.inf;
          }
        }
      }
    }

    Element button = querySelector('.route-widget .refresh-button');
    Element icon = button.querySelector('.fa');
    button.classes.add('disabled');
    icon.classes.add('in-progress');
    _calcAlgorithm = new AntColonyOptimization();
    _calcAlgorithm.solve(c).then((ar) {
      _updateResult(ar);
    }).whenComplete(() {
      icon.classes.remove('in-progress');
      button.classes.remove('disabled');
      _calcAlgorithm = null;
      _model.calcProgress = false;
    }).catchError((_) {});
  }

  City _getCityByIndex(int index) {
    if (index == 0) {
      return _model.firstCity;
    } else if (index == _model.cities.length + 1) {
      return _model.lastCity;
    } else {
      return _model.cities[index - 1];
    }
  }

  void _updateResult(AlgorithmResult ar) {
    if (ar.distance > TSPAlgorithm.inf) { // Path not found
      _model.path = null;
    } else {
      List<List<City>> path = <List<City>>[];
      for (int i = 0; i < ar.points.length - 1; ++i) {
        path.add(<City>[_getCityByIndex(ar.points[i]), _getCityByIndex(ar.points[i + 1])]);
      }

      _model.path = new Path(path, ar.distance);
    }
  }
}