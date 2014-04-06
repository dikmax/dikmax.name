part of routePlanner;

class ChangeEvent<T> {
  T oldValue;
  T newValue;

  ChangeEvent(this.newValue, this.oldValue);
}

class Model {
  List<City> _cities;
  City _firstCity;
  City _lastCity;
  Path _path;
  List<List<City>> _exclusions;
  bool _calcProgress;

  Model() {
    _cities = <City>[];
    _exclusions = <List<City>>[];
  }

  City get firstCity => _firstCity;
  void set firstCity(City val) {
    City oldVal = _firstCity;
    _firstCity = val;
    endCityChangeEvent.add(new ChangeEvent(val, oldVal));
  }

  City get lastCity => _lastCity;
  void set lastCity(City val) {
    City oldVal = _lastCity;
    _lastCity = val;
    endCityChangeEvent.add(new ChangeEvent(val, oldVal));
  }

  Path get path => _path;
  void set path(Path val) {
    Path oldVal = _path;
    _path = val;
    pathChangeEvent.add(new ChangeEvent(val, oldVal));
  }

  List<City> get cities => _cities;

  void addCity(City city) {
    _cities.add(city);
    cityAddEvent.add(city);
  }

  void addAllCities(List<City> cities) {
    _cities.addAll(cities);
    addManyCitiesEvent.add(cities);
  }

  void removeCity(City city) {
    _cities.remove(city);
    cityRemoveEvent.add(city);
  }

  void removeAllCities() {
    List<City> oldValues = _cities;
    _cities = <City>[];
    clearCitiesEvent.add(oldValues);
  }

  void makeFirst(City city) {
    City oldFirstCity = _firstCity;
    _cities.remove(city);
    cityRemoveEvent.add(city);
    firstCity = city;

    if (_cities.indexOf(oldFirstCity) == -1 && _lastCity != oldFirstCity) {
      _cities.add(oldFirstCity);
      cityAddEvent.add(oldFirstCity);
    }
  }

  void makeLast(City city) {
    City oldLastCity = _lastCity;
    _cities.remove(city);
    cityRemoveEvent.add(city);
    lastCity = city;

    if (_cities.indexOf(oldLastCity) == -1 && oldLastCity != firstCity) {
      _cities.add(oldLastCity);
      cityAddEvent.add(oldLastCity);
    }
  }

  // TODO replace with List<Segment>
  List<List<City>> get exclusions => _exclusions;

  void exclude(List<City> segment) {
    if (_exclusions.contains(segment)) {
      return;
    }
    _exclusions.add(segment);
    excludeEvent.add(segment);
  }

  void include(List<City> segment) {
    if (_exclusions.remove(segment)) {
      includeEvent.add(segment);
    }
  }

  bool get calcProgress => _calcProgress;
  void set calcProgress(bool val) {
    _calcProgress = val;
    calcProgressEvent.add(val);
  }

  StreamController<ChangeEvent<City>> endCityChangeEvent = new StreamController<ChangeEvent<City>>.broadcast(sync: true);
  Stream<ChangeEvent<City>> get onEndCityChange => endCityChangeEvent.stream;

  StreamController<City> cityAddEvent = new StreamController<City>.broadcast(sync: true);
  Stream<City> get onCityAdd => cityAddEvent.stream;

  StreamController<City> cityRemoveEvent = new StreamController<City>.broadcast(sync: true);
  Stream<City> get onCityRemove => cityRemoveEvent.stream;

  StreamController<List<City>> clearCitiesEvent = new StreamController<List<City>>.broadcast(sync: true);
  Stream<List<City>> get onClearCities => clearCitiesEvent.stream;

  StreamController<List<City>> addManyCitiesEvent = new StreamController<List<City>>.broadcast(sync: true);
  Stream<List<City>> get onAddManyCities => addManyCitiesEvent.stream;

  StreamController<ChangeEvent<Path>> pathChangeEvent = new StreamController<ChangeEvent<Path>>.broadcast(sync: true);
  Stream<ChangeEvent<Path>> get onPathChange => pathChangeEvent.stream;

  StreamController<List<City>> excludeEvent = new StreamController<List<City>>.broadcast(sync: true);
  Stream<List<City>> get onExclude => excludeEvent.stream;

  StreamController<List<City>> includeEvent = new StreamController<List<City>>.broadcast(sync: true);
  Stream<List<City>> get onInclude => includeEvent.stream;

  StreamController<bool> calcProgressEvent = new StreamController<bool>.broadcast(sync: true);
  Stream<bool> get onCalcProgress => calcProgressEvent.stream;

}
