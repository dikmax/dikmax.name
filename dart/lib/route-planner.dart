library routePlanner;

import 'dart:async';
import 'dart:convert';
import 'package:crypto/crypto.dart';
import 'dart:html';
import 'dart:js';
import 'dart:math';
import 'tsp.dart';
import 'package:archive/archive.dart';
import 'src/common/icons.dart';


part 'src/routePlanner/AddCityWidget.dart';
part 'src/routePlanner/CalcWidget.dart';
part 'src/routePlanner/CitiesListWidget.dart';
part 'src/routePlanner/City.dart';
part 'src/routePlanner/ExclusionsWidget.dart';
part 'src/routePlanner/GeoCoder.dart';
part 'src/routePlanner/MapWidget.dart';
part 'src/routePlanner/Model.dart';
part 'src/routePlanner/Path.dart';
part 'src/routePlanner/RouteWidget.dart';

class RoutePlannerApplication {
  Model _model;
  MapWidget map;
  CitiesListWidget citiesList;
  ExclusionsWidget exclusions;
  RouteWidget route;

  RoutePlannerApplication() {
    _model = new Model();
    map = new MapWidget(_model, querySelector('.map-widget'));
    citiesList = new CitiesListWidget(_model, querySelector('.cities-list-widget'));
    route = new RouteWidget(_model, querySelector('.route-widget'));
    exclusions = new ExclusionsWidget(_model, querySelector('.exclusions-widget'));

    _initialize();
    window.onHashChange.listen(_onHashChangeListener);
    _model.onPathChange.listen((_) => updateUrl());

    route.calcWidget.autoUpdate = true;
    querySelector('.main-widget').classes.remove('hidden');
  }

  void _initialize() {
    bool initialized = _initializeFromHash();
    if (!initialized) {
      City minsk = new City("Минск", 53.906077, 27.554914);
      _model.firstCity = minsk;
      _model.lastCity = minsk;
      _model.path = new Path(<List<City>> [[minsk, minsk]], 0.0);
    }
  }

  void _onHashChangeListener(Event e) {
    route.calcWidget.autoUpdate = false;
    map.reset();
    _initialize();
    route.calcWidget.autoUpdate = true;
  }

  bool _initializeFromHash() {
    String hash = window.location.hash;
    if (hash == '' || hash == '#') {
      return false;
    }
    hash = hash.substring(1);

    try {
      List<int> bz2 = CryptoUtils.base64StringToBytes(hash);
      List<int> bytes = new BZip2Decoder().decodeBytes(bz2);
      String str = UTF8.decode(bytes);
      List items = JSON.decode(str);
      bool roundTrip = false;
      int count;
      if (items.last is bool && items.last == true) {
        roundTrip = true;
        items.removeLast();
        count = items.length;
      } else {
        count = items.length - 1;
      }
      _model.removeAllCities();

      _model.firstCity = new City.fromJson(items.first);

      List<City> cities = [];
      double distance = 0.0;
      List<List<City>> path = <List<City>>[];
      City prevCity = _model.firstCity;
      for (int i = 1; i < count; ++i) {
        City city = new City.fromJson(items[i]);
        cities.add(city);
        path.add(<City>[prevCity, city]);
        distance += prevCity.distanceTo(city);
        prevCity = city;
      }
      if (roundTrip) {
        _model.lastCity = _model.firstCity;
        path.add(<City>[prevCity, _model.firstCity]);
      } else {
        _model.lastCity = new City.fromJson(items.last);
        path.add(<City>[prevCity, _model.lastCity]);
      }
      distance += prevCity.distanceTo(_model.lastCity);

      _model.addAllCities(cities);
      _model.path = new Path(path, distance);
    } catch (e) {
      return false;
    }
    return true;
  }

  void updateUrl() {
    if (_model.path == null) {
      window.location.hash = '#';
    }
    List data = [
      _model.firstCity.toJson()
    ];
    int length = _model.path.path.length - (_model.firstCity == _model.lastCity ? 1 : 0);

    for (int i = 0; i < length; ++i) {
      data.add(_model.path.path[i][1].toJson());
    }
    if (_model.firstCity == _model.lastCity) {
      data.add(true);
    }

    List<int> bytes = UTF8.encode(JSON.encode(data));
    List<int> bz2 = new BZip2Encoder().encode(bytes);

    String base64 = CryptoUtils.bytesToBase64(bz2);
    if (History.supportsState) {
      window.history.replaceState(null, document.title, window.location.pathname + "#" + base64);
    } else {
      window.location.hash = '#' + base64;
    }
  }
}
