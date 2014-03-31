library routePlanner;

import 'dart:async';
import 'dart:convert';
import 'package:crypto/crypto.dart';
import 'dart:html';
import 'dart:js';
import 'dart:math';
import 'package:angular/angular.dart';
import 'tsp.dart';
import 'package:archive/archive.dart';

// TODO remove var typees
@NgController(
    selector: '.cities-list',
    publishAs: 'ctrl'
)
class CitiesListController {
  List<City> cities;
  String newCity = '';
  City firstCity;
  City lastCity;
  List<City> suggestions;
  List<List<City>> exclusions;
  bool showSuggestions = false;
  int activeSuggestion = -1;
  bool autoUpdate = true;
  Path result;
  int elapsed = 0;
  bool loaded = true;

  var map;
  var route;

  CitiesListController() {
    var options;
    options = new JsObject.jsify({
        "behaviors": ["drag", "scrollZoom", "dblClickZoom", "multiTouch", "rightMouseButtonMagnifier"],
        "center": [53.906077, 27.554914],
        "zoom": 8
    });
    map = new JsObject(context['ymaps']['Map'], ["map", options]);
    map['controls'].callMethod('add', ['zoomControl']);

    bool initialized = _initializeFromHash();
    if (!initialized) {
      City minsk = new City("Минск", 53.906077, 27.554914);
      firstCity = minsk;
      lastCity = minsk;
      cities = [];
      /*cities = [
        new City("Загреб", 45.807205, 15.967563),
        new City("Братислава", 48.149248, 17.106986),
        new City("Вена", 48.202536, 16.368796),
        new City("Кишинёв", 47.024672, 28.832336),
        new City("Бухарест", 44.434288, 26.102958),
        new City("Будапешт", 47.506216, 19.06482),
        new City("Любляна", 46.051259, 14.503067),
        new City("Рим", 41.903044, 12.495799),
        new City("Венеция", 45.438108, 12.318166),
        new City("Палермо", 38.121359, 13.358433)
      ];*/
    }
    suggestions = [];
    exclusions = [];

    var placemark = firstCity.placemark;
    placemark['options'].callMethod('set', ['preset', 'twirl#blueDotIcon']);
    map["geoObjects"].callMethod("add", [placemark]);

    if (firstCity != lastCity) {
      placemark = lastCity.placemark;
      placemark['options'].callMethod('set', ['preset', 'twirl#blueDotIcon']);
      map["geoObjects"].callMethod("add", [placemark]);
    }

    if (!initialized) {
      calc();
    }
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
      firstCity = new City.fromJson(items.first);
      cities = [];
      double distance = 0.0;
      List<int> path = [0];
      City prevCity = firstCity;
      for (int i = 1; i < count; ++i) {
        City city = new City.fromJson(items[i]);
        cities.add(city);
        path.add(i);
        distance += prevCity.distanceTo(city);
        prevCity = city;
      }
      if (roundTrip) {
        lastCity = firstCity;
        path.add(items.length);
      } else {
        lastCity = new City.fromJson(items.last);
      }
      distance += prevCity.distanceTo(lastCity);

      for (City city in cities) {
        var placemark = city.placemark;
        placemark['options'].callMethod('set', ['preset', 'twirl#blueIcon']);
        map["geoObjects"].callMethod("add", [placemark]);
      }

      _updateResult(new AlgorithmResult(path, distance));
    } catch (e) {
      return false;
    }

    return true;
  }

  void searchCity() {
    // TODO error handling
    Geocoder.geocode(newCity).then((value) {
      if (value.length == 1) {
        addCity(value[0], null);
        return;
      }
      suggestions = value;
      showSuggestions = true;
      activeSuggestion = -1;
    });
  }

  void _scrollIntoView() {
    var element = query('.suggestions-list .suggestion-${activeSuggestion}');
    Point offset = element.offsetTo(document.body);
    if (offset.y < window.scrollY + 70) {
      window.scrollTo(window.scrollX, offset.y - 70);
    }
    if (offset.y + element.clientHeight > window.scrollY + window.innerHeight) {
      window.scrollTo(window.scrollX, offset.y + element.clientHeight - window.innerHeight);
    }
  }


  void cityKeyDown(KeyboardEvent event) {
    bool stop = false;
    switch (event.keyCode) {
    case KeyCode.ENTER:
      if (showSuggestions && activeSuggestion != -1) {
        addCity(suggestions[activeSuggestion], null);
      } else {
        searchCity();
      }
      stop = true;
      break;

    case KeyCode.ESC:
      showSuggestions = false;
      newCity = '';
      stop = true;
      break;

    case KeyCode.UP:
      --activeSuggestion;
      if (activeSuggestion < 0) {
        activeSuggestion = suggestions.length - 1;
      }
      _scrollIntoView();
      stop = true;
      break;

    case KeyCode.DOWN:
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

  void remove(City city) {
    cities.remove(city);
    map["geoObjects"].callMethod("remove", [city.placemark]);
    if (autoUpdate) {
      calc();
    }
  }

  void makeFirst(City city) {
    if (cities.indexOf(firstCity) == -1 && lastCity != firstCity) {
      cities.add(firstCity);
      firstCity.placemark['options'].callMethod('set', ['preset', 'twirl#blueIcon']);
    }
    cities.remove(city);
    firstCity = city;
    firstCity.placemark['options'].callMethod('set', ['preset', 'twirl#blueDotIcon']);
    if (autoUpdate) {
      calc();
    }
  }

  void makeLast(City city) {
    if (cities.indexOf(lastCity) == -1 && lastCity != firstCity) {
      cities.add(lastCity);
      lastCity.placemark['options'].callMethod('set', ['preset', 'twirl#blueIcon']);
    }
    cities.remove(city);
    lastCity = city;
    lastCity.placemark['options'].callMethod('set', ['preset', 'twirl#blueDotIcon']);
    if (autoUpdate) {
      calc();
    }
  }

  void addCity(city, event) {
    if (event != null) {
      event.preventDefault();
    }
    cities.add(city);
    suggestions = [];
    showSuggestions = false;
    newCity = '';
    var placemark = city.placemark;
    placemark['options'].callMethod('set', ['preset', 'twirl#blueIcon']);
    map["geoObjects"].callMethod("add", [placemark]);
    if (autoUpdate) {
      calc();
    }
  }

  void exclude(int index) {
    exclusions.add(result.path[index]);
    if (autoUpdate) {
      calc();
    }
  }

  void include(int index) {
    exclusions.removeAt(index);
    if (autoUpdate) {
      calc();
    }
  }

  void updateUrl() {
    if (result == null) {
      window.location.hash = '#';
    }
    List data = [
      firstCity
    ];
    int length = result.path.length - (firstCity == lastCity ? 1 : 0);

    for (var i = 0; i < length; ++i) {
      data.add(result.path[i][1]);
    }
    if (firstCity == lastCity) {
      data.add(true);
    }

    List<int> bytes = UTF8.encode(JSON.encode(data));
    List<int> bz2 = new BZip2Encoder().encode(bytes);

    var base64 = CryptoUtils.bytesToBase64(bz2);
    if (History.supportsState) {
      window.history.replaceState(null, document.title, window.location.pathname + "#" + base64);
    } else {
      window.location.hash = '#' + base64;
    }
  }

  bool _calcInProgress = false;

  void calc() {
    if (_calcInProgress) {
      return;
    }
    if (cities.length == 0) {
      if (route != null) {
        map["geoObjects"].callMethod("remove", [route]);
      }

      bool pathExcluded = false;
      exclusions.forEach((List<City> element) {
        if (element[0] == firstCity && element[1] == lastCity ||
          element[1] == firstCity && element[0] == lastCity) {
          pathExcluded = true;
        }
      });

      if (pathExcluded) {
        result = null;
        updateUrl();
        return;
      }

      result = new Path(<List<City>>[<City>[firstCity, lastCity]], firstCity.distanceTo(lastCity));

      var coords = [[firstCity.lat, firstCity.lon], [lastCity.lat, lastCity.lon]];

      var lineString = new JsObject(context['ymaps']['geometry']['LineString'], [
          new JsObject.jsify(coords),
          new JsObject.jsify({
              "coordRendering": 'shortestPath',
              "geodesic": true
          })
      ]);
      route = new JsObject(context['ymaps']['GeoObject'], [
          new JsObject.jsify({ "geometry": lineString })
      ]);

      map["geoObjects"].callMethod("add", [route]);
      if (firstCity != lastCity) {
        map.callMethod("setBounds", [new JsObject.jsify(
            [[min(firstCity.lat, lastCity.lat), min(firstCity.lon, lastCity.lon)],
            [max(firstCity.lat, lastCity.lat), max(firstCity.lon, lastCity.lon)]]
        )]);
      }
      updateUrl();
      return;
    }

    // Prepare tables
    List<City> index = <City>[firstCity];
    index.addAll(cities);
    index.add(lastCity);

    int iLen = index.length;

    List<List<double>> c = <List<double>>[];
    for (int i = 0; i < iLen; ++i) {
      c.add(new List<double>.filled(iLen, TSPAlgorithm.inf));
    }

    for (int i = 0; i < cities.length; ++i) {
      c[0][i+1] = firstCity.distanceTo(cities[i]);
      c[i+1][iLen - 1] = lastCity.distanceTo(cities[i]);
    }
    c[iLen - 1][0] = 0.0; // Just for convenience

    for (int i = 0; i < cities.length - 1; ++i) {
      for (int j = i + 1; j < cities.length; ++j) {
        double distance = cities[i].distanceTo(cities[j]);
        c[i + 1][j + 1] = distance;
        c[j + 1][i + 1] = distance;
      }
    }

    // Apply exclusions
    if (exclusions != null) {
      for (List<City> exclusion in exclusions) {
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

    Element button = query('.route-box .refresh-button');
    Element icon = query('.route-box .refresh-button i');
    button.classes.add('disabled');
    icon.classes.add('in-progress');
    _calcInProgress = true;
    (new AntColonyOptimization()).solve(c).then((ar) {
      _updateResult(ar);

      updateUrl();
      icon.classes.remove('in-progress');
      button.classes.remove('disabled');
      _calcInProgress = false;
    });
  }

  City _getCityByIndex(int index) {
    if (index == 0) {
      return firstCity;
    } else if (index == cities.length + 1) {
      return lastCity;
    } else {
      return cities[index - 1];
    }
  }

  void _updateResult(AlgorithmResult ar) {
    if (ar.distance > TSPAlgorithm.inf) { // Path not found
      result = null;
    } else {
      List<List<City>> path = <List<City>>[];
      for (int i = 0; i < ar.points.length - 1; ++i) {
        path.add(<City>[_getCityByIndex(ar.points[i]), _getCityByIndex(ar.points[i + 1])]);
      }

      result = new Path(path, ar.distance);
    }

    if (route != null) {
      map["geoObjects"].callMethod("remove", [route]);
      route = null;
    }

    var coords = [];
    double minLat = firstCity.lat;
    double maxLat = firstCity.lat;
    double minLon = firstCity.lon;
    double maxLon = firstCity.lon;
    coords.add([firstCity.lat, firstCity.lon]);

    for (int i = 1; i < ar.points.length - 1; ++i) {
      City city = cities[ar.points[i] - 1];
      minLat = min(minLat, city.lat);
      minLon = min(minLon, city.lon);
      maxLat = max(maxLat, city.lat);
      maxLon = max(maxLon, city.lon);
      coords.add([city.lat, city.lon]);
    }

    minLat = min(minLat, lastCity.lat);
    minLon = min(minLon, lastCity.lon);
    maxLat = max(maxLat, lastCity.lat);
    maxLon = max(maxLon, lastCity.lon);
    coords.add([lastCity.lat, lastCity.lon]);

    if (ar.distance < TSPAlgorithm.inf) {
      var lineString = new JsObject(context['ymaps']['geometry']['LineString'], [
          new JsObject.jsify(coords),
          new JsObject.jsify({
              "coordRendering": 'shortestPath',
              "geodesic": true
          })
      ]);
      route = new JsObject(context['ymaps']['GeoObject'], [
          new JsObject.jsify({ "geometry": lineString })
      ]);

      map["geoObjects"].callMethod("add", [route]);
    }
    if (minLat == maxLat && minLon == maxLon) {
      map.callMethod("setCenter", [new JsObject.jsify([minLat, minLon]), 8]);
    } else {
      map.callMethod("setBounds", [new JsObject.jsify([[minLat, minLon], [maxLat, maxLon]])]);
    }
  }
}

class City {
  String _name;
  String _fullName;
  double _lat;
  double _lon;

  City(this._name, this._lat, this._lon, [this._fullName = '']);

  City.fromJson(String json) {
    List<String> values = json.split("|");
    _name = values[0];
    _lat = double.parse(values[1]);
    _lon = double.parse(values[2]);
    _fullName = '';
  }

  String get name => _name;
  String get fullName => _fullName;
  double get lat => _lat;
  double get lon => _lon;

  JsObject _placemark;

  JsObject get placemark {
    if (_placemark == null) {
      _placemark = new JsObject(context['ymaps']['Placemark'], [
          new JsObject.jsify([_lat, _lon]),
          new JsObject.jsify({
              "balloonContent": _name
          })
      ]);
    }

    return _placemark;
  }

  static const double R = 6371.0;
  static const double D2R = PI / 180;

  double distanceTo(City city2) {
    double dLat = (city2.lat - lat) * D2R;
    double dLon = (city2.lon - lon) * D2R;

    double a = pow(sin(dLat / 2.0), 2) + cos(lat * D2R) * cos(city2.lat * D2R) * pow(sin(dLon/2.0), 2);
    double c = 2 * atan2(sqrt(a), sqrt(1-a));
    return R * c;
  }

  int distanceToRound(City city2) {
    return distanceTo(city2).round();
  }

  String toString() => _fullName == '' ? "${_name} (${_lat}, ${_lon})" : "${_fullName} (${_lat}, ${_lon})";

  String toJson() => "${_name}|${_lat}|${_lon}";
}

class Path {
  List<List<City>> path;
  double distance;

  int get distanceRound => distance.round();

  Path(this.path, this.distance);
}

class Geocoder {
  static Future<List<City>> geocode(String request) {
    Completer<List<City>> completer = new Completer<List<City>>();

    HttpRequest.getString('http://geocode-maps.yandex.ru/1.x/?format=json&geocode=' + request)
    .then((String contents) {
      JsonDecoder decoder = new JsonDecoder(null);
      var data = decoder.convert(contents);

      List<City> result = <City>[];
      for (var item in data['response']['GeoObjectCollection']['featureMember']) {
        if (item['GeoObject']['metaDataProperty']['GeocoderMetaData']['kind'] != 'locality') {
          continue;
        }
        List<String> point = item['GeoObject']['Point']['pos'].split(' ');
        result.add(new City(item['GeoObject']['name'],
        double.parse(point[1]), double.parse(point[0]),
        item['GeoObject']['metaDataProperty']['GeocoderMetaData']['text']));
      }

      completer.complete(result);
    })
    .catchError(completer.completeError);

    return completer.future;
  }
}
