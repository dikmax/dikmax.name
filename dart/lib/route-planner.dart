library routePlanner;

import 'dart:async';
import 'dart:convert';
import 'package:crypto/crypto.dart';
import 'dart:html';
import 'dart:js';
import 'dart:math';
import 'package:angular/angular.dart';
import 'tsp.dart';
import 'package:lzma/lzma.dart' as LZMA;

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
  bool autoUpdate = true;
  Path result;
  int elapsed = 0;
  bool loaded = true;

  var map;
  var route;

  CitiesListController() {
    var minsk = new City("Минск", 53.906077, 27.554914);
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
    suggestions = [];
    exclusions = [];

    var options = new JsObject.jsify({
        "behaviors": ["drag", "scrollZoom", "dblClickZoom", "multiTouch", "rightMouseButtonMagnifier"],
        "center": [53.906077, 27.554914],
        "zoom": 8
    });
    map = new JsObject(context['ymaps']['Map'], ["map", options]);
    map['controls'].callMethod('add', ['zoomControl']);

    var placemark = minsk.placemark;
    placemark['options'].callMethod('set', ['preset', 'twirl#blueDotIcon']);
    map["geoObjects"].callMethod("add", [placemark]);

    calc();
  }

  void searchCity() {
    // TODO error handling
    Geocoder.geocode(newCity).then((value) {
      suggestions = value;
      showSuggestions = true;
    });
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
    event.preventDefault();
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
    Map data = {
      "f": firstCity,
      "c": cities
    };
    if (firstCity != lastCity) {
      data["l"] = lastCity;
    }

    List<int> bytes = UTF8.encode(JSON.encode(data));
    var input = new LZMA.InStream(bytes);
    var output = new LZMA.OutStream();
    LZMA.compress(input, output);



    print(bytes.length);
    print(output.data.length);
    print(JSON.encode(data));
    print(output.data);
    var base64 = CryptoUtils.bytesToBase64(output.data);
    print(base64);
  }

  void calc() {
    if (cities.length == 0) {
      result = new Path(<List<City>>[<City>[firstCity, lastCity]], firstCity.distanceTo(lastCity));

      if (route != null) {
        map["geoObjects"].callMethod("remove", [route]);
      }

      var coords = [[firstCity.lat, firstCity.lon], [lastCity.lat, lastCity.lon]];

      var lineString = new JsObject(context['ymaps']['geometry']['LineString'], [new JsObject.jsify(coords)]);
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

    AlgorithmResult ar = (new AntColonyOptimization()).solve(c);

    if (ar.distance > TSPAlgorithm.inf) { // Path not found
      result = null;
    } else {
      List<List<City>> path = <List<City>>[];
      for (int i = 0; i < ar.points.length - 1; ++i) {
        path.add(<City>[index[ar.points[i]], index[ar.points[i + 1]]]);
      }

      result = new Path(path, ar.distance);
    }

    if (route != null) {
      map["geoObjects"].callMethod("remove", [route]);
      route = null;
    }

    var coords = [];
    double minLat = index[0].lat;
    double maxLat = index[0].lat;
    double minLon = index[0].lon;
    double maxLon = index[0].lon;

    for (int i in ar.points) {
      City city = index[i];
      minLat = min(minLat, city.lat);
      minLon = min(minLon, city.lon);
      maxLat = max(maxLat, city.lat);
      maxLon = max(maxLon, city.lon);
      coords.add([city.lat, city.lon]);
    }

    if (ar.distance < TSPAlgorithm.inf) {
      var lineString = new JsObject(context['ymaps']['geometry']['LineString'],
      [new JsObject.jsify(coords)]
      );
      route = new JsObject(context['ymaps']['GeoObject'], [
          new JsObject.jsify({ "geometry": lineString })
      ]);

      map["geoObjects"].callMethod("add", [route]);
    }
    map.callMethod("setBounds", [new JsObject.jsify([[minLat, minLon], [maxLat, maxLon]])]);
    updateUrl();
  }
}

class City {
  String _name;
  String _fullName;
  double _lat;
  double _lon;

  City(this._name, this._lat, this._lon, [this._fullName = '']);

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

  Map toJson() => {
    "n": _name,
    "a": _lat,
    "o": _lon
  };
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
