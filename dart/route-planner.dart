import 'dart:async';
import 'dart:collection';
import 'dart:convert';
import 'dart:html';
import 'dart:js';
import 'dart:math';

// Temporary, please follow https://github.com/angular/angular.dart/issues/476
@MirrorsUsed(targets: const ['City', 'Path', 'double'], override: '*')
import 'dart:mirrors';

import 'package:angular/angular.dart';
import 'app.dart';

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
  var previewPlacemark;
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

  void addCity(city) {
    cities.add(city);
    suggestions = [];
    showSuggestions = false;
    newCity = '';
    if (previewPlacemark != null) {
      map["geoObjects"].callMethod("remove", [previewPlacemark]);
      previewPlacemark = null;
    }
    var placemark = city.placemark;
    placemark['options'].callMethod('set', ['preset', 'twirl#blueIcon']);
    map["geoObjects"].callMethod("add", [placemark]);
    if (autoUpdate) {
      calc();
    }
  }

  void showCity(city) {
    if (previewPlacemark != null) {
      map["geoObjects"].callMethod("remove", [previewPlacemark]);
    }
    previewPlacemark = city.placemark;
    previewPlacemark['options'].callMethod('set', ['preset', 'twirl#whiteIcon']);
    map["geoObjects"].callMethod("add", [previewPlacemark]);
    map.callMethod("setCenter", [
      new JsObject.jsify([city.lat, city.lon])
    ]);
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

  static final double inf = 1e8;

  // Bounds and branches
  void calc() {
    Stopwatch stopwatch = new Stopwatch()..start();

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
      return;
    }

    // Prepare tables
    List<City> index = <City>[firstCity];
    index.addAll(cities);
    index.add(lastCity);

    int iLen = index.length;

    List<List<double>> c = <List<double>>[];
    for (int i = 0; i < iLen; ++i) {
      c.add(new List<double>.filled(iLen, inf));
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
            c[index0][index1] = inf;
            c[index1][index0] = inf;
          }
        }
      }
    }

    // Algorithm goes here

    // Hungry search for initial upper estimate
    double f_x_0 = 0.0;
    List<int> x_0 = <int>[0];
    while (x_0.length < iLen - 1) {
      double min;
      int next;
      int last = x_0.last;
      for (int i = 1; i < iLen - 1; ++i) {
        if (x_0.contains(i)) {
          continue;
        }
        if (min == null || c[last][i] < min) {
          min = c[last][i];
          next = i;
        }
      }
      x_0.add(next);
      f_x_0 += min;
    }
    f_x_0 += c[x_0.last][iLen - 1];
    x_0.add(iLen - 1);

    void iteration(List<int> I, Set<int> J) {
      Set<int> Iset = new Set<int>.from(I);
      int Ilast = I.last;
      Iset.remove(Ilast);

      List<int> Isupp = <int>[]; // All - I
      int firstAvailable;

      for (int i = 0; i < iLen; ++i) {
        if (!Iset.contains(i)) {
          Isupp.add(i);
          if (firstAvailable == null && i != Ilast && !J.contains(i)) {
            firstAvailable = i;
          }
        }
      }

      if (firstAvailable == null) {
        return;
      }

      List<double> alpha = new List<double>.filled(iLen, inf);
      for (int i in Isupp) {
        double min = inf;
        for (int j in Isupp) {
          if (j == Ilast || i == Ilast && J.contains(j)) {
            continue;
          }
          if (c[i][j] < min) {
            min = c[i][j];
          }
        }

        alpha[i] = min;
      }

      List<double> beta = new List<double>.filled(iLen, inf);
      for (int i in Isupp) {
        if (i == Ilast) {
          continue;
        }
        double min = inf;
        for (int j in Isupp) {
          if (j == Ilast && J.contains(i)) {
            continue;
          }
          double val = c[j][i] - alpha[j];
          if (val < min) {
            min = val;
          }
        }

        beta[i] = min;
      }

      // Low estimate
      double H = 0.0;
      double routeLength = 0.0;
      for (int i = 0; i < I.length - 1; ++i) {
        routeLength += c[I[i]][I[i+1]];
      }
      H += routeLength;
      for (int i in Isupp) {
        if (i >= iLen - 1) {
          continue;
        }
        H += alpha[i];
      }
      for (int i in Isupp) {
        if (i == Ilast) {
          continue;
        }
        H += beta[i];
      }

      if (H >= f_x_0) {
        return;
      }

      // High estimate
      double f = routeLength + c[Ilast][firstAvailable];
      int prev = firstAvailable;
      int skip = firstAvailable;
      List<int> x = new List<int>.from(I);
      x.add(firstAvailable);

      for (int i in Isupp) {
        if (i != Ilast && i != firstAvailable) {
          f += c[prev][i];
          x.add(i);
          prev = i;
        }
      }

      if (f < f_x_0) {
        f_x_0 = f;
        x_0 = x;
      }

      if (H < f_x_0) {
        // Use firstAvailable as next stop
        I.add(firstAvailable);
        iteration(I, new Set<int>());
        I.removeLast();

        // Don't firstAvailable as next stop
        J.add(firstAvailable);
        iteration(I, J);
      }
    }

    iteration([0], new Set<int>());
    //print("Elapsed: ${stopwatch.elapsedMilliseconds}");

    if (f_x_0 > inf) { // Path not found
      result = null;
    } else {
      List<List<City>> path = <List<City>>[];
      for (int i = 0; i < x_0.length - 1; ++i) {
        path.add(<City>[index[x_0[i]], index[x_0[i + 1]]]);
      }

      result = new Path(path, f_x_0);
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

    for (int i in x_0) {
      City city = index[i];
      minLat = min(minLat, city.lat);
      minLon = min(minLon, city.lon);
      maxLat = max(maxLat, city.lat);
      maxLon = max(maxLon, city.lon);
      coords.add([city.lat, city.lon]);
    }

    if (f_x_0 < inf) {
      var lineString = new JsObject(context['ymaps']['geometry']['LineString'],
      [new JsObject.jsify(coords)]
      );
      route = new JsObject(context['ymaps']['GeoObject'], [
          new JsObject.jsify({ "geometry": lineString })
      ]);

      map["geoObjects"].callMethod("add", [route]);
    }
    map.callMethod("setBounds", [new JsObject.jsify([[minLat, minLon], [maxLat, maxLon]])]);
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

class AppModule extends Module {
  AppModule() {
    type(CitiesListController);
  }
}

void main() {
  // Initializing site function
  App app = new App();
  app.init();

  // Initializing route planner
  context['ymaps'].callMethod('ready', [() => ngBootstrap(module: new AppModule())]);
}