part of routePlanner;

class MapWidget {
  // TODO move placemarks from City class here.
  Model _model;
  JsObject map;
  JsObject route;

  MapWidget(this._model, Element el) {
    JsObject options = new JsObject.jsify({
        "behaviors": ["drag", "scrollZoom", "dblClickZoom", "multiTouch", "rightMouseButtonMagnifier"],
        "center": [53.906077, 27.554914],
        "zoom": 8
    });
    map = new JsObject(context['ymaps']['Map'], [el, options]);
    map['controls'].callMethod('add', ['zoomControl']);

    _model.onEndCityChange.listen(endCityChange);
    _model.onCityAdd.listen(cityAdd);
    _model.onCityRemove.listen(cityRemove);
    _model.onClearCities.listen(clearCities);
    _model.onAddManyCities.listen(cityAddAll);
    _model.onPathChange.listen(pathChange);
  }

  void endCityChange(ChangeEvent<City> event) {
    if (event.oldValue != null) {
      map["geoObjects"].callMethod("remove", [event.oldValue.placemark]);
    }
    JsObject placemark = event.newValue.placemark;
    placemark['options'].callMethod('set', ['preset', 'twirl#blueDotIcon']);
    map["geoObjects"].callMethod("add", [placemark]);
  }

  void cityAdd(City city) {
    JsObject placemark = city.placemark;
    placemark['options'].callMethod('set', ['preset', 'twirl#blueIcon']);
    map["geoObjects"].callMethod("add", [placemark]);
  }

  void cityAddAll(List<City> cities) {
    for (City city in cities) {
      JsObject placemark = city.placemark;
      placemark['options'].callMethod('set', ['preset', 'twirl#blueIcon']);
      map["geoObjects"].callMethod("add", [placemark]);
    }
  }

  void cityRemove(City city) {
    map["geoObjects"].callMethod("remove", [city.placemark]);
  }

  void clearCities(List<City> cities) {
    for (City city in cities) {
      map["geoObjects"].callMethod("remove", [city.placemark]);
    }
  }

  void pathChange(ChangeEvent<Path> event) {
    if (route != null) {
      map["geoObjects"].callMethod("remove", [route]);
    }

    Path path = event.newValue;
    if (path == null) {
      route = null;
      return;
    }

    var firstCity = path.path[0][0];
    List<List<double>> coords = [];
    double minLat = firstCity.lat;
    double maxLat = firstCity.lat;
    double minLon = firstCity.lon;
    double maxLon = firstCity.lon;
    coords.add([firstCity.lat, firstCity.lon]);

    for (int i = 0; i < path.path.length; ++i) {
      City city = path.path[i][1];
      minLat = min(minLat, city.lat);
      minLon = min(minLon, city.lon);
      maxLat = max(maxLat, city.lat);
      maxLon = max(maxLon, city.lon);
      coords.add([city.lat, city.lon]);
    }

    if (path.distance < TSPAlgorithm.inf) {
      JsObject lineString = new JsObject(context['ymaps']['geometry']['LineString'], [
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

  void reset() {
    if (route != null) {
      map["geoObjects"].callMethod("remove", [route]);
    }
    for (City city in _model.cities) {
      map["geoObjects"].callMethod("remove", [city.placemark]);
    }
    map["geoObjects"].callMethod("remove", [_model.firstCity.placemark]);
    map["geoObjects"].callMethod("remove", [_model.lastCity.placemark]);
  }
}
