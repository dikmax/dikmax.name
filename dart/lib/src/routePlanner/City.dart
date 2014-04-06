part of routePlanner;

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

