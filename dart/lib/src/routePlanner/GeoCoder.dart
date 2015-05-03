part of routePlanner;

class GeoCoder {
  static Future<List<City>> geoCode(String request) async {
    String contents = await HttpRequest.getString('http://geocode-maps.yandex.ru/1.x/?format=json&geocode=' + request);
    JsonDecoder decoder = new JsonDecoder(null);
    Map data = decoder.convert(contents);

    List<City> result = <City>[];
    for (Map item in data['response']['GeoObjectCollection']['featureMember']) {
      if (item['GeoObject']['metaDataProperty']['GeocoderMetaData']['kind'] != 'locality') {
        continue;
      }
      List<String> point = item['GeoObject']['Point']['pos'].split(' ');
      result.add(new City(item['GeoObject']['name'],
      double.parse(point[1]), double.parse(point[0]),
      item['GeoObject']['metaDataProperty']['GeocoderMetaData']['text']));
    }

    return result;
  }
}
