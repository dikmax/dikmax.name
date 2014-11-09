library map;

import 'dart:convert';
import 'dart:html';
import 'dart:js';
import 'dart:math' as math;

class MapApplication {
  static const String DATA_PATH = "/map";
  static const String LAND_COLOR = "#ffffdd";

  // Colors for countries
  static final Map<String, String> COLORS = <String, String>{
      'blue': '#a3cec5',
      'green': '#d3e46f',
      'orange': '#fdc663',
      'pink': '#f3c1d3',
      'purple': '#ceb5cf',
      'red': '#fdaf6b',
      'turquoise': '#aadb78',
      'yellow': '#fae364'
  };

  num width;
  num height;
  JsObject svg;
  JsObject g;
  JsObject map;
  JsObject zoom;
  JsFunction projection;
  JsFunction path;
  Element tooltip;
  Element popover;
  var countries, visited;
  List cities;

  void init() {
    map = context['d3'].callMethod('select', ['.map']);
    width = map.callMethod('property', ['clientWidth']);
    height = width * 0.57;

  zoom = context['d3']['behavior'].callMethod('zoom')
      .callMethod('scaleExtent', [new JsObject.jsify([1, 60])])
      .callMethod('size', [new JsObject.jsify([width, height])])
      .callMethod('on', ["zoom", onZoom]);

    tooltip = new DivElement()
      ..classes.addAll(['country-tooltip', 'hidden']);
    querySelector('.map').append(tooltip);

    popover = querySelector('.popover.info');
    popover.onClick.listen((_) => hidePopOver());

    initBackground();

    HttpRequest.request('${DATA_PATH}/world.json').then((HttpRequest request) {
      var world = JSON.decode(request.responseText);

      HttpRequest.request('${DATA_PATH}/data.json').then((HttpRequest request) {
        var visitedData = JSON.decode(request.responseText);
        initData(world, visitedData);
      }, onError: (_) {
        window.alert("Error loading visited countries data");
        initData(world, {});
      });

    }, onError: (_) => window.alert("Error loading world data."));
  }

  void initData(world, visitedData) {
    visited = visitedData;
    countries = context['topojson']
      .callMethod('feature', [new JsObject.jsify(world), new JsObject.jsify(world['objects']['countries'])])['features'];
    var subunits = context['topojson']
      .callMethod('feature', [new JsObject.jsify(world), new JsObject.jsify(world['objects']['subunits'])])['features'];
    countries.addAll(subunits);
    var regions = context['topojson']
      .callMethod('feature', [new JsObject.jsify(world), new JsObject.jsify(world['objects']['regions'])])['features'];
    countries.addAll(regions);
    cities = [];

    visited.forEach((_, v) {
      if (v['cities'] != null) {
        cities.addAll(v['cities']);
      }
    });
    initCountries();
    initCities();
    initDefaultZoom();
  }

  void initBackground() {
    // D3 code
    projection = context['d3']['geo']['polyhedron'].callMethod('waterman')
      .callMethod('scale', [0.121841155 * width])
      .callMethod('translate', [new JsObject.jsify([width / 2, height / 2])])
      .callMethod('rotate', [new JsObject.jsify([20, 0])]);

    JsObject graticule = context['d3']['geo'].callMethod('graticule')
      .callMethod('minorStep', [new JsObject.jsify([5, 5])]);

    path = context['d3']['geo'].callMethod('path')
      .callMethod('projection', [projection])
      .callMethod('pointRadius', [1]);

    svg = map.callMethod('append', ['svg'])
      .callMethod('attr', ['width', width])
      .callMethod('attr', ['height', height])
      .callMethod('call', [zoom]);

    JsObject clipPath = svg.callMethod('append', ['defs'])
      .callMethod('append', ['clipPath'])
      .callMethod('attr', ['id', 'outlineClipPath']);
    clipPath.callMethod('append', ['path'])
      .callMethod('datum', [new JsObject.jsify({"type": "Sphere"})])
      .callMethod('attr', ['class', 'graticule outline'])
      .callMethod('attr', ['d', path]);

    g = svg.callMethod('append', ['g'])
      .callMethod('style', ['clip-path', 'url(#outlineClipPath)']);

    g.callMethod('append', ['path'])
      .callMethod('datum', [new JsObject.jsify({"type": "Sphere"})])
      .callMethod('attr', ['class', 'background'])
      .callMethod('attr', ['d', path])
      .callMethod("on", ["click", (d, i, [_]) => hidePopOver()]);

    g.callMethod('append', ['path'])
      .callMethod('datum', [graticule])
      .callMethod('attr', ['class', 'graticule'])
      .callMethod('attr', ['d', path]);

    g.callMethod("style", ["stroke-width", 1])
      .callMethod('attr', ["transform", "translate(0,0)scale(1)"]);

    g.callMethod("append", ['path'])
      .callMethod("datum", [new JsObject.jsify({'type': "Sphere"})])
      .callMethod("attr", ["class", "graticule outline"])
      .callMethod("attr", ["d", path]);
  }

  void initCountries() {
    // Render countries
    JsObject country = g.callMethod("selectAll", [".country"]).callMethod("data", [countries]);
    country.callMethod("enter")
      .callMethod("insert", ["path", ".graticule.outline"])
      .callMethod("attr", ["class", "country"])
      .callMethod("attr", ["d", path])
      .callMethod("attr", ["id", (d, i, [_]) => d['id']])
      .callMethod("style", ["fill", (d, i, [_]) {
        String regionId;
        String countryId = d['id'];
        if (countryId.length > 3) {
          regionId = countryId;
          countryId = countryId.substring(0, 3);
        }
        var c = visited[countryId];
        if (c == null) {
          return LAND_COLOR;
        }

        var color = c['color'];
        if (color == null || COLORS[color] == null) {
          return LAND_COLOR;
        }

        if (regionId != null) {
          if (visited[countryId]['regions'] != null && visited[countryId]['regions'][regionId] != null) {
            return COLORS[color];
          }
          return LAND_COLOR;
        }
        return COLORS[color];
      }]);

    num offsetLeft = map.callMethod("property", ['offsetLeft']) + 5;
    num offsetTop = map.callMethod("property", ['offsetTop']) - 40;

    country.callMethod("on", ["mousemove", (d, i, [_]) {
      var mouse = context['d3'].callMethod("mouse", [svg.callMethod("node")]);

      var regionId;
      var countryId = d['id'];
      if (countryId.length > 3) {
        regionId = countryId;
        countryId = countryId.substring(0, 3);
      }

      String name = getCountryName(countryId, regionId);
      tooltip
        ..classes.remove("hidden")
        ..style.left = "${mouse[0] + offsetLeft}px"
        ..style.top = "${mouse[1] + offsetTop}px"
        ..innerHtml = name;
    }]);
    country.callMethod("on", ["mouseout", (d, i, [_]) => tooltip.classes.add('hidden')]);
    country.callMethod("on", ["click", (d, i, [_]) {
      var regionId;
      var countryId = d['id'];
      if (countryId.length > 3) {
        regionId = countryId;
        countryId = countryId.substring(0, 3);
      }

      if (visited[countryId] == null || visited[countryId]['name'] == null) {
        hidePopOver();
        return;
      }
      String name = visited[countryId]['name'];

      var mouse = context['d3'].callMethod("mouse", [svg.callMethod("node")]);

      List cities = visited[countryId]['cities'];
      cities.sort((a, b) => a['name'].compareTo(b['name']));
      Iterable content = cities.map((e) => "<li>${formatCityString(e)}</li>");

      showPopOver(name, '<ul class="list-unstyled">${content.join('')}</ul>', mouse);
    }]);
  }

  String getCountryName(countryId, regionId) {
    String name = "Неизведанная территория";
    if (visited[countryId] != null && visited[countryId]['name'] != null) {
      name = visited[countryId]['name'];
      if (regionId != null) {
        if (visited[countryId]['regions'] != null && visited[countryId]['regions'][regionId] != null) {
          name = visited[countryId]['regions'][regionId] + ' &mdash; ' + name;
        }
      }
    }
    return name;
  }

  void initCities() {
    var city = g.callMethod("selectAll", [".city"]).callMethod("data", [new JsObject.jsify(cities)]);
    city.callMethod("enter")
      .callMethod("insert", ["path"])
      .callMethod("attr", ["class", "city"])
      .callMethod("attr", ["d", (d, i, [_]) => path.apply([new JsObject.jsify({
        "type": "Point",
        "coordinates": [d['lon'], d['lat']]
      })])]);


    num offsetLeft = map.callMethod("property", ['offsetLeft']) + 5;
    num offsetTop = map.callMethod("property", ['offsetTop']) - 40;

    city.callMethod("on", ["mousemove", (d, i, [_]) {
      var mouse = context['d3'].callMethod("mouse", [svg.callMethod("node")]);

      tooltip
        ..classes.remove("hidden")
        ..style.left = "${mouse[0] + offsetLeft}px"
        ..style.top = "${mouse[1] + offsetTop}px"
        ..innerHtml = d['name'];
    }]);
    city.callMethod("on", ["mouseout", (d, i, [_]) => tooltip.classes.add('hidden')]);
    city.callMethod("on", ["click", (d, i, [_]) {
      var mouse = context['d3'].callMethod("mouse", [svg.callMethod("node")]);

      showPopOver(d['name'], "<p>${formatCityString(d, true)}</p>", mouse);
    }]);
  }

  void initDefaultZoom() {
    num maxX = -1000000.0;
    num maxY = -1000000.0;
    num minX = 1000000.0;
    num minY = 1000000.0;
    cities.forEach((city) {
      var coords = projection.apply([new JsObject.jsify([city['lon'], city['lat']])]);
      maxX = math.max(maxX, coords[0]);
      minX = math.min(minX, coords[0]);
      maxY = math.max(maxY, coords[1]);
      minY = math.min(minY, coords[1]);
    });

    num scale = 0.95 / math.max((maxX - minX) / width, (maxY - minY) / height);

    List<num> translate = [-(minX + maxX) / 2 * scale + width / 2, -(minY + maxY) / 2 * scale + height / 2];

    path = path.callMethod("pointRadius", [math.max(1/4, 1/scale)]);

    var c = g.callMethod("selectAll", ['.city']);
    g.callMethod('transition').callMethod("duration", [5000])
      .callMethod("attr", ["transform", "translate(${translate.join(',')})scale(${scale})"])
      .callMethod("tween", ["side-effects", (_d, _i, [_]) {
        var i = context['d3'].callMethod('interpolateNumber', [1, scale]);
        return (t) {
          var s = i.apply([t]);
          g.callMethod("style", ["stroke-width", 1 / s]);
          path = path.callMethod("pointRadius", [math.max(1/4, 1/s)]);
          c.callMethod("attr", ["d", (d, i, [_]) => path.apply([new JsObject.jsify({
              "type": "Point",
              "coordinates": [d['lon'], d['lat']]
          })])]);
        };
      }]);

    zoom.callMethod("translate", [new JsObject.jsify(translate)]);
    zoom.callMethod("scale", [scale]);
  }

  void onZoom([_0, _1, _2]) {
    JsObject t_ = context['d3']['event']['translate'];
    List<num> t = <num>[t_[0], t_[1]];
    num s = context['d3']['event']['scale'];

    t[0] = math.max(math.min(t[0], 0), width * (1 - s));
    t[1] = math.max(math.min(t[1], 0), height * (1 - s));

    path = path.callMethod("pointRadius", [math.max(1/4, 1/s)]);
    zoom.callMethod("translate", [new JsObject.jsify(t)]);
    g.callMethod("style", ["stroke-width", 1 / s])
      .callMethod("attr", ["transform", "translate(${t.join(',')})scale(${s})"]);
    g.callMethod("selectAll", ['.city']).
      callMethod("attr", ["d", (d, i, [_]) => path.apply([new JsObject.jsify({
        "type": "Point",
        "coordinates": [d['lon'], d['lat']]
      })])]);

    hidePopOver();
  }

  void showPopOver(String title, String content, mouse) {
    num offsetLeft = map.callMethod("property", ['offsetLeft']) + 5;
    num offsetTop = map.callMethod("property", ['offsetTop']) - 40;

    popover.querySelector('.popover-title').innerHtml = title;
    popover.querySelector('.popover-content').innerHtml = content;

    popover.style.display = 'block';
    int width = popover.clientWidth;
    int height = popover.clientHeight;
    int top = mouse[1] + offsetTop - height + 40;
    popover.classes.toggle('top', top >= 60);
    popover.classes.toggle('bottom', top < 60);
    if (top < 60) {
      top = mouse[1] + offsetTop + 40;
    }
    popover.style
      ..left = "${mouse[0] + offsetLeft - width / 2 - 3}px"
      ..top = "${top}px";
  }

  void hidePopOver() {
    popover.style.display = 'none';
  }

  static final List<String> MONTHS = <String>['январь', 'февраль', 'март', 'апрель', 'май', 'июнь', 'июль', 'август',
    'сентябрь', 'октябрь', 'ноябрь', 'декабрь'];

  String formatVisitDate(visit) {
    var result = '';

    int yearFrom = visit['yearFrom'];
    if (yearFrom == null) {
      yearFrom = visit['year'];
      if (yearFrom == null) {
        yearFrom = visit['yearTo'];
      }
    }
    int yearTo = visit['yearTo'];
    if (yearTo == null) {
      yearTo = visit['year'];
      if (yearTo == null) {
        yearTo = visit['yearFrom'];
      }
    }
    int monthFrom = visit['monthFrom'];
    if (monthFrom == null) {
      monthFrom = visit['month'];
      if (monthFrom == null) {
        monthFrom = visit['monthTo'];
      }
    }
    int monthTo = visit['monthTo'];
    if (monthTo == null) {
      monthTo = visit['month'];
      if (monthTo == null) {
        monthTo = visit['monthFrom'];
      }
    }

    if (yearFrom != null) {
      if (yearFrom != yearTo) {
        if (monthFrom != null) {
          result += MONTHS[monthFrom - 1] + ' ';
        }
        result += "${yearFrom} &ndash; ";
        if (monthTo != null) {
          result += MONTHS[monthTo - 1] + ' ';
        }
        result += yearTo.toString();
      } else if (monthFrom != null) {
        result += MONTHS[monthFrom - 1] + ' ';
        if (monthFrom != monthTo) {
          result += '&ndash; ${MONTHS[monthTo - 1]} ';
        }
        result += yearFrom.toString();
      } else {
        result += yearFrom.toString();
      }
    }

    if (result != '') {
      result = '<small>${result}</small>';
    }
    return result;

  }

  String formatCityString(city, [bool skipName = false]) {
    if (city['visits'] != null && city['visits'][0] != null) {
      String date = formatVisitDate(city['visits'][0]);
      String name = (skipName ? '' : city['name']) + (date != '' ? ' ' + date : '');
      if (city['visits'][0]['link'] != null) {
        name = '<a href="${city['visits'][0]['link']}">${name}</a>';
      }
      for (int i = 1; i < city['visits'].length; ++i) {
        var visit = city['visits'][i];
        date = formatVisitDate(visit);
        if (date != '') {
          if (visit['link'] != null) {
            date = '<a href="${visit['link']}">${date}</a>';
          }

          name += ", " + date;
        }
      }

      if (name == '' && skipName) {
        name = '<small>дата неизвестна</small>';
      }
      return name;
    }

    return skipName ? '<small>дата неизвестна</small>' : city['name'];
  }
}
