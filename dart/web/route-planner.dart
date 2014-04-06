import 'dart:js';

import '../lib/app.dart';
import '../lib/route-planner.dart';

void main() {
  // Initializing site function
  App app = new App();
  app.init();

  // Initializing route planner
  context['ymaps'].callMethod('ready', [() => new CitiesListController()]);
}