import 'dart:js';

import 'package:dikmax.name/app.dart';
import 'package:dikmax.name/route-planner.dart';

void main() {
  // Initializing site function
  App app = new App();
  app.init();

  // Initializing route planner
  context['ymaps'].callMethod('ready', [() => new RoutePlannerApplication()]);
}