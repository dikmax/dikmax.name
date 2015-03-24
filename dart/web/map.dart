import 'dart:js';

import '../lib/app.dart';
import '../lib/map.dart';


void main() {
  // Initializing site function
  App app = new App();
  app.init();

  MapApplication map = new MapApplication();
  map.init();
}