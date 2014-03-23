import 'dart:js';

// Temporary, please follow https://github.com/angular/angular.dart/issues/476
@MirrorsUsed(targets: const ['routePlanner'], override: '*')
import 'dart:mirrors';

import 'package:angular/angular.dart';
import '../lib/app.dart';
import '../lib/route-planner.dart';

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