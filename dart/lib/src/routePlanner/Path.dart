part of routePlanner;

class Path {
  List<List<City>> path;
  double distance;

  int get distanceRound => distance.round();

  Path(this.path, this.distance);
}
