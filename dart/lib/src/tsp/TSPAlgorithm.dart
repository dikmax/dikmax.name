part of tsp;

abstract class TSPAlgorithm {
  static final double inf = 1e8;

  bool cancelled = false;

  Future<AlgorithmResult> solve(List<List<double>> c);

  void cancel () {
    cancelled = true;
  }
}

class AlgorithmResult {
  List<int> points;
  double distance;

  AlgorithmResult(this.points, this.distance);
}