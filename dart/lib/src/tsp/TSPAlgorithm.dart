part of tsp;

abstract class TSPAlgorithm {
  static final double inf = 1e8;

  Future<AlgorithmResult> solve(List<List<double>> c);
}

class AlgorithmResult {
  List<int> points;
  double distance;

  AlgorithmResult(this.points, this.distance);
}