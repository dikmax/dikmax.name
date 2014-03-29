part of tsp;

// TODO remove unnecessary object creation. Reuse objects.
class AntColonyOptimization extends TSPAlgorithm {
  int _size;
  List<List<double>> _dist;   // Distances matrix
  List<List<int>> _nnList;    // Nearest neighbor list
  List<List<double>> _pheromone;
  List<List<double>> _choiceInfo;
  List<Ant> _ants;
  List<int> bestTour;
  double bestLength;
  Random random = new Random();

  static final double alpha = 0.1; // The importance of the previous trails
  static final double beta = 2.0;  // The importance of the durations
  static final double rho = 0.5;   // Pheromone evaporation speed
  static final double rho1 = 1 - rho;
  static final int antsCount = 20;
  static final int wavesCount = 50; // TODO
  static final int stillPeriod = 50;

  AlgorithmResult solve(List<List<double>> dist) {
    Stopwatch stopwatch = new Stopwatch()..start();
    _dist = dist;
    _initializeData();

    int wave = 0;
    int bestUpdatedIterationsAgo = 0;
    while (stopwatch.elapsedMilliseconds < 500 && bestUpdatedIterationsAgo < stillPeriod) {
      ++bestUpdatedIterationsAgo;
      ++wave;
      _constructSolutions();
      _localSearch();
      _updateStatistics();
      _updatePheromoneTrails();

      bool bestUpdated = false;
      for (Ant ant in _ants) {
        if (bestTour == null || ant.tourLength < bestLength) {
          bestTour = ant.tour;
          bestLength = ant.tourLength;
          bestUpdated = true;
          print("Wave: $wave, length: $bestLength");
        }
      }
      if (bestUpdated) {
        bestUpdatedIterationsAgo = 0;
      }
    }

    if (bestUpdatedIterationsAgo == stillPeriod) {
      print("Still!");
    }

    _3opt(bestTour, bestLength);

    print("Waves: ${wave}");

    return new AlgorithmResult(bestTour, bestLength);
  }

  void _initializeData() {
    _size = _dist.length;
    _pheromone = <List<double>>[];
    _choiceInfo = <List<double>>[];
    for (int i = 0; i < _size; ++i) {
      _pheromone.add(new List<double>.filled(_size, 1.0));
      _choiceInfo.add(new List<double>.filled(_size, 0.0));
    }
    _selectionProbability = new List<double>.filled(_size, 0.0);
    _computeNearestNeighborLists();
    _computeChoiceInformation();
    _initializeAnts();
  }

  void _computeNearestNeighborLists() {
    _nnList = <List<int>>[];
    for (int i = 0; i < _size; ++i) {
      List<Tuple<int, double>> list = <Tuple<int, double>>[];
      for (int j = 0; j < _size; ++j) {
        list.add(new Tuple(j, _dist[i][j]));
      }

      list.sort((a, b) => Comparable.compare(a.item2, b.item2));
      List<int> nnList = <int>[];
      for (int j = 0; j < _size; ++j) {
        nnList.add(list[j].item1);
      }
      _nnList.add(nnList);
    }
  }

  void _computeChoiceInformation() {
    for (int i = 0; i < _size; ++i) {
      for (int j = 0; j < _size; ++j) {
        _choiceInfo[i][j] = pow(_pheromone[i][j], alpha) * pow(_dist[i][j], -beta);
      }
    }
  }

  void _initializeAnts() {
    _ants = <Ant>[];
    for (int i = 0; i < antsCount; ++i) {
      _ants.add(new Ant());
    }
  }

  void _constructSolutions() {
    for (Ant ant in _ants) {
      ant.reset(_size);
    }
    for (int step = 1; step < _size - 1; ++step) {
      for (Ant ant in _ants) {
        _asDecisionRule(ant, step);
      }
    }
    for (Ant ant in _ants) {
      int last = ant.tour.last;
      ant.tour.add(_size - 1);
      ant.tourLength += _dist[last][_size - 1];
    }
  }

  List<double> _selectionProbability;

  void _asDecisionRule(Ant ant, int step) {
    int last = ant.tour[step - 1];
    double probabilitiesSum = 0.0;
    for (int i = 1; i < _size - 1; ++i) {
      if (ant.visited[i]) {
        _selectionProbability[i] = 0.0;
      } else {
        _selectionProbability[i] = _choiceInfo[last][i];
        probabilitiesSum += _selectionProbability[i];
      }
    }

    double r = random.nextDouble() * probabilitiesSum;
    int i = 1;
    double p = _selectionProbability[i];
    while (p < r) {
      ++i;
      p += _selectionProbability[i];
    }
    ant.tour.add(i);
    ant.tourLength += _dist[last][i];
    ant.visited[i] = true;
  }

  //bool _2optCheck(Ant)
  void _localSearch() {
    // Performing 2-opt local search
    for (Ant ant in _ants) {
      bool changed = true;

      if (changed) {
        changed = false;
        for (int i = 1; !changed && i < _size - 2; ++i) {
          int at_i_1 = ant.tour[i - 1];
          int at_i = ant.tour[i];
          int at_i1 = ant.tour[i + 1];
          double adjDistI = -_dist[at_i_1][at_i] - _dist[at_i][at_i1];
          // Checking two neighbor nodes
          if (i < _size - 2) {
            int at_i2 = ant.tour[i + 2];
            double adjDist = adjDistI - _dist[at_i1][at_i2]
                + _dist[at_i_1][at_i1] + _dist[at_i1][at_i]
                + _dist[at_i][at_i2];
            if (adjDist < 0) {
              ant.tour[i] = at_i1;
              ant.tour[i + 1] = at_i;
              ant.tourLength += adjDist;
              changed = true;
            }
          }
          // Checking other nodes
          for (int j = i + 2; !changed && j < _size - 1; ++j) {
            int at_j_1 = ant.tour[j - 1];
            int at_j = ant.tour[j];
            int at_j1 = ant.tour[j + 1];
            double adjDist = adjDistI - _dist[at_j_1][at_j] - _dist[at_j][at_j1]
                + _dist[at_i_1][at_j] + _dist[at_j][at_i1]
                + _dist[at_j_1][at_i] + _dist[at_i][at_j1];
            if (adjDist < 0) {
              ant.tour[i] = at_j;
              ant.tour[j] = at_i;
              ant.tourLength += adjDist;
              changed = true;
            }
          }
        }
      }
    }
  }

  void _updateStatistics() {}

  void _updatePheromoneTrails() {
    _evaporate();
    for (Ant ant in _ants) {
      _depositPheromone(ant);
    }
    _computeChoiceInformation();
  }

  void _evaporate() {
    for (int i = 0; i < _size - 1; ++i) {
      for (int j = i; j < _size; ++j ) {
        _pheromone[i][j] *= rho1;
        _pheromone[j][i] = _pheromone[i][j];
      }
    }
  }

  void _depositPheromone(Ant ant) {
    double delta = 1/ant.tourLength;
    for (int i = 0; i < ant.tour.length - 1; ++i) {
      int from = ant.tour[i];
      int to = ant.tour[i + 1];
      _pheromone[from][to] += delta;
      _pheromone[to][from] = _pheromone[from][to];
    }
  }

  void _3opt(List<int> tour, double distance) {
    // TODO implement
  }
}

class Tuple<T1, T2> {
  final T1 item1;
  final T2 item2;

  const Tuple(this.item1, this.item2);
}

class Ant {
  double tourLength;
  List<int> tour;
  List<bool> visited;

  Ant() {
    tourLength = 0.0;
  }

  void reset(int size) {
    tourLength = 0.0;
    visited = new List<bool>.filled(size, false);
    visited[0] = true;
    tour = <int>[0];
  }
}