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
  static final int antsCount = 200;
  static final int wavesCount = 500;

  AlgorithmResult solve(List<List<double>> dist) {
    Stopwatch stopwatch = new Stopwatch()..start();
    _dist = dist;
    _initializeData();

    int wave = 0;
    while (stopwatch.elapsedMilliseconds < 500) {
      ++wave;
      _constructSolutions();
      _localSearch();
      _updateStatistics();
      _updatePheromoneTrails();
      _checkBestSolution();
    }

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
    for (int i = 0; i < antsCount; ++i) {
      _ants[i].reset(_size);
    }
    for (int step = 1; step < _size - 1; ++step) {
      for (int k = 0; k < antsCount; ++k) {
        _asDecisionRule(_ants[k], step);
      }
    }
    for (int k = 0; k < antsCount; ++k) {
      int last = _ants[k].tour.last;
      _ants[k].tour.add(_size - 1);
      _ants[k].tourLength += _dist[last][_size - 1];
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

  void _localSearch() {}

  void _updateStatistics() {}

  void _updatePheromoneTrails() {
    _evaporate();
    for (int k = 0; k < antsCount; ++k) {
      _depositPheromone(_ants[k]);
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

  void _checkBestSolution() {
    for (int i = 0; i < antsCount; ++i) {
      if (bestTour == null || _ants[i].tourLength < bestLength) {
        bestTour = _ants[i].tour;
        bestLength = _ants[i].tourLength;
      }
    }
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