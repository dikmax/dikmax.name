part of tsp;

// TODO remove unnecessary object creation. Reuse objects.
class AntColonyOptimization extends TSPAlgorithm {
  int _size;
  List<List<double>> _dist;   // Distances matrix
  List<List<int>> _nnList;    // Nearest neighbor list
  List<List<double>> _pheromone;
  List<List<double>> _choiceInfo;
  List<Ant> _ants;
  List<int> _bestTour;
  double _bestLength;
  Random _random = new Random();
  int _timeout;

  static final double alpha = 0.1; // The importance of the previous trails
  static final double beta = 2.0;  // The importance of the durations
  static final double rho = 0.5;   // Pheromone evaporation speed
  static final double rho1 = 1 - rho;
  static final int antsCount = 50;
  static final int stillPeriod = 100;
  static final double epsilon = 0.01;

  Future<AlgorithmResult> solve(List<List<double>> dist) {
    Completer<AlgorithmResult> c = new Completer<AlgorithmResult>();

    Stopwatch stopwatch = new Stopwatch()..start();
    _dist = dist;
    _timeout = dist.length * 100;
    _initializeData();

    int wave = 0;
    int bestUpdatedIterationsAgo = 0;

    void process() {
      if (stopwatch.elapsedMilliseconds < _timeout && bestUpdatedIterationsAgo < stillPeriod) {
        ++bestUpdatedIterationsAgo;
        ++wave;
        _constructSolutions();
        _localSearch();
        _updatePheromoneTrails();

        bool bestUpdated = false;
        for (Ant ant in _ants) {
          if (_bestTour == null || ant.tourLength < _bestLength) {
            _bestTour = ant.tour;
            _bestLength = ant.tourLength;
            bestUpdated = true;
          }
        }
        if (bestUpdated) {
          bestUpdatedIterationsAgo = 0;
        }
        Timer.run(process);
      } else {

        print("Waves: ${wave}, bestLength: $_bestLength");

        Timer.run(() {
          _3opt();
          print("BestLength: $_bestLength");
          c.complete(new AlgorithmResult(_bestTour, _bestLength));
        });
      }
    }

    Timer.run(process);

    return c.future;
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

    double r = _random.nextDouble() * probabilitiesSum;
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
        int i = 1;
        for (; !changed && i < _size - 2; ++i) {
          int at_i_1 = ant.tour[i - 1];
          int at_i = ant.tour[i];
          double adjDistI = -_dist[at_i_1][at_i];

          for (int j = i + 1; !changed && j < _size - 1; ++j) {
            int at_j = ant.tour[j];
            int at_j1 = ant.tour[j + 1];
            double adjDist = adjDistI - _dist[at_j][at_j1]
                + _dist[at_i_1][at_j] + _dist[at_i][at_j1];
            if (adjDist < 0) {
              // Reverse detached part
              int l = ((j - i + 1) / 2).floor();
              for (int k = 0; k < l; ++k) {
                int tmp = ant.tour[i+k];
                ant.tour[i+k] = ant.tour[j-k];
                ant.tour[j-k] = tmp;
              }
              ant.tourLength += adjDist;
              --i;
              changed = true;
            }
          }
        }
      }
    }
  }

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

  void _3opt() {
    List<double> forward = new List<double>.filled(_size, 0.0);
    List<double> backward = new List<double>.filled(_size, 0.0);
    List<int> curr = new List<int>.from(_bestTour);
    double best;
    bool changed = true;

    void updateCosts () {
      for (int i = 1; i < _size; ++i) {
        forward[i] = forward[i - 1] + _dist[curr[i - 1]][curr[i]];
      }
      best = forward.last;
      for (int i = _size - 2; i >= 0; --i) {
        backward[i] = backward[i + 1] + _dist[curr[i + 1]][curr[i]];
      }
    }

    double getCost(a, b) {
      if (a <= b) {
        return forward[b] - forward[a];
      } else {
        return backward[b] - backward[a];
      }
    }

    double checkCost(a, b, c, d, e, f) => getCost(0, a) + _dist[curr[a]][curr[b]] +
        getCost(b, c) + _dist[curr[c]][curr[d]] + getCost(d, e) + _dist[curr[e]][curr[f]] + getCost(f, _size - 1);

    void update(a, b, c, d, e, f) {
      changed = true;
      List<int> next = new List<int>.from(curr);

      int offset = a + 1;
      next[offset++] = curr[b];
      if (b < c) {
        for (int i = b + 1; i <= c; ++i) {
          next[offset++] = curr[i];
        }
      } else {
        for (int i = b - 1; i >= c; --i) {
          next[offset++] = curr[i];
        }
      }
      next[offset++] = curr[d];
      if (d < e) {
        for (int i = d + 1; i <= e; ++i) {
          next[offset++] = curr[i];
        }
      } else {
        for (int i = d - 1; i >= e; --i) {
          next[offset++] = curr[i];
        }
      }
      next[offset++] = curr[f];
      curr = next;

      updateCosts();
    }

    updateCosts();
    while (changed) {
      changed = false;
      for (int i = 0; i < _size - 3; ++i) {
        for (int j = i + 1; j < _size - 2; ++j) {
          for (int k  = j + 1; k < _size - 1; ++k) {
            if (checkCost(i, i + 1, j, k, j + 1, k + 1) - best < -epsilon) {
              update(i, i+1, j, k, j+1, k+1);
            }
            if (checkCost(i, j, i+1, j+1, k, k+1) - best < -epsilon) {
              update(i, j, i+1, j+1, k, k+1);
            }
            if (checkCost(i, j, i+1, k, j+1, k+1) - best < -epsilon) {
              update(i, j, i+1, k, j+1, k+1);
            }
            if (checkCost(i, j+1, k, i+1, j, k+1) - best < -epsilon) {
              update(i, j+1, k, i+1, j, k+1);
            }
            if (checkCost(i, j+1, k, j, i+1, k+1) - best < -epsilon) {
              update(i, j+1, k, j, i+1, k+1);
            }
            if (checkCost(i, k, j+1, i+1, j, k+1) - best < -epsilon) {
              update(i, k, j+1, i+1, j, k+1);
            }
            if (checkCost(i, k, j+1, j, i+1, k+1) - best < -epsilon) {
              update(i, k, j+1, j, i+1, k+1);
            }
          }
        }
      }
    };

    _bestTour = curr;
    _bestLength = best;
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