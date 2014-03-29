part of tsp;


class BranchAndBound extends TSPAlgorithm {
  Future<AlgorithmResult> solve(List<List<double>> table) {
    Completer<AlgorithmResult> c = new Completer<AlgorithmResult>();

    c.complete(_solve(table));
    return c.future;
  }

  AlgorithmResult _solve(List<List<double>> c) {
    int iLen = c.length;

    // Hungry search for initial upper estimate
    double f_x_0 = 0.0;
    List<int> x_0 = <int>[0];
    while (x_0.length < iLen - 1) {
      double min;
      int next;
      int last = x_0.last;
      for (int i = 1; i < iLen - 1; ++i) {
        if (x_0.contains(i)) {
          continue;
        }
        if (min == null || c[last][i] < min) {
          min = c[last][i];
          next = i;
        }
      }
      x_0.add(next);
      f_x_0 += min;
    }
    f_x_0 += c[x_0.last][iLen - 1];
    x_0.add(iLen - 1);

    void iteration(List<int> I, Set<int> J) {
      Set<int> Iset = new Set<int>.from(I);
      int Ilast = I.last;
      Iset.remove(Ilast);

      List<int> Isupp = <int>[]; // All - I
      int firstAvailable;

      for (int i = 0; i < iLen; ++i) {
        if (!Iset.contains(i)) {
          Isupp.add(i);
          if (firstAvailable == null && i != Ilast && !J.contains(i)) {
            firstAvailable = i;
          }
        }
      }

      if (firstAvailable == null) {
        return;
      }

      List<double> alpha = new List<double>.filled(iLen, TSPAlgorithm.inf);
      for (int i in Isupp) {
        double min = TSPAlgorithm.inf;
        for (int j in Isupp) {
          if (j == Ilast || i == Ilast && J.contains(j)) {
            continue;
          }
          if (c[i][j] < min) {
            min = c[i][j];
          }
        }

        alpha[i] = min;
      }

      List<double> beta = new List<double>.filled(iLen, TSPAlgorithm.inf);
      for (int i in Isupp) {
        if (i == Ilast) {
          continue;
        }
        double min = TSPAlgorithm.inf;
        for (int j in Isupp) {
          if (j == Ilast && J.contains(i)) {
            continue;
          }
          double val = c[j][i] - alpha[j];
          if (val < min) {
            min = val;
          }
        }

        beta[i] = min;
      }

      // Low estimate
      double H = 0.0;
      double routeLength = 0.0;
      for (int i = 0; i < I.length - 1; ++i) {
        routeLength += c[I[i]][I[i+1]];
      }
      H += routeLength;
      for (int i in Isupp) {
        if (i >= iLen - 1) {
          continue;
        }
        H += alpha[i];
      }
      for (int i in Isupp) {
        if (i == Ilast) {
          continue;
        }
        H += beta[i];
      }

      if (H >= f_x_0) {
        return;
      }

      // High estimate
      double f = routeLength + c[Ilast][firstAvailable];
      int prev = firstAvailable;
      int skip = firstAvailable;
      List<int> x = new List<int>.from(I);
      x.add(firstAvailable);

      for (int i in Isupp) {
        if (i != Ilast && i != firstAvailable) {
          f += c[prev][i];
          x.add(i);
          prev = i;
        }
      }

      if (f < f_x_0) {
        f_x_0 = f;
        x_0 = x;
      }

      if (H < f_x_0) {
        // Use firstAvailable as next stop
        I.add(firstAvailable);
        iteration(I, new Set<int>());
        I.removeLast();

        // Don't firstAvailable as next stop
        J.add(firstAvailable);
        iteration(I, J);
      }
    }

    iteration([0], new Set<int>());
    //print("Elapsed: ${stopwatch.elapsedMilliseconds}");

    return new AlgorithmResult(x_0, f_x_0);
  }
}