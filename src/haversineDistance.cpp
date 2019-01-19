#include <math.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double haversine_distance (double olat1, double olon1, double olat2, double olon2, double delta_lat = -1, double delta_lon = -1, bool unitless = false) {
  // double pi = 3.1415926535897;
  double lat1 = olat1 * M_PI / 180 ;
  double lat2 = olat2 * M_PI / 180 ;
  double lon1 = olon1 * M_PI / 180 ;
  double lon2 = olon2 * M_PI / 180 ;
  // std::abs doesn't work with the precision req
  if (delta_lat < 0) {
    delta_lat = (lat1 > lat2) ? (lat1 - lat2) : (lat2 - lat1) ;
  }
  if (delta_lon < 0) {
    delta_lon = (lon1 > lon2) ? (lon1 - lon2) : (lon2 - lon1) ;
  }

  double num = std::pow(sin(delta_lat / 2), 2);
  double den = (cos(lat1) * cos(lat2)) * std::pow(sin(delta_lon / 2), 2);
  double out = num + den;
  if (unitless) {
    return out;
  }
  out = sqrt(out);
  out = asin(out);
  out *= 6371;
  out *= 2;
  return out;
}

double do_euclid_dist (double x1,
                       double x2,
                       double y1,
                       double y2,
                       bool unitless = false) {
  double out = 0;
  double x = x2 - x1;
  double y = y2 - y1;
  out = x * x + y * y;
  if (unitless) {
    return out;
  }
  return sqrt(out);
}

// unitless =>

// [[Rcpp::export]]
NumericVector haversineDistance(NumericVector lat1, NumericVector lon1, NumericVector lat2, NumericVector lon2, bool unitless = false) {
  int N = lat1.length();
  if (N != lon1.length() || N != lat2.length() || N != lon2.length()) {
    stop("Lengths of input vectors differ.");
  }
  NumericVector out(N);
  if (unitless) {
    for (int i = 0; i < N; ++i) {
      out[i] = haversine_distance(lat1[i], lon1[i], lat2[i], lon2[i], -1, -1, true);
    }
  } else {
    for (int i = 0; i < N; ++i) {
      out[i] = haversine_distance(lat1[i], lon1[i], lat2[i], lon2[i]);
    }
  }
  return out;
}





// [[Rcpp::export]]
NumericVector theEuclidDistance (NumericVector x1,
                                 NumericVector x2,
                                 NumericVector y1,
                                 NumericVector y2,
                                 bool unitless = false) {
  unsigned int N = x1.size();
  if (N != y1.size() || N != x2.size() || N != y2.size()) {
    stop("x and y lengths differ.");
  }
  NumericVector out(N);
  double x1i = 0;
  double x2i = 0;
  double y1i = 0;
  double y2i = 0;
  for (unsigned int i = 0; i < N; ++i) {
    x1i = x1[i];
    x2i = x2[i];
    y1i = y1[i];
    y2i = y2[i];
    out[i] = do_euclid_dist(x1i, x2i, y1i, y2i, unitless);
  }
  return out;
}

// [[Rcpp::export]]
double hausdorffEuclid (NumericVector x,
                        NumericVector y) {
  int N = x.size();
  double maxmin_dist = 0;
  for (int i = 0; i < N; ++i) {
    double xi = x[i];
    double yi = y[i];
    double min_dist_i = 0;
    for (int j = 0; j < N; ++j) {
      if (i == j) {
        continue;
      }
      double xj = x[j];
      double yj = y[j];
      double dij = 0;
      dij = do_euclid_dist(xi, xj, yi, yj);
      if (min_dist_i == 0 || dij < min_dist_i) {
        min_dist_i = dij;
      }
    }
    if (min_dist_i > maxmin_dist) {
      maxmin_dist = min_dist_i;
    }
  }
  return maxmin_dist;
}

// [[Rcpp::export]]
bool is_sorted_ascending (NumericVector x) {
  unsigned int N = x.size();
  for (unsigned int i = 1; i < N; ++i) {
    if (x[i] < x[i - 1]) {
      return false;
    }
  }
  return true;
}

// [[Rcpp::export]]
List pole_of_inaccessibility (NumericVector x,
                              NumericVector y,
                              double minx,
                              double maxx,
                              double miny,
                              double maxy) {
  int N = x.size();
  if (N != y.size()) {
    stop("lengths of x and y differ");
  }

  // Use minimum and maximum as parameters as these define
  // the bounding box: important to avoid (a) results
  // that are 'technically correct' but perverse and (b)
  // results that drift out
  const double wx = maxx - minx;
  const double wy = maxy - miny;

  double xc = minx + wx / 2;
  double yc = miny + wy / 2;

  double xp = xc;
  double yp = yc;
  double r = 0;
  double rxy = (wx > wy) ? wx : wy;

  double xi, yi = 0;

  // Note all values are incremented at the head of the while loop
  // so are not the first but the 'zeroth' entries of the sequence.
  // Create a grid
  double g_res = 1;
  double max_g_res = 1;
  double min_g_res = 0.5;


  // Cycle through at a given resolution, keep going until
  // there is a cell with no points. This is a good first
  // approximation to a radius.
  int min_points_in_grid = N;
  bool interior_nonempty = true;
  int chances_remaining = 0;

  double xmin, xmax, ymin, ymax = 0;

  int Debug1 = 0;
  int Debug2 = 0;
  int Debug3 = 0;
  bool reached_binary_maximum = false;

  // For every resolution "g_res" does there exist an empty cell
  // of that resolution?
  // TODO: Once a cell is a found move to a close point
  while (interior_nonempty || chances_remaining) {
    ++Debug1;

    if (!interior_nonempty) {
      reached_binary_maximum = true;
      --chances_remaining;
    }
    if (!reached_binary_maximum && interior_nonempty) {
      g_res *= 2;
      max_g_res *= 2;
      min_g_res *= 2;
    }

    // Increment radix towards the resolution we know to be too fine
    if (reached_binary_maximum) {
      ++Debug3;
      if (interior_nonempty) {
        // needs to be finer
        g_res += (max_g_res - g_res) / 2;
      } else {
        // needs to be more coarse
        g_res -= (g_res - min_g_res) / 2;
      }

    }
    min_points_in_grid = N; // need to reestablish



// translations right and up in x and y
    double tx = wx / g_res;
    double ty = wy / g_res;

    double west_edge = minx - tx;
    double east_edge = minx;

    // grid centres
    double xg = west_edge + tx / 2;
    Rcpp::checkUserInterrupt();
    for (int gridx = 0; min_points_in_grid && (gridx < g_res); ++gridx) {
      west_edge += tx;
      east_edge += tx;
      xg += tx;

      double south_edge = miny - ty;
      double north_edge = miny;
      double yg = south_edge + ty / 2;
      for (int gridy = 0; min_points_in_grid && (gridy < g_res); ++gridy) {
        north_edge += ty;
        south_edge += ty;
        yg += ty;

        // now count number of points in this cell
        int npoints = 0;
        for (int i = 0; i < N; ++i) {
          xi = x[i];
          if (xi < west_edge || xi > east_edge) {
            continue;
          }
          yi = y[i];
          if (yi < south_edge || yi > north_edge) {
            continue;
          }
          ++npoints;
        }
        if (npoints < min_points_in_grid && north_edge < maxy && east_edge < maxx) {
          min_points_in_grid = npoints;
          r = g_res;
          xp = xg;
          yp = yg;
          xmin = west_edge;
          xmax = east_edge;
          ymin = south_edge;
          ymax = north_edge;
        }
      }
    }
    interior_nonempty = min_points_in_grid > 0;
  }


  List out = List::create (Named("x_centre") = xp,
                           Named("y_centre") = yp,
                           Named("xmin") = xmin,
                           Named("xmax") = xmax,
                           Named("ymin") = ymin,
                           Named("ymax") = ymax,
                           Named("Radius") = 1/r);
  return out;
}

// [[Rcpp::export]]
int which_min_HaversineDistance (NumericVector lat1,
                                 NumericVector lon1,
                                 double lat2,
                                 double lon2,
                                 double upperBound = 10) {
  int N = lat1.length();
  if (N != lon1.length()) {
    stop("length(lat1) != length(lat2).");
  }

  // Find a way to detect upper bound at runtime
  double max_delta_lat = 0;
  double max_delta_lon = 0;
  double new_dist = 0;

  if (upperBound > 0) {
    while (max_delta_lat < 2 && new_dist < upperBound) {
      max_delta_lat += 0.001;
      new_dist = haversine_distance(lat2, lon2, lat2 + max_delta_lat, lon2);
    }
    new_dist = 0;
    while (max_delta_lon < 2 && new_dist < upperBound) {
      max_delta_lon += 0.001;
      new_dist = haversine_distance(lat2, lon2, lat2, lon2 + max_delta_lon);
    }
  }

  int out = 1;
  double cur_dist = 0;
  double min_dist = 50000; // bigger than the circumference of Earth
  min_dist = haversine_distance(lat1[0], lon1[0], lat2, lon2);
  double delta_lat = 0;
  double delta_lon = 0;
  bool skip = false;

  for (int i = 1; i < N; ++i) {
    // If the delta lat is greater than the upper bound, don't calculate haversine distance
    if (upperBound > 0) {
      delta_lat = lat1[i] - lat2;
      skip = (delta_lat > 0) ? (delta_lat > max_delta_lat) : (delta_lat < -max_delta_lat);
      if (skip) {
        continue ;
      }
      delta_lon = lon1[i] - lon2;
      skip = (delta_lon > 0) ? (delta_lon > max_delta_lon) : (delta_lon < -max_delta_lon);
      if (skip) {
        continue ;
      }
    }

    cur_dist = haversine_distance(lat1[i], lon1[i], lat2, lon2);
    if (cur_dist < min_dist) {
      min_dist = cur_dist;
      out = i + 1;
    }
  }
  return out;
}

// #nocov start
// [[Rcpp::export]]
void showValue(const char* what, double x) {
  Rcout << " " << what << " \t " << x << std::endl;
}
// #nocov end

// [[Rcpp::export]]
List match_min_Haversine (NumericVector lat1,
                          NumericVector lon1,
                          NumericVector lat2,
                          NumericVector lon2,
                          IntegerVector tabl,
                          double r = 0.002,
                          double cartR = -1,
                          double dist0 = 10,
                          bool excl_self = false,
                          int ncores = 1) {
  int N1 = lat1.length();
  if (N1 != lon1.length()) {
    stop("length(lat1) != length(lon1).");
  }
  int N2 = lat2.length();
  if (N2 != lon2.length()) {
    stop("length(lat2) != length(lon2)");
  }
  if (excl_self && N2 != N1) {
    warning("`excl_self = true`, yet lengths of `lat1` and `lat2` differ. This implies matching positions in lat1,lon1 and lat2,lon2 do not reflect the same points.");
  }

  int N3 = tabl.length();
  bool use_tbl = N3 == N2;

  double lati = 0;
  double loni = 0;
  double latj = 0;
  double lonj = 0;

  double euij = 0;

  double delta_lat = 0;
  double delta_lon = 0;

  IntegerVector out(N1);
  NumericVector out2(N1);
  double to_rad = M_PI / 180;
  double dist0_km = dist0 / 1000;

  bool skip = false;
  int k = 0;
  for (int i = 0; i < N1; ++i) {
    Rcpp::checkUserInterrupt();
    lati = lat1[i];
    loni = lon1[i];

    // Unitless distances to monitor for the minimum
    double min_dist = 50000;
    double cur_dist = 0;

    // Use this to check the 'near enough' distance
    double min_dist_km = 50000;
    k = 0;
    for (int j = 0; j < N2; ++j) {
      if (excl_self && j == i) {
        continue ;
      }
      if (excl_self && j > i) {
        // Since excl_self is only used when
        // the two sets are the same,
        // we can get more performance
        // by just replaying the values
        // we've already found to be closest.

        // Perhaps not? Consider x = {1, 2, 4, 8, 16}

      }

      latj = lat2[j];
      lonj = lon2[j];

      // haversine distance / euclidean is maximal for low latitude and
      // longitudes of around 135 and
      // is 114
      if (cartR > 0) {
        euij = do_euclid_dist(loni, lonj, lati, latj, true);
        if (euij > cartR) {
          continue;
        }
      }


      delta_lat = lati - latj;
      if (delta_lat < 0) {
        delta_lat = lati - latj;
      }
      delta_lat *= to_rad;
      if (r > 0) {
        skip = delta_lat > r;
        if (skip) {
          continue;
        }
      }
      delta_lon = loni - lonj;
      if (delta_lon < 0) {
        delta_lon = lonj - loni ;
      }
      delta_lon *= to_rad;
      if (r > 0) {
        skip = delta_lon > r;
        if (skip) {
          continue;
        }
      }
      // unitless if we just need to compare to min_dist
      cur_dist = haversine_distance(lati, loni, latj, lonj, delta_lat, delta_lon, true);
      if (cur_dist < min_dist) {
        min_dist = cur_dist;

        // There was an issue with undefined behaviour;
        // k being out of range of j appeared to be the
        // cause so this was included in case k was somehow
        // not being reset within every j-loop.
        k = 0;
        k += j;

        // Calculate the real distance
        min_dist_km = haversine_distance(lati, loni, latj, lonj, delta_lat, delta_lon);
        // If within the "close-enough" distance, break to the next item to geocode.
        if (min_dist_km < dist0_km) {
          break;
        }
      }
    }


    if (use_tbl) {
      if (k >= tabl.length()) {
        showValue("k", k); // #nocov
        showValue("i", i); // #nocov
        stop("k >= tabl.length"); // #nocov
      }
      out[i] = tabl[k];
      out2[i] = min_dist_km;
    } else {
      if (k >= lon2.length()) {
        showValue("k", k); // #nocov
        showValue("i", i); // #nocov
        stop("k >= tabl.length"); // #nocov
      }
      ++k;  // for R indexing
      out[i] = k;
      out2[i] = min_dist_km;
    }
  }

  // NumericMatrix mat(N1, 2);
  // mat(_, 0) = out;
  // mat(_, 1) = out2;

  List mat = List::create(Named("pos") = out,
                          Named("dist") = out2);

  return mat;
}




// [[Rcpp::export]]
NumericVector theEuclidDistance (NumericVector x1,
                                 NumericVector x2,
                                 NumericVector y1,
                                 NumericVector y2,
                                 bool unitless = false) {
  unsigned int N = x1.size();
  if (N != y1.size() || N != x2.size() || N != y2.size()) {
    stop("x and y lengths differ.");
  }
  NumericVector out(N);
  double x1i = 0;
  double x2i = 0;
  double y1i = 0;
  double y2i = 0;
  for (unsigned int i = 0; i < N; ++i) {
    x1i = x1[i];
    x2i = x2[i];
    y1i = y1[i];
    y2i = y2[i];
    out[i] = do_euclid_dist(x1i, x2i, y1i, y2i, unitless);
  }
  return out;
}






