#include "hutilscpp.h"


// # nocov start

void showValue(const char* what, double x) {
  // return 0;
}

// # nocov end


double sinhalfsq (double x) {
  const double o = sin(x / 2);
  return o * o;
  // return 0.5 - 0.5 * cos(x); // slowest!
  // return (sin(x/2) * sin(x/2));
}

double haversine_distance(double olat1, double olon1, double olat2, double olon2,
                          bool unitless) {
  // double pi = 3.1415926535897;
  const double lat1 = olat1 * (M_PI / 180) ;
  const double lat2 = olat2 * (M_PI / 180) ;
  const double lon1 = olon1 * (M_PI / 180) ;
  const double lon2 = olon2 * (M_PI / 180) ;
  // std::abs doesn't work with the precision req


  // const double delta_lat = (lat1 > lat2) ? (lat1 - lat2) : (lat2 - lat1) ;
  const double delta_lat = fabs(lat1 - lat2);
  // const double delta_lon = (lon1 > lon2) ? (lon1 - lon2) : (lon2 - lon1) ;
  const double delta_lon = fabs(lon1 - lon2);

  // 6371 * 2 * asin(sqrt(sin(d_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(d_lon / 2)^2))
  double out = 0;
  if (unitless) {
    // just assume sin(x) = x
    out = delta_lat * delta_lat + delta_lon * delta_lon * cos(lat1) * cos(lat2);
    return out;
  }

  double den = cos(lat1) * cos(lat2) * sinhalfsq(delta_lon);
  out = sinhalfsq(delta_lat);
  out += den;
  out = sqrt(out);
  out = asin(out);
  out *= 6371;
  out *= 2;
  return out;
}

double do_euclid_dist(double x1,
                      double x2,
                      double y1,
                      double y2,
                      bool unitless) {
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

SEXP C_haversineDistance(SEXP Lat1,
                         SEXP Lon1,
                         SEXP Lat2, SEXP Lon2, SEXP Unitless) {
  R_xlen_t N = xlength(Lat1);
  if (N != xlength(Lon1) || N != xlength(Lat2) || N != xlength(Lon2)) {
    error("Lengths of input vectors differ.");
  }
  if (TYPEOF(Lat1) != REALSXP ||
      TYPEOF(Lon1) != REALSXP ||
      TYPEOF(Lat2) != REALSXP ||
      TYPEOF(Lon2) != REALSXP ||
      TYPEOF(Unitless) != LGLSXP) {
    error("Internal error(haversineDistance): wrong types."); // # nocov
  }

  const double * lat1 = REAL(Lat1);
  const double * lat2 = REAL(Lat2);
  const double * lon1 = REAL(Lon1);
  const double * lon2 = REAL(Lon2);
  const bool unitless = asLogical(Unitless);
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * out = REAL(ans);
  if (unitless) {
    for (R_xlen_t i = 0; i < N; ++i) {
      out[i] = haversine_distance(lat1[i], lon1[i], lat2[i], lon2[i], true);
    }
  } else {
    for (R_xlen_t i = 0; i < N; ++i) {
      out[i] = haversine_distance(lat1[i], lon1[i], lat2[i], lon2[i], false);
    }
  }
  UNPROTECT(1);
  return ans;
}



SEXP C_theEuclidDistance(SEXP X1,
                        SEXP X2,
                        SEXP Y1,
                        SEXP Y2,
                        SEXP Unitless) {
  if (TYPEOF(X1) != REALSXP ||
      TYPEOF(X2) != REALSXP ||
      TYPEOF(Y1) != REALSXP ||
      TYPEOF(Y2) != REALSXP ||
      TYPEOF(Unitless) != LGLSXP) {
    error("Internal error(theEuclidDistance): wrong input types."); // # nocov
  }
  const bool unitless = asLogical(Unitless);
  R_xlen_t N = xlength(X1);
  R_xlen_t y1N = xlength(Y1);
  R_xlen_t x2N = xlength(X2);
  R_xlen_t y2N = xlength(Y2);
  if (N != y1N || N != x2N || N != y2N) {
    error("x and y lengths differ.");
  }
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * out = REAL(ans);
  const double * x1 = REAL(X1);
  const double * x2 = REAL(X2);
  const double * y1 = REAL(Y1);
  const double * y2 = REAL(Y2);
  for (R_xlen_t i = 0; i < N; ++i) {
    out[i] = do_euclid_dist(x1[i], x2[i], y1[i], y2[i], unitless);
  }
  UNPROTECT(1);
  return ans;
}


SEXP C_hausdorffEuclid(SEXP xx,
                      SEXP yy) {
  R_xlen_t N = xlength(xx);
  const double * x = REAL(xx);
  const double * y = REAL(yy);
  double maxmin_dist = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
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
      dij = do_euclid_dist(xi, xj, yi, yj, false);
      if (min_dist_i == 0 || dij < min_dist_i) {
        min_dist_i = dij;
      }
    }
    if (min_dist_i > maxmin_dist) {
      maxmin_dist = min_dist_i;
    }
  }
  return ScalarReal(maxmin_dist);
}

// 00 -> bottom left
// 01 -> top left
// 10 -> bottom right
// 11 -> top right



//' @return Integer vector of two elements: first element
//' 0,1,2,3 depending on which quarter is least populated;
//' second element, the number of points

void do_EmptiestQuarter(int out[],
                        const double * x,
                        const double * y,
                        int N,
                        double minx,
                        double maxx,
                        double miny,
                        double maxy) {
  if (minx > maxx) {
    minx = x[0];
    maxx = x[0];
    for (R_xlen_t i = 1; i < N; ++i) {
      minx = (x[i] < minx) ? x[i] : minx;
      maxx = (x[i] > maxx) ? x[i] : maxx;
    }
  }
  if (miny > maxy) {
    miny = y[0];
    maxy = y[0];
    for (R_xlen_t i = 1; i < N; ++i) {
      miny = (y[i] < miny) ? y[i] : miny;
      maxy = (y[i] > maxy) ? y[i] : maxy;
    }
  }

  double xcentre = minx + (maxx - minx) / 2;
  double ycentre = miny + (maxy - miny) / 2;

  int q = -1;
  int o = -1;

  int min_points = N;
  double x0, x1, y0, y1 = 0;
  for (q = 0; q < 4; ++q) {
    int q_points = 0;
    switch (q) {
    case 0:
      x0 = minx;
      x1 = xcentre;
      y0 = miny;
      y1 = ycentre;
      break;
    case 1:
      x0 = minx;
      x1 = xcentre;
      y0 = ycentre;
      y1 = maxy;
      break;
    case 2:
      x0 = xcentre;
      x1 = maxx;
      y0 = miny;
      y1 = ycentre;
      break;
    case 3:
      x0 = xcentre;
      x1 = maxx;
      y0 = ycentre;
      y1 = maxy;
      break;
    }
    for (R_xlen_t i = 0; i < N; ++i) {
      double xi = x[i];
      double yi = y[i];
      if (xi > x0 && xi < x1 && yi > y0 && yi < y1) {
        ++q_points;
      }
    }
    if (q_points < min_points) {
      min_points = q_points;
      o = q;
    }
  }
  out[0] = o;
  out[1] = min_points;
}

SEXP CEmptiestQuarter(SEXP xx,
                      SEXP yy,
                      SEXP Minx,
                      SEXP Maxx,
                      SEXP Miny,
                      SEXP Maxy) {
  if (TYPEOF(xx) != REALSXP ||
      TYPEOF(yy) != REALSXP ||
      xlength(xx) != xlength(yy) ||
      xlength(xx) >= INT_MAX ||
      TYPEOF(Minx) != REALSXP ||
      TYPEOF(Maxx) != REALSXP ||
      TYPEOF(Miny) != REALSXP ||
      TYPEOF(Maxy) != REALSXP) {
    error("Internal error(CemptiestQuarter): wrong input types."); // # nocov
  }
  const double * x = REAL(xx);
  const double * y = REAL(yy);
  double minx = asReal(Minx);
  double maxx = asReal(Maxx);
  double miny = asReal(Miny);
  double maxy = asReal(Maxy);
  int N = length(xx);
  if (N == 0) {
    return IntegerN(2); // # nocov
  }
  int out[2] = {0, 0};

  do_EmptiestQuarter(out, x, y, N, minx, maxx, miny, maxy);
  SEXP ans = PROTECT(allocVector(INTSXP, 2));
  INTEGER(ans)[0] = out[0];
  INTEGER(ans)[1] = out[1];
  UNPROTECT(1);
  return ans;
}

void theEmptiestQuarters(int out[],
                         const double * x,
                         const double * y,
                         int N,
                         double minx,
                         double maxx,
                         double miny,
                         double maxy,
                         int depth) {
  if (minx > maxx) {
    minx = Mind(x, N, 1);
    maxx = Maxd(x, N, 1);
  }
  if (miny > maxy) {
    miny = Mind(y, N, 1);
    maxy = Maxd(y, N, 1);
  }
  double x0 = minx;
  double x1 = maxx;
  double y0 = miny;
  double y1 = maxy;
  double dx, dy = 0;
  int o2[2] = {0, 0};
  do_EmptiestQuarter(o2, x, y, N, minx, maxx, miny, maxy);
  int o = o2[0];
  for (R_xlen_t i = 0; i < depth; ++i) {
    out[i] = o;
    dx = x1 - x0;
    dx /= 2;
    dy = y1 - y0;
    dy /= 2;
    switch (o) {
    case 0:
      x1 -= dx;
      y1 -= dy;
      break;
    case 1:
      x1 -= dx;
      y0 += dy;
      break;
    case 2:
      x0 += dx;
      y1 -= dy;
      break;
    case 3:
      x0 += dx;
      y0 += dy;
      break;
    }
    int eq[2] = {0, 0};
    do_EmptiestQuarter(eq, x, y, N, x0, x1, y0, y1);
    o = eq[0];
    int nPoints = eq[1];
    if (nPoints == 0) {
      for (int j = i + 1; j < depth; ++j) {
        out[j] = -1;
      }
      break;
    }
  }
}

SEXP C_theEmptiestQuarters(SEXP x, SEXP y,
                           SEXP minx,
                           SEXP maxx,
                           SEXP miny,
                           SEXP maxy,
                           SEXP Depth) {
  if (TYPEOF(Depth) != INTSXP || asInteger(Depth) > 256) {
    error("Depth is not integer or exceeds 256."); // # nocov
  }
  if (TYPEOF(x) != REALSXP || TYPEOF(y) != REALSXP) {
    error("x,y not REAL."); // # nocov
  }
  if (xlength(x) >= INT_MAX || xlength(x) != xlength(y)) {
    error("x,y wrong lengths."); // # nocov
  }
  int N = length(x);
  if (N == 0) {
    return IntegerN(2); // # nocov
  }
  int out[256] = {0};
  theEmptiestQuarters(out, REAL(x), REAL(y), N,
                      asReal(minx),
                      asReal(maxx),
                      asReal(miny),
                      asReal(maxy),
                      asInteger(Depth));
  SEXP ans = PROTECT(allocVector(INTSXP, 2));
  INTEGER(ans)[0] = out[0];
  INTEGER(ans)[1] = out[1];
  UNPROTECT(1);
  return ans;
}

SEXP C_which_min_HaversineDistance(SEXP Lat1,
                                   SEXP Lon1,
                                   SEXP lat22,
                                   SEXP lon22,
                                   SEXP UpperBound) {
  if (TYPEOF(Lat1) != REALSXP ||
      TYPEOF(Lon1) != REALSXP ||
      TYPEOF(lat22) != REALSXP ||
      xlength(lat22) != 1 ||
      TYPEOF(lon22) != REALSXP ||
      xlength(lon22) != 1 ||
      TYPEOF(UpperBound) != REALSXP ||
      xlength(UpperBound) != 1) {
    error("Internal error(which_min_HaversineDistance): wrong input types."); // # nocov
  }
  const double upperBound = asReal(UpperBound);
  R_xlen_t N = xlength(Lat1);
  if (N != xlength(Lon1)) {
    error("length(lat1) != length(lat2)."); // # nocov
  }
  const double * lat1 = REAL(Lat1);
  const double * lon1 = REAL(Lon1);
  const double lat2 = asReal(lat22);
  const double lon2 = asReal(lon22);

  // Find a way to detect upper bound at runtime
  double max_delta_lat = 0;
  double max_delta_lon = 0;
  double new_dist = 0;

  if (upperBound > 0) {
    while (max_delta_lat < 2 && new_dist < upperBound) {
      max_delta_lat += 0.001;
      new_dist = haversine_distance(lat2, lon2, lat2 + max_delta_lat, lon2, false);
    }
    new_dist = 0;
    while (max_delta_lon < 2 && new_dist < upperBound) {
      max_delta_lon += 0.001;
      new_dist = haversine_distance(lat2, lon2, lat2, lon2 + max_delta_lon, false);
    }
  }

  int out = 1;
  double cur_dist = 0;
  double min_dist = 50000; // bigger than the circumference of Earth
  min_dist = haversine_distance(lat1[0], lon1[0], lat2, lon2, false);
  double delta_lat = 0;
  double delta_lon = 0;
  bool skip = false;

  for (R_xlen_t i = 1; i < N; ++i) {
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

    cur_dist = haversine_distance(lat1[i], lon1[i], lat2, lon2, false);
    if (cur_dist < min_dist) {
      min_dist = cur_dist;
      out = i + 1;
    }
  }
  return ScalarInteger(out);
}

SEXP C_match_min_Haversine(SEXP Lat1,
                          SEXP Lon1,
                          SEXP Lat2,
                          SEXP Lon2,
                          SEXP Tabl,
                          SEXP CartR,
                          SEXP Dist0_km,
                          SEXP Verify_cartR,
                          SEXP Verify_box,
                          SEXP Excl_self,
                          SEXP Ncores) {
  if (TYPEOF(Lat1) != REALSXP ||
      TYPEOF(Lon1) != REALSXP ||
      TYPEOF(Lat2) != REALSXP ||
      TYPEOF(Lon2) != REALSXP ||
      xlength(Lat1) != xlength(Lon1) ||
      xlength(Lat2) != xlength(Lon2) ||
      xlength(Lat2) >= INT_MAX ||
      xlength(Lat1) >= INT_MAX ||
      TYPEOF(Tabl) != INTSXP ||
      xlength(Tabl) >= INT_MAX) {
    error("Internal error(C_match_min_Haversine): wrong input types."); // # nocov
  }
  const double * lat1 = REAL(Lat1);
  const double * lat2 = REAL(Lat2);
  const double * lon1 = REAL(Lon1);
  const double * lon2 = REAL(Lon2);
  const int * tabl = INTEGER(Tabl);

  if (TYPEOF(CartR) != REALSXP ||
      xlength(CartR) != 1 ||
      TYPEOF(Dist0_km) != REALSXP ||
      xlength(Dist0_km) != 1) {
    error("Internal error(C_match_min_Haversine): wrong input types."); // # nocov
  }
  const double cartR = asReal(CartR);
  const double dist0_km = asReal(Dist0_km);


  const bool verify_cartR = asLogical(Verify_cartR);
  const bool do_verify_box = asLogical(Verify_box);
  const bool excl_self = asLogical(Excl_self);
  const int ncores = asInteger(Ncores);

  int N1 = length(Lat1);
  int N2 = length(Lat2);
  if (excl_self && N2 != N1) {
    warning("`excl_self = true`, yet lengths of `lat1` and `lat2` differ. This implies matching positions in lat1,lon1 and lat2,lon2 do not reflect the same points.");
  }

  int N3 = length(Tabl);

  bool use_tbl = N3 == N2;

  double lati = 0;
  double loni = 0;
  double latj = 0;
  double lonj = 0;

  int n_protect = 0;
  SEXP Out = PROTECT(allocVector(INTSXP, N1));
  ++n_protect;
  int * out = INTEGER(Out);

  SEXP Out2 = PROTECT(allocVector(REALSXP, N1));
  double * out2 = REAL(Out2);
  ++n_protect;

  // half-equatorial circumference: used as an 'infinity' for
  // min_dist while also available to check we have actually
  // achieved a minimum distance. (Should be 1 and around 20,000.)
  double BIGDIST = haversine_distance(0, 0, 0, 179.99, true);
  double BIGDISTKM = haversine_distance(0, 0, 0, 179.99, false);

  bool do_verify_cartR = verify_cartR;
  bool do_check_cartR = cartR > 0;

  int k = 0;

  for (int i = 0; i < N1; ++i) {
    if (ncores == 1 && (i % 16) == 0) {
      R_CheckUserInterrupt();
    }
    lati = lat1[i];
    loni = lon1[i];

    // Unitless distances to monitor for the minimum
    double min_dist = BIGDIST;
    double cur_dist = 0;

    // Use this to check the 'near enough' distance after a minimum candidate
    double min_dist_km = BIGDISTKM;
    double max_lati = lati + cartR;
    double min_lati = lati - cartR;
    double max_loni = loni + cartR;
    double min_loni = loni - cartR;

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

      // Don't calculate haversine distance when the current
      // point is outside the box defined by cartR
      if (do_check_cartR) {
        if (latj < min_lati ||
            latj > max_lati ||
            lonj < min_loni ||
            lonj > max_loni) {
          continue;
        }
      }

      // unitless if we just need to compare to min_dist
      cur_dist = haversine_distance(lati, loni, latj, lonj, true);
      if (cur_dist < min_dist) {
        min_dist = cur_dist;

        // There was an issue with undefined behaviour;
        // k being out of range of j appeared to be the
        // cause so this was included in case k was somehow
        // not being reset within every j-loop.
        k = 0;
        k += j;

        // Calculate the real distance
        min_dist_km = haversine_distance(lati, loni, latj, lonj, false);
        // If within the "close-enough" distance, break to the next item to geocode.
        if (min_dist_km < dist0_km) {
          break;
        }
      }
    }

    if (do_verify_box) {
      // The half-length of the square to check within
      double box_r = do_euclid_dist(loni, lon2[k], lati, lat2[k], false);
      double cur_dist_km_new = 0;

      // Box is around the *target*
      double box_max_lat = lati + box_r;
      double box_min_lat = lati - box_r;
      double box_max_lon = loni + box_r;
      double box_min_lon = loni - box_r;
      for (int j = 0; j < N2; ++j) {
        double lat2j = lat2[j], lon2j = lon2[j];
        if (lat2j > box_min_lat &&
            lat2j < box_max_lat &&
            lon2j > box_min_lon &&
            lon2j < box_max_lon) {
          cur_dist_km_new = haversine_distance(lati, loni, lat2j, lon2j, false);
          if (cur_dist_km_new < min_dist_km) {
            k = 0;
            k += j;
            min_dist_km = cur_dist_km_new;
          }
        }
      }
    }

    if (do_verify_cartR && min_dist_km == BIGDISTKM) {
      // We have failed to identify a small distance
      // Likely reason: too ambitious cartR
      do_check_cartR = false; // set from here onwards -- if it happens once, likely to happen again

      for (int j = 0; j < N2; ++j) {
        double cur_dist_km = 0;
        cur_dist_km = haversine_distance(lati, loni, latj, lonj, false);
        if (cur_dist_km < min_dist_km) {
          k = 0;
          k += j;
          min_dist_km = cur_dist_km;
          if (min_dist_km < dist0_km) {
            break;
          }
        }
      }
    }

    if (use_tbl) {
      if (k >= N3) {
        // # nocov start
        if (i <= INT_MAX && k <= INT_MAX) {
          error("k >= tabl.length; k = %d; i = %d", k, i);
        }
        error("k >= tabl.length");
        // # nocov end
      }
      out[i] = tabl[k];
      out2[i] = min_dist_km;
    } else {
      if (k >= N2) {
        // # nocov start
        if (i <= INT_MAX && k <= INT_MAX) {
          error("k >= tabl.length; k = %d; i = %d", k, i);
        }
        error("k >= lon2.length");
        // # nocov end
      }
      ++k;  // for R indexing
      out[i] = k;
      out2[i] = min_dist_km;
    }
  }

  SEXP mat = PROTECT(allocVector(VECSXP, 2));
  ++n_protect;
  SET_VECTOR_ELT(mat, 0, Out);
  SET_VECTOR_ELT(mat, 1, Out2);
  UNPROTECT(n_protect);
  return mat;
}





