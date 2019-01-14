#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;


// [[Rcpp::export]]
double haversine_distance (double olat1, double olon1, double olat2, double olon2, double delta_lat = -1, double delta_lon = -1, bool unitless = false) {
  double pi = 3.1415926535897;
  double lat1 = olat1 * pi / 180 ;
  double lat2 = olat2 * pi / 180 ;
  double lon1 = olon1 * pi / 180 ;
  double lon2 = olon2 * pi / 180 ;
  // abs doesn't work with the precision req
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
int which_min_HaversineDistance (NumericVector lat1, NumericVector lon1, double lat2, double lon2, double upperBound = 10) {
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

// [[Rcpp::export]]
void showValue(const char* what, double x) {
  Rcout << "The value " << what << " is " << x << std::endl;
}

// [[Rcpp::export]]
List match_min_Haversine (NumericVector lat1,
                                   NumericVector lon1,
                                   NumericVector lat2,
                                   NumericVector lon2,
                                   IntegerVector tabl,
                                   double r = 0.002,
                                   double dist0 = 10,
                                   bool excl_self = false) {
  int N1 = lat1.length();
  if (N1 != lon1.length()) {
    stop("length(lat1) != length(lon1).");
  }
  int N2 = lat2.length();
  if (N2 != lon2.length()) {
    stop("length(lat2) != length(lon2)");
  }
  if (excl_self && N2 != N1) {
    warning("`excl_self = true`, yet lengths of N2 and N1 differ. This implies matching positions in lat1,lon1 and lat2,lon2 do not reflect the same points.");
  }

  int N3 = tabl.length();
  bool use_tbl = N3 == N2;

  double lati = 0;
  double loni = 0;
  double latj = 0;
  double lonj = 0;

  double delta_lat = 0;
  double delta_lon = 0;

  double pi = 3.1415926535897;
  IntegerVector out(N1);
  NumericVector out2(N1);
  double to_rad = pi / 180;

  bool skip = false;
  int k = 0;
  for (int i = 0; i < N1; ++i) {
    Rcpp::checkUserInterrupt();
    lati = lat1[i];
    loni = lon1[i];
    double min_dist = 50000;
    double cur_dist = 0;
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
      delta_lat = lati - latj;
      if (delta_lat < 0) {
        delta_lat = lati - latj;
      }
      delta_lat *= to_rad;
      skip = delta_lat > r;
      if (skip) {
        continue;
      }
      delta_lon = loni - lonj;
      if (delta_lon < 0) {
        delta_lon = lonj - loni ;
      }
      delta_lon *= to_rad;
      skip = delta_lon > r;
      if (skip) {
        continue;
      }
      cur_dist = haversine_distance(lati, loni, latj, lonj, delta_lat, delta_lon);
      if (cur_dist < min_dist) {
        min_dist = cur_dist;

        // There was an issue with undefined behaviour;
        // k being out of range of j appeared to be the
        // cause so this was included in case k was somehow
        // not being reset within every j-loop.
        k = 0;
        k += j;


        // If within the "close-enough" distance, break to the next entry.
        if (min_dist < (dist0 / 1000)) {
          break ;
        }
      }
    }


    if (use_tbl) {
      if (k >= tabl.length()) {
        showValue("k", k);
        showValue("i", i);
        stop("k >= tabl.length");
      }
      out[i] = tabl[k];
      out2[i] = min_dist;
    } else {
      if (k >= lon2.length()) {
        showValue("k", k);
        showValue("i", i);
        stop("k >= tabl.length");
      }
      ++k;  // for R indexing
      out[i] = k;
      out2[i] = min_dist;
    }
  }

  // NumericMatrix mat(N1, 2);
  // mat(_, 0) = out;
  // mat(_, 1) = out2;

  List mat = List::create(Named("pos") = out,
                          Named("dist") = out2);

  return mat;
}






