#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;


// [[Rcpp::export]]
double haversine_distance (double olat1, double olon1, double olat2, double olon2, double delta_lat = -1, double delta_lon = -1) {
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
  double out = asin(sqrt(num + den));
  out *= 6371;
  out *= 2;
  return out;
}

// [[Rcpp::export]]
NumericVector haversineDistance(NumericVector lat1, NumericVector lon1, NumericVector lat2, NumericVector lon2) {
  int N = lat1.length();
  if (N != lon1.length() || N != lat2.length() || N != lon2.length()) {
    stop("Lengths of input vectors differ.");
  }
  NumericVector out(N);
  for (int i = 0; i < N; ++i) {
    out[i] = haversine_distance(lat1[i], lon1[i], lat2[i], lon2[i]);
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

//' @title Match coordinates to nearest coordinates
//' @name match_min_Haversine
//' @description When geocoding coordinates to known addresses, an efficient way to
//' match the given coordinates with the known is necessary.
//' @param lat1,lat2 Coordinates to be geocoded. Numeric vectors of equal length.
//' @param lat2,lon2 Coordinates of known locations. Numeric vectors of equal length
//' (likely to be a different length than the length of \code{lat1}).
//' @param tabl Either \code{0L} or an integer vector the same length as \code{lat1}. If \code{0L},
//' then the positions in \code{lat2,lon2} are returned; if a vector, the corresponding values.
//' @param r A "radius" of distances outside which distances will not be considered.
//' Used for efficiency. The default of \code{r = 0.01} corresponds to about 15 km.
//'
//' @return For each row in \code{lat1, lon1} the position (or corresponding value in \code{tbl})
//' in \code{lat2, lon2} which is nearest to \code{lat1, lon1}.
//'
//' @examples
//' lat2 <- runif(5, -38, -37.8)
//' lon2 <- rep(145, 5)
//'
//' lat1 <- c(-37.875, -37.91)
//' lon1 <- c(144.96, 144.978)
//'
//' match_min_Haversine(lat1, lon1, lat2, lon2, 0L)
//'
//'
//' @export match_min_Haversine

// [[Rcpp::export]]
IntegerVector match_min_Haversine (NumericVector lat1, NumericVector lon1, NumericVector lat2, NumericVector lon2, IntegerVector tabl, double r = 0.002) {
  int N1 = lat1.length();
  if (N1 != lon1.length()) {
    stop("length(lat1) != length(lon1).");
  }
  int N2 = lat2.length();
  if (N2 != lon2.length()) {
    stop("length(lat2) != length(lon2)");
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

  bool skip = false;
  for (int i = 0; i < N1; ++i) {
    lati = lat1[i] * pi / 180;
    loni = lon1[i] * pi / 180;
    double min_dist = 50000;
    double cur_dist = 0;
    int k = 0;
    for (int j = 0; j < N2; ++j) {
      latj = lat2[j] * pi / 180;
      lonj = lon2[j] * pi / 180;
      delta_lat = lati - latj;
      if (delta_lat < 0) {
        delta_lat = lati - latj;
      }
      skip = delta_lat > r;
      if (skip) {
        continue;
      }
      delta_lon = loni - lonj;
      if (delta_lon < 0) {
        delta_lon = lonj - loni ;
      }
      skip = delta_lon > r;
      if (skip) {
        continue;
      }
      cur_dist = haversine_distance(lati, loni, latj, lonj, delta_lat, delta_lon);
      if (cur_dist < min_dist) {
        min_dist = cur_dist;
        k = j;
      }
    }


    if (use_tbl) {
      out[i] = tabl[k];
    } else {
      ++k;  // for R indexing
      out[i] = k;
    }
  }
  return out;
}






