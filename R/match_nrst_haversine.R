#' @title Match coordinates to nearest coordinates
#' @description When geocoding coordinates to known addresses, an efficient way to
#' match the given coordinates with the known is necessary. This function provides this
#' efficiency by using \code{C++} and allowing approximate matching.
#' @param lat,lon Coordinates to be geocoded. Numeric vectors of equal length.
#' @param addresses_lat,addresses_lon Coordinates of known locations. Numeric vectors of equal length
#' (likely to be a different length than the length of \code{lat}, except when \code{excl_self = TRUE}).
#' @param Index A vector the same length as \code{lat} to encode the match between \code{lat,lon}
#' and \code{addresses_lat,addresses_lon}. The default is to use the integer position
#' of the nearest match to
#' \code{addresses_lat,addresses_lon}.
#' @param cartesian_R The maximum radius of any address from the points to be geocoded. Used
#' to accelerate the detection of minimum distances. Note, as the argument name suggests,
#' the distance is in cartesian coordinates, so a small number is likely.
#'
#' @param close_enough The distance, in metres, below which a match will be considered to have occurred.
#' (The distance that is considered "close enough" to be a match.)
#'
#' For example, \code{close_enough = 10} means the first location within ten metres will be matched,
#' even if a closer match occurs later.
#'
#' May be provided as a string to emphasize the units, e.g. \code{close_enough = "0.25km"}.
#' Only \code{km} and \code{m} are permitted.
#'
#' @param excl_self (bool, default: \code{FALSE}) For each \eqn{x_i} of the first coordinates,
#' exclude the \eqn{y_i}-th point when determining closest match. Useful to determine the
#' nearest neighbour within a set of coordinates, \emph{viz.}
#' \code{match_nrst_haversine(x, y, x, y, excl_self = TRUE)}.
#'
#' @param as.data.table Return result as a \code{data.table}?
#' If \code{FALSE}, a list is returned. \code{TRUE} by default to
#' avoid dumping a huge list to the console.
#'
#' @param .verify_box Check the initial guess against other points within the
#' box of radius \eqn{\ell^\infty}.
#'
#'
#' @return A list (or \code{data.table} if \code{as.data.table = TRUE}) with two elements,
#' both the same length as \code{lat}, giving for point \code{lat,lon}:
#' \describe{
#' \item{\code{pos}}{the position (or corresponding value in \code{Table})
#'   in \code{addresses_lat,addresses_lon} nearest to \code{lat, lon}.}
#' \item{\code{dist}}{the distance, in kilometres, between the two points.}
#' }
#'
#' @examples
#' lat2 <- runif(5, -38, -37.8)
#' lon2 <- rep(145, 5)
#'
#' lat1 <- c(-37.875, -37.91)
#' lon1 <- c(144.96, 144.978)
#'
#' match_nrst_haversine(lat1, lon1, lat2, lon2)
#' match_nrst_haversine(lat1, lon1, lat1, lon1, 11:12, excl_self = TRUE)
#'
#' @export match_nrst_haversine
#'
#'

match_nrst_haversine <- function(lat,
                                 lon,
                                 addresses_lat,
                                 addresses_lon,
                                 Index = seq_along(addresses_lat),
                                 cartesian_R = NULL,
                                 close_enough = 10,
                                 excl_self = FALSE,
                                 as.data.table = TRUE,
                                 .verify_box = TRUE) {

  stopifnot(is.numeric(lat),
            is.numeric(lon),
            length(lat) == length(lon),
            length(lat) >= 1L,
            is.numeric(addresses_lat),
            is.numeric(addresses_lon),
            length(addresses_lat) == length(addresses_lon),
            length(addresses_lat) >= 1L)
  check_TF(excl_self)
  check_TF(as.data.table)

  if (ce_err_msg <- isnt_number(close_enough)) {
    if (length(close_enough) != 1L || !grepl("^[0-9]+(\\.[0-9]+)?\\s*k?m$", close_enough)) {
      stop(attr(ce_err_msg, "ErrorMessage"), "\n",
           "`close_enough` may be a string of numbers ",
           "ending in 'km' or 'm' to designate units.")
    }
    dist0_km <- units2km(close_enough)
  } else {
    dist0_km <- close_enough / 1000
  }


  min_lat <- min(lat)
  max_lat <- max(lat)
  min_lon <- min(lon)
  max_lon <- max(lon)
  verify_cartR <- FALSE
  if (is.null(cartesian_R)) {
    if (min_lat > -63 && max_lat < 63) {
      # Within 63 degrees of latitude, the cartesian distance
      # is at least 50 times the distance in km.
      cartesian_R <- 0.02  # within a km
      verify_cartR <- TRUE
    }
  }





  if (length(Index) != length(addresses_lat)) {
    warning("`Table` was provided, but was not the same ",
            "length as `addresses_lat`, ",
            "so returning positions.")
    Index <- seq_along(addresses_lat)
  }
  Index.int <-
    if (!is.integer(Index)) {
      seq_along(Index)
    } else {
      Index
    }

  out <- match_min_Haversine(lat,
                             lon,
                             addresses_lat,
                             addresses_lon,
                             Index.int,
                             cartR = cartesian_R,
                             dist0_km = dist0_km,
                             verify_cartR = verify_cartR,
                             do_verify_box = .verify_box,
                             excl_self = excl_self)

  if (!is.integer(Index)) {
    out[[1L]] <- Index[out[[1L]]]
  }

  if (as.data.table) {
    return(data.table::as.data.table(out))
  } else {
    return(out)
  }
}

which_min_HaversineDistance <- function(lat1, lon1, lat2, lon2, upperBound = 10) {
  if (length(lat1) != length(lon1)) {
    stop("length(lat1) != length(lon1)")
  }
  .Call("C_which_min_HaversineDistance",
        lat1, lon1, lat2, lon2, upperBound,
        PACKAGE = packageName)
}


match_min_Haversine <- function(lat,
                                lon,
                                addresses_lat,
                                addresses_lon,
                                tabl = NULL,
                                cartR = -1,
                                dist0_km = 0.01,
                                verify_cartR = FALSE,
                                do_verify_box = FALSE,
                                excl_self = FALSE,
                                ncores = 1L) {
  if (length(lat) != length(lon)) {
    stop("length(lat1) != length(lon1)")
  }
  if (length(addresses_lat) != length(addresses_lon)) {
    stop("length(lat2) != length(lon2)")
  }
  if (is.null(tabl)) {
    tabl <- seq_along(lat)
  }
  if (is.integer(lat)) {
    lat <- as.double(lat)
  }
  if (is.integer(lon)) {
    lon <- as.double(lon)
  }
  if (is.integer(addresses_lat)) {
    addresses_lat <- as.double(addresses_lat)
  }
  if (is.integer(addresses_lon)) {
    addresses_lon <- as.double(addresses_lon)
  }

  stopifnot(is.double(lat),
            is.double(lon),
            is.double(addresses_lat),
            is.double(addresses_lon),
            length(lat) == length(lon),
            length(addresses_lat) == length(addresses_lon),
            length(lat) <= .Machine$integer.max,
            length(addresses_lat) <= .Machine$integer.max)

  if (cramsg <- isnt_number(cartR, na.bad = TRUE, infinite.bad = FALSE)) {
    stop(attr(cramsg, "ErrorMessage")) # nocov
  }
  if (d0amsg <- isnt_number(dist0_km, na.bad = TRUE, infinite.bad = FALSE)) {
    stop(attr(d0amsg, "ErrorMessage")) # nocov
  }
  out <- .Call("C_match_min_Haversine",
               lat,
               lon,
               addresses_lat,
               addresses_lon,
               tabl,
               cartR,
               dist0_km,
               verify_cartR,
               do_verify_box,
               excl_self,
               ncores,
               PACKAGE = packageName)
  names(out) <- c("pos", "dist")
  out
}

theEuclidDistance <- function(x1, x2, y1, y2, unitless = FALSE) {
  .Call("C_theEuclidDistance", x1, x2, y1, y2, unitless, PACKAGE = packageName)
}

hausdorffEuclid <- function(x, y) {
  .Call("C_hausdorffEuclid", x, y, PACKAGE = packageName)
}

EmptiestQuarter <- function(x, y, minx = 1, maxx = -1, miny = 1, maxy = -1) {
  ad <- as.double
 .Call("CEmptiestQuarter", ad(x), ad(y), ad(minx), ad(maxx), ad(miny), ad(maxy), PACKAGE = packageName)
}

theEmptiestQuarters <- function(x, y, minx = 1, maxx = -1, miny = 1, maxy = -1, depth = 4L) {
  ad <- as.double
  .Call("C_theEmptiestQuarters", ad(x), ad(y), ad(minx), ad(maxx), ad(miny), ad(maxy), depth, PACKAGE = packageName)
}


