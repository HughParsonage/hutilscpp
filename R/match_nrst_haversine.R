#' @title Match coordinates to nearest coordinates
#' @description When geocoding coordinates to known addresses, an efficient way to
#' match the given coordinates with the known is necessary. This function provides this
#' efficiency by using \code{C++} and allowing approximate matching.
#' @param lat,lon Coordinates to be geocoded. Numeric vectors of equal length.
#' @param addresses_lat,addresses_lon Coordinates of known locations. Numeric vectors of equal length
#' (likely to be a different length than the length of \code{lat}, except when \code{excl_self = TRUE}).
#' @param Table Either \code{0L} or an integer vector the same length as \code{lat}. If \code{0L},
#' then the positions in \code{lat2,lon2} are returned; if a vector, the corresponding values.
#' @param R A "radius" of distances outside which distances will not be considered.
#' Used for efficiency. The default of \code{R = 0.01} corresponds to about 15 km.
#' @param close_enough The distance, in metres, below which a match will be considered to have occurred.
#' (The distance that is considered "close enough" to be a match.)
#'
#' For example, \code{close_enough = 10} means the first location within ten metres will be matched,
#' even if a more distant match occurs later.
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
#' @param ncores Integer number of cores to use.
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
#' match_nrst_haversine(lat1, lon1, lat2, lon2, 0L)
#' match_nrst_haversine(lat1, lon1, lat1, lon1, 11:12, excl_self = TRUE)
#'
#' @export match_nrst_haversine
#'
#'

match_nrst_haversine <- function(lat,
                                 lon,
                                 addresses_lat,
                                 addresses_lon,
                                 Table = 0L,
                                 R = NULL,
                                 close_enough = 10,
                                 excl_self = FALSE,
                                 as.data.table = TRUE,
                                 ncores = 1L) {
  if (is.null(R)) {
    R <- -1
  }

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
  if (R_err_msg <- isnt_number(R, infinite.bad = FALSE)) {
    stop(attr(R_err_msg, "ErrorMessage"))
  }
  dist0 <- close_enough
  if (ce_err_msg <- isnt_number(close_enough)) {
    if (length(close_enough) != 1L || !grepl("^[0-9]+(\\.[0-9]+)?\\s*k?m$", close_enough)) {
      stop(attr(ce_err_msg, "ErrorMessage"), "\n",
           "`close_enough` may be a string of numbers ending in 'km' or 'm' to designate units.")
    }

    # put km before m!
    if (endsWith(close_enough, "km")) {
      dist0 <- sub("\\s*km$", "", close_enough)
      # use as.double here and as.numeric later to separate warning msgs
      dist0 <- as.double(dist0) * 1000
    } else if (endsWith(close_enough, "m")) {
      dist0 <- sub("\\s*m$", "", close_enough)
      dist0 <- as.numeric(dist0)
    }
    stopifnot(!anyNA(dist0), is.numeric(dist0))
  }
  if (is.infinite(R)) {
    R <- -1
  }

  # Must be an integer (Rcpp handles the lengths)
  .Table <- Table
  recast_Table <-
    !identical(Table, 0L) &&
    (!is.integer(Table) ||
       length(Table) != length(addresses_lat))
  if (recast_Table) {
    .Table <- 0L
  }

  out <- match_min_Haversine(lat,
                             lon,
                             addresses_lat,
                             addresses_lon,
                             .Table,
                             r = R,
                             dist0 = dist0,
                             ncores = ncores)

  if (recast_Table) {
    if (length(Table) != length(addresses_lat)) {
      warning("`Table` was provided, but was not the same length as `addresses_lat`, ",
              "so returning positions.")
    } else {
      out[[1L]] <- Table[out[[1L]]]
    }
  }

  if (as.data.table) {
    return(data.table::as.data.table(out))
  } else {
    return(out)
  }
}


