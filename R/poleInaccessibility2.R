#' Find a binary pole of inaccesibility
#' @param x,y Coordinates.
#' @param DT A \code{data.table} containing \code{LONGITUDE} and \code{LATITUDE} to define
#' the \code{x} and \code{y} coordinates.
#' @param x_range,y_range Numeric vectors of length-2; the range of \code{x} and \code{y}.
#' Use this rather than the default
#' @return A named vector containing the
#' \code{xmin}, \code{xmax} and
#' \code{ymin}, \code{ymax} coordinates of
#' the largest rectangle of width an integer power of two that is empty.
#'
#'


poleInaccessibility2 <- function(x = NULL,
                                 y = NULL,
                                 DT = NULL,
                                 x_range = NULL,
                                 y_range = NULL) {
  if (is.null(DT) && is.null(x) && is.null(y)) {
    stop("`DT`, `x`, and `y` were all NULL. ",
         "`DT` or both `x` and `y` must be provided.")
  }
  if (is.null(DT)) {
    DT <- data.table(x, y)
    setnames(DT,
             c("x", "y"),
             c("LONGITUDE", "LATITUDE"))
  }
  if (is.null(x_range)) {
    global_minx <- DT[, min(x)]
    global_maxx <- DT[, max(x)]
  } else {
    global_minx <- x_range[1]
    global_maxx <- x_range[2]
  }
  if (is.null(y_range)) {
    global_miny <- DT[, min(y)]
    global_maxy <- DT[, max(y)]
  } else {
    global_miny <- y_range[1]
    global_maxy <- y_range[2]
  }
  npoints <- DT[, .N]
  d <- 0L
  while (npoints > 0) {
    d <- d + 1L
    cut_DT(DT, d)
    n_by_quarter <-
      DT[, .N, keyby = c(paste0("xbreaks", d), paste0("ybreaks", d))]
    if (n_by_quarter[, .N] < 4^d) {
      npoints <- 0L
      DT_expand <- CJ(xbreaks = seq_len(2^d),
                      ybreaks = seq_len(2^d))
      setnames(DT_expand,
               c("xbreaks", "ybreaks"),
               c(paste0("xbreaks", d), paste0("ybreaks", d)))
      setkeyv(DT_expand, key(n_by_quarter))

      zero_by_quarter <-
        DT_expand[!n_by_quarter]
    }
  }


  # This returns the 'scaled' versions of each rectangle's vertex (0, 0) - (1, 1)
  z_by_q <- zero_by_quarter[, lapply(.SD, function(x) c({x - 1} / 2^d, x / 2^d))]

  unscale <- function(p, v0, v1) {
    v0 + (v1 - v0) * p
  }
  # Need to then unscale
  c("xmin" = unscale(z_by_q[1][[1]], global_minx, global_maxx),
    "xmax" = unscale(z_by_q[2][[1]], global_minx, global_maxx),
    "ymin" = unscale(z_by_q[1][[2]], global_miny, global_maxy),
    "ymax" = unscale(z_by_q[2][[2]], global_miny, global_maxy))
}


cut_DT <- function(DT, depth = 1L, x_range = NULL, y_range = NULL) {
  stopifnot(hasName(DT, "LONGITUDE"))
  stopifnot(hasName(DT, "LATITUDE"))
  if (is.null(x_range)) {
    x_range <- DT[, range_rcpp(LONGITUDE)]
  }
  if (is.null(y_range)) {
    y_range <- DT[, range_rcpp(LATITUDE)]
  }
  DT[, "xbreaks" := .bincode(LONGITUDE,
                             include.lowest = TRUE,
                             breaks = seq.int(from = x_range[1],
                                              to = x_range[2],
                                              length.out = 2^depth + 1L))]
  setnames(DT, "xbreaks", paste0("xbreaks", depth))
  DT[, "ybreaks" := .bincode(LATITUDE,
                             include.lowest = TRUE,
                             breaks = seq.int(from = y_range[1],
                                              to = y_range[2],
                                              length.out = 2^depth + 1L))]
  setnames(DT, "ybreaks", paste0("ybreaks", depth))
  DT[]
}

