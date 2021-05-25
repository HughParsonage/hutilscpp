

haversineDistance <- function(lat1, lon1, lat2, lon2, unitless = FALSE) {
  .Call("C_haversineDistance", lat1, lon1, lat2, lon2, unitless, PACKAGE = packageName)
}

