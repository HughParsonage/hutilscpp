# Verification and fallback must honor positional self-exclusion.
for (verify_box in c(FALSE, TRUE)) {
  for (spacing in c(0.01, 1)) {
    lat <- c(0, spacing)
    result <- match_nrst_haversine(lat, c(0, 0), lat, c(0, 0),
                                  excl_self = TRUE, as.data.table = FALSE,
                                  .verify_box = verify_box)
    expect_identical(result$pos, c(2L, 1L))
    expect_equal(result$dist, rep(hutilscpp:::haversineDistance(0, 0, spacing, 0), 2))
  }

  # The first candidate, the last candidate and later queries all require
  # distances to be calculated from the corresponding candidate coordinates.
  for (addresses in list(c(1, 2), c(2, 1))) {
    result <- match_nrst_haversine(c(0, 3), c(0, 0), addresses, c(0, 0),
                                  as.data.table = FALSE, .verify_box = verify_box)
    expect_identical(result$pos, as.integer(c(which.min(addresses), which.max(addresses))))
    expect_equal(result$dist, rep(hutilscpp:::haversineDistance(0, 0, 1, 0), 2))
  }

  result <- match_nrst_haversine(c(0, 1), c(0, 0), c(0, 1), c(0, 0),
                                Index = c("first", "second"), excl_self = TRUE,
                                as.data.table = FALSE, .verify_box = verify_box)
  expect_identical(result$pos, c("second", "first"))

  # No eligible candidate must not be returned as an excluded self match.
  result <- match_nrst_haversine(0, 0, 0, 0, excl_self = TRUE,
                                as.data.table = FALSE, .verify_box = verify_box)
  expect_identical(result$pos, NA_integer_)
  expect_identical(result$dist, Inf)

  for (latitude in c(-90, -70, -63, 63, 70, 90)) {
    result <- match_nrst_haversine(latitude, 0, latitude, 0,
                                  as.data.table = FALSE, .verify_box = verify_box)
    expect_identical(result$pos, 1L)
    expect_identical(result$dist, 0)
  }
}

# Distances at the antipode must be eligible minima too.
result <- match_nrst_haversine(0, 0, 0, 180, as.data.table = FALSE)
expect_identical(result$pos, 1L)
expect_equal(result$dist, hutilscpp:::haversineDistance(0, 0, 0, 180))
