context("test-haversine")

test_that("Error handling", {
  expect_error(haversineDistance(1, 1:2, 1:3, 1:4), regexp = "ength")
  expect_error(haversineDistance(1:2, 1:2, 1:3, 1:4), regexp = "ength")
  expect_error(which_min_HaversineDistance(1, 1:2, 1, 1),
               regexp = "length(lat1) != length(lat2)",
               fixed = TRUE)
  expect_error(match_min_Haversine(1, 1:2, 1, 1, 0L),
               regexp = "length(lat1) != length(lon1)",
               fixed = TRUE)
  expect_error(match_min_Haversine(1, 1, 1:2, 1, 0L),
               regexp = "length(lat2) != length(lon2)",
               fixed = TRUE)
  expect_warning(match_min_Haversine(1, 1, 1:2, 1:2, 0L, excl_self = TRUE),
                 regexp = "`excl_self = true`, yet lengths of `lat1` and `lat2` differ.",
                 fixed = TRUE)

})

# Same as hutils
# bankstown to sydney airports approximately 17628m
test_that("Bankstown airport to Sydney airport approximately 17628m", {
  expect_lt(haversineDistance(-33 - 56/60 - 46/3600, 151 + 10/60 + 38/3600,
                               -33 - 55/60 - 28/3600, 150 + 59/60+18/3600) / 17.628 - 1,
            0.01)
})

test_that("Broken Hill airport to Sydney airport approximately 932158", {
  expect_lt(haversineDistance(-33 - 56/60 - 46/3600, 151 + 10/60 + 38/3600,
                              -32 - 00/60 - 05/3600, 141 + 28/60 + 18/3600) / 932.158 - 1,
            0.01)
})

test_that("which_min_HaversineDistance", {
  lat1 <- -33 - seq(0, 1, length.out = 10)
  lon1 <- rep_len(150, 10)
  lat2 <- -33.09
  lon2 <- 150
  expect_identical(which_min_HaversineDistance(lat1, lon1, lat2, lon2), 2L)

  expect_identical(which_min_HaversineDistance (double(5), seq(-0.1, 0, length.out = 5), 0, 0,
                                                upperBound = 0),
                   5L)
  expect_identical(which_min_HaversineDistance (double(5), seq(-0.1, 0, length.out = 5), 0, 0,
                                                upperBound = 100),
                   5L)
})

test_that("match_min_Haversine", {
  lat2 <- c(-37.929, -37.962, -37.983, -37.928, -37.85)
  lon2 <- rep(145, 5)

  lat1 <- c(-37.875, -37.88)
  lon1 <- c(144.96, 144.978)

  expect_identical(match_min_Haversine(lat1, lon1, lat2, lon2, 0L)[[1L]], c(5L, 5L))
  expect_identical(match_min_Haversine(lat1, lon1, lat2, lon2, 0L, r = 0.002)[[1L]], c(5L, 5L))
  expect_identical(match_min_Haversine(lat1, lon1, lat2, lon2, 101:105)[[1L]], c(5L, 5L) + 100L)
})

test_that("match_min_Haversine excl_self", {
  lat1 <- c(-37.875, -37.88)
  lon1 <- c(144.96, 144.978)
  mat <- match_min_Haversine(lat1, lon1, lat1, lon1, 0L)
  expect_equal(NROW(mat), length(lat1))
})

test_that("unitless", {
  lat2 <- c(-37.929, -37.962, -37.983, -37.928, -37.85)
  lon2 <- rep(145, 5)

  lat2 <- rep_len(lat2, 10)
  lon2 <- rep_len(lon2, 10)

  lat1 <- rep_len(c(-37.875, -37.88), 10)
  lon1 <- rep_len(c(144.96, 144.978), 10)

  expect_identical(order(haversineDistance(lat1, lon1, lat2, lon2)),
                   order(haversineDistance(lat1, lon1, lat2, lon2, unitless = TRUE)))
})

