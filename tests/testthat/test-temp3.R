test_that("temp3 works", {
  library(magrittr)
  expect_identical(do_count_logical_long(c(TRUE, FALSE, NA)), rep(1, 3))
  expect_true(do_in_int(1L, 1:4))
  expect_false(do_in_int(0L, 1:4))
  ans1 <- do_and3_x_op(1:5, 7L, 1L, 2L,
                       1:5, 7L, 1L, 2L,
                       1:5, 7L, 1L, 2L,
                       integer(0),
                       integer(0),
                       integer(0),
                       logical(0),
                       logical(0),
                       logical(0))
  expect_identical(ans1, c(1:5) %in% c(1L, 2L))

  ans2 <- do_and3_x_op(1:5, 9L, 1L, 2L,
                       1:5, 9L, 1L, 2L,
                       1:5, 9L, 1L, 2L,
                       integer(0),
                       integer(0),
                       integer(0),
                       logical(0),
                       logical(0),
                       logical(0))
  expect_identical(ans2, magrittr::and(1:5 > 1L, 1:5 < 2L))

  ans3 <- do_and3_x_op(1:5, 10L, 1L, 2L,
                       1:5, 10L, 1L, 2L,
                       1:5, 10L, 1L, 2L,
                       integer(0),
                       integer(0),
                       integer(0),
                       logical(0),
                       logical(0),
                       logical(0))
  expect_identical(ans3, and(c(1:5) <= 1L, c(1:5) <= 2))

  ans4 <- do_and3_x_op(integer(0), 10L, 1L, 2L,
                       integer(0), 10L, 1L, 2L,
                       integer(0), 10L, 1L, 2L,
                       integer(0),
                       integer(0),
                       integer(0),
                       c(TRUE, FALSE),
                       c(TRUE, FALSE),
                       c(TRUE, FALSE))
  expect_identical(ans4, c(TRUE, FALSE))


  expect_true(all(do_op_along(1:10, 0, 1:10)))
  expect_false(any(do_op_along(1:10, -1, 1:10)))

  expect_error(do_op_along(1:10, 0L, 1:11), "lengths differ")
  expect_error(do_and3_x_op(integer(2), 10L, 1L, 2L,
                            integer(1), 10L, 1L, 2L,
                            integer(0), 10L, 1L, 2L,
                            integer(0),
                            integer(0),
                            integer(0),
                            c(TRUE, FALSE),
                            c(TRUE, FALSE),
                            c(TRUE, FALSE)),
               regexp = "length",
               fixed = TRUE)
  expect_error(do_and3_x_op(integer(3), 10L, 1L, 2L,
                            integer(1), 10L, 1L, 2L,
                            integer(2), 10L, 1L, 2L,
                            integer(0),
                            integer(0),
                            integer(0),
                            c(TRUE, FALSE),
                            c(TRUE, FALSE),
                            c(TRUE, FALSE)),
               regexp = "length",
               fixed = TRUE)
  expect_error(do_and3_x_op(integer(3), 10L, 1L, 2L,
                            integer(1), 10L, 1L, 2L,
                            integer(2), 10L, 1L, 2L,
                            integer(7),
                            integer(0),
                            integer(0),
                            c(TRUE, FALSE),
                            c(TRUE, FALSE),
                            c(TRUE, FALSE)),
               regexp = "length",
               fixed = TRUE)
  expect_true(test_intersect3_stdint())
  expect_true(test_intersect3_stdint(4))
  expect_true(test_intersect3_stdint(5))
})
