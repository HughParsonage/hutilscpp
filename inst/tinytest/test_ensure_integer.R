
ensure_integer <- hutilscpp:::ensure_integer


expect_error(ensure_integer(5.5))
expect_error(ensure_integer("5"))
expect_warning(ensure_integer(5.5, on_failure = "warn"))
expect_error(ensure_integer(5e9))
expect_identical(ensure_integer(5), 5L)
expect_identical(ensure_integer(5L), 5L)

