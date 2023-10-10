#context "test-is_constant")

# test_that("Error handling", {
expect_error(is_constant(list(x = 0, y = integer(5))),
             "atomic")


# test_that("is_constant works", {
expect_true(is_constant(NULL))
expect_true(is_constant(integer(0)))
expect_true(is_constant(integer(1)))

expect_true(is_constant(logical(10)))
expect_true(is_constant(integer(10)))
expect_true(is_constant(double(10)))
expect_true(is_constant(character(10)))
expect_true(is_constant(as.factor(character(10))))
expect_true(is_constant(raw(10)))

expect_false(is_constant(c(TRUE, FALSE)))
expect_false(is_constant(c(integer(10), 1L)))
expect_false(is_constant(c(integer(10), 1)))
expect_false(is_constant(c(character(10), 1)))
expect_false(is_constant(c(character(10), 1)))
expect_false(is_constant(c(raw(10), as.raw(1))))
expect_false(is_constant(c(factor(c(character(10), 1)))))
expect_false(is_constant(c(NaN, 1)))
expect_false(is_constant(c(NA, 1)))
expect_true(is_constant(c(NaN, NA)))
expect_false(is_constant(c(NA, NaN, 1)))


# test_that("is_constant works (nThread = 1)", {
nThreadOpt <- getOption("hutilscpp.nThread")
options(hutilscpp.nThread = 1L)
expect_true(is_constant(NULL))
expect_true(is_constant(integer(0)))
expect_true(is_constant(integer(1)))

expect_true(is_constant(logical(10)))
expect_true(is_constant(integer(10)))
expect_true(is_constant(double(10)))
expect_true(is_constant(character(10)))
expect_true(is_constant(as.factor(character(10))))
expect_true(is_constant(raw(10)))

expect_false(is_constant(c(TRUE, FALSE)))
expect_false(is_constant(c(integer(10), 1L)))
expect_false(is_constant(c(integer(10), 1)))
expect_false(is_constant(c(character(10), 1)))
expect_false(is_constant(c(character(10), 1)))
expect_false(is_constant(c(raw(10), as.raw(1))))
expect_false(is_constant(c(factor(c(character(10), 1)))))
expect_false(is_constant(c(NaN, 1)))
expect_false(is_constant(c(NA, 1)))
expect_true(is_constant(c(NaN, NA)))
expect_false(is_constant(c(NA, NaN, 1)))
options("hutilscpp.nThread" = 2L)


# test_that("is_constant(nThread = 2)", {
if (at_home()) {
    expect_true(is_constant(NULL))
    expect_true(is_constant(integer(0)))
    expect_true(is_constant(integer(1)))

    expect_true(is_constant(logical(10)))
    expect_true(is_constant(integer(10)))
    expect_true(is_constant(double(10)))
    expect_true(is_constant(character(10)))
    expect_true(is_constant(as.factor(character(10))))
    expect_true(is_constant(raw(10)))

    expect_false(is_constant(c(TRUE, FALSE)))
    expect_false(is_constant(c(integer(10), 1L)))
    expect_false(is_constant(c(integer(10), 1)))
    expect_false(is_constant(c(character(10), 1)))
    expect_false(is_constant(c(character(10), 1)))
    expect_false(is_constant(c(raw(10), as.raw(1))))
    expect_false(is_constant(c(factor(c(character(10), 1)))))
    expect_false(is_constant(c(NaN, 1)))
    expect_false(is_constant(c(NA, 1)))
    expect_true(is_constant(c(NaN, NA)))
    expect_false(is_constant(c(NA, NaN, 1)))


  # test_that("is_constant nThread", {
  # skip_on_cran()
  x <- integer(1024)
  expect_true(is_constant(x, nThread = 2L))
  xnc <- c(x, 1L, x)
  expect_false(is_constant(xnc, nThread = 2L))
}
options("hutilscpp.nThread" = nThreadOpt)

# test_that("is_constant with NA", {
expect_false(is_constant(c(TRUE, TRUE, TRUE, NA)))
expect_false(is_constant(c(FALSE, FALSE, NA)))
expect_false(is_constant(c(1L, 1L, 1L, NA)))
expect_true(is_constant(c(NA, NA, NA)))
expect_true(is_constant(c(NA_integer_, NA_integer_)))
expect_true(is_constant(c(NA_real_, NA_real_)))
expect_true(is_constant(c(NA_real_, NA_real_, NaN)))
expect_true(is_constant(c(NA_character_, NA_integer_)))


# test_that("isntConstant works", {
expect_equal(isntConstant(integer(0)), 0)
expect_equal(isntConstant(c(1, 1, 1)), 0L)
expect_equal(isntConstant(rep(0L, 2L)), 0L)
expect_equal(isntConstant(c(1, 2, 1)), 2L)
expect_equal(isntConstant(c(1L, 1L, 1L, -3L)), 4L)
expect_equal(isntConstant(logical(4)), 0L)
expect_equal(isntConstant(c(logical(4), TRUE)), 5L)
expect_equal(isntConstant(rep(NaN, 10)), 0)
expect_equal(isntConstant(character(5)), 0)
expect_equal(isntConstant(rep(NA_character_, 5)), 0)
expect_equal(isntConstant(complex(5)), 0)
expect_equal(isntConstant(1:5 + complex(5)), 2)


# test_that("isntConstant error handling", {
expect_error(isntConstant(list(list())),
             pattern = "was not atomic")


# test_that("isntConstant len-1", {
expect_equal(isntConstant(1), 0L)


# test_that("isntConstant logical", {
expect_equal(isntConstant(c(NA, NA, TRUE)), 3L)
expect_equal(isntConstant(c(NA, NA, TRUE, FALSE)), 3L)
expect_equal(isntConstant(logical(3)), 0L)
expect_equal(isntConstant(!logical(3)), 0L)
expect_equal(isntConstant(c(logical(3), TRUE)), 4L)
expect_equal(isntConstant(!c(logical(3), TRUE)), 4L)



# test_that("isntConstant NA", {
expect_equal(isntConstant(rep(NA, 5)), 0L)
expect_equal(isntConstant(rep(NA_integer_, 5)), 0L)
expect_equal(isntConstant(c(1, rep(NA, 5))), 2L)
expect_equal(isntConstant(c(rep(NA, 5), 1)), 6L)
expect_equal(isntConstant(c(rep(NA, 5), 1)), 6L)
expect_equal(isntConstant(c(1, 1, NA, 1)), 3L)


# test_that("isntConstant character", {
expect_equal(isntConstant(character(5)), 0L)
expect_equal(isntConstant(c(NA_character_, character(4))), 2L)
expect_equal(isntConstant(c("", NA_character_, "")), 2L)
expect_equal(isntConstant(c("a", "a", "")), 3L)


# test_that("isntConstant other type", {
x5 <- raw(5)
expect_equal(isntConstant(x5), 0L)
expect_equal(isntConstant(c(as.raw(1), x5)), 2L)
expect_equal(isntConstant(c(x5, as.raw(1))), 6L)


# test_that("do_isntConstant(LGL)", {
expect_equal(isntConstant(NULL), 0L)
expect_equal(isntConstant(TRUE), 0L)
expect_equal(isntConstant(c(TRUE, FALSE)), 2L)
expect_equal(isntConstant(c(TRUE, TRUE)), 0L)


# test_that("hutilsc original", {
xl <- logical(5)
xi <- integer(5)
xd <- double(5)
xc <- character(5)
expect_true(is_constant(xl))
expect_true(is_constant(xi))
expect_true(is_constant(xd))
expect_true(is_constant(xc))

yl <- c(TRUE, xl)
yi <- c(1L, xi)
yd <- c(1, xd)
yc <- c(" ", xc)
expect_false(is_constant(yl))
expect_false(is_constant(yi))
expect_false(is_constant(yd))
expect_false(is_constant(yc))

xcomplex <- complex(10)
expect_true(is_constant(xcomplex))
xcomplex <- c(xcomplex, 2)
expect_false(is_constant(xcomplex))

xraw <- raw(10)
expect_true(is_constant(xraw))
xraw <- c(xraw, charToRaw("b"))
expect_false(is_constant(xraw))

expect_true(is_constant(NULL))
expect_true(is_constant(1))




