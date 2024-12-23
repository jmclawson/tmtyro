test_that("get_ttr() calculates basic cumulative type-token ratios", {
  x <- c("A", "A", "B", "C")
  expect_equal(
    round(get_ttr(x), 2),
    c(1.00, 0.50, 0.67, 0.75)
  )
})

test_that("get_ttr() calculates cumulative type-token ratios with all unique values", {
  x <- c("A", "B", "C")
  expect_equal(
    get_ttr(x),
    c(1, 1, 1)
  )
})

test_that("get_ttr() calculates cumulative type-token ratios with all repeated values", {
  x <- c("A", "A", "A")
  expect_equal(
    round(get_ttr(x), 2),
    c(1.00, 0.50, 0.33)
  )
})

test_that("get_ttr() works with one single values", {
  # Edge case: Single value
  x <- c("A")
  expect_equal(
    get_ttr(x),
    c(1)
  )
})

test_that("get_ttr() works with an empty vector", {
  x <- c()
  expect_equal(
    get_ttr(x),
    numeric(0)
  )
})

test_that("get_ttr() handles NA values", {
  x <- c("A", "B", NA, "A", "C")
  expect_equal(
    round(get_ttr(x),2),
    c(1.00, 1.00, 0.67, 0.50, 0.60)
  )
})
