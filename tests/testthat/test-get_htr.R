test_that("get_htr() calculates cumulative hapax-token ratios for basic cases", {
  x <- c("A", "A", "B", "C", "B")
  expect_equal(
    round(get_htr(x),2),
    c(1, 0, 0.33, 0.5, 0.2)
  )
})

test_that("get_htr() works with vectors having all unique values", {
  x <- c("A", "B", "C", "D")
  expect_equal(
    get_htr(x),
    c(1, 1, 1, 1)
  )
})

test_that("get_htr() works with vectors having all repeated values", {
  x <- c("A", "A", "A", "A")
  expect_equal(
    get_htr(x),
    c(1, 0, 0, 0)
  )
})

test_that("get_htr() works with a single-value", {
  x <- c("A")
  expect_equal(
    get_htr(x),
    c(1)
  )
})

test_that("get_htr() handles an empty vector", {
  x <- c()
  expect_equal(
    get_htr(x),
    numeric(0)
  )
})

test_that("get_htr() handles NA values", {
  x <- c("A", "B", NA, "A", "C", NA)
  expect_equal(
    round(get_htr(x), 2),
    c(1.00, 1.00, 1.00, 0.33, 0.50, 0.50)
  )
})
