test_that("get_cumulative_vocabulary() works for basic cases", {
  x <- c("A", "A", "B", "B", "C", "D")
  expect_equal(
    get_cumulative_vocabulary(x),
    c(1, 1, 2, 2, 3, 4)
  )
})

test_that("get_cumulative_vocabulary() correctly counts with all unique values", {
  x <- c("A", "B", "C", "D")
  expect_equal(
    get_cumulative_vocabulary(x),
    c(1, 2, 3, 4)
  )
})

test_that("get_cumulative_vocabulary() correctly counts with all repeated values", {
  x <- c("A", "A", "A", "A")
  expect_equal(
    get_cumulative_vocabulary(x),
    c(1, 1, 1, 1)
  )
})

test_that("get_cumulative_vocabulary() correctly counts with one single value", {
  x <- c("A")
  expect_equal(
    get_cumulative_vocabulary(x),
    c(1)
  )
})

test_that("get_cumulative_vocabulary() handles an empty vector", {
  x <- c()
  expect_equal(
    get_cumulative_vocabulary(x),
    integer(0)
  )
})

test_that("get_cumulative_vocabulary() handles NA values", {
  x <- c("A", "B", NA, "A", "C")
  expect_equal(
    get_cumulative_vocabulary(x),
    c(1, 2, 2, 2, 3)
  )
})
