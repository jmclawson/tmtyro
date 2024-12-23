test_that("is_hapax() identifies basic hapaxes", {
  x <- c("A", "B", "A", "C", "D", "D")
  expect_equal(
    is_hapax(x),
    c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE)
  )
})

test_that("is_hapax() identifies hapaxes when a vector has all unique values", {
  x <- c("A", "B", "C", "D")
  expect_equal(
    is_hapax(x),
    c(TRUE, TRUE, TRUE, TRUE)
  )
})

test_that("is_hapax() identifies hapaxes when a vector has all repeated values", {
  x <- c("A", "A", "B", "B")
  expect_equal(
    is_hapax(x),
    c(FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("is_hapax() identifies hapaxes when a vector has NA values", {
  x <- c("A", "B", NA, "A", NA, NA)
  expect_equal(
    is_hapax(x),
    c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("is_hapax() identifies hapaxes with a numeric vector", {
  x <- c(1, 2, 3, 1, 4, 5, 5)
  expect_equal(
    is_hapax(x),
    c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
  )
})

test_that("is_hapax() identifies hapaxes with a logical vector", {
  x <- c(FALSE, TRUE, TRUE)
  expect_equal(
    is_hapax(x),
    c(TRUE, FALSE, FALSE)
  )
})

test_that("is_hapax() handles an empty vector", {
  x <- c()
  expect_equal(
    is_hapax(x),
    logical(0)
  )
})
