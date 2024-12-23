test_that("is_new() identifies first occurrences with basic cases", {
  expect_equal(is_new(c("A", "B", "C", "A")), c(TRUE, TRUE, TRUE, FALSE))
})

test_that("is_new() identifies first occurrences with numeric vectors", {
  expect_equal(is_new(c(1, 2, 3, 1)), c(TRUE, TRUE, TRUE, FALSE))
})

test_that("is_new() identifies first occurrences with logical vectors", {
  expect_equal(is_new(c(TRUE, FALSE, TRUE)), c(TRUE, TRUE, FALSE))
})

test_that("is_new() handles empty vectors", {
  expect_equal(is_new(character(0)), logical(0))
})

test_that("is_new() handles single-element vectors", {
  expect_equal(is_new(c("A")), c(TRUE))
})

test_that("is_new() shows case sensitivity", {
  expect_equal(is_new(c("a", "A", "a")), c(TRUE, TRUE, FALSE))
})
