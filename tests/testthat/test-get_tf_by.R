test_that("get_tf_by() computes basic category-based frequencies correctly", {
  expect_equal(
    get_tf_by(
      x = c("A", "A", "B", "A", "B", "C"),
      by = c("X", rep("Y", 5))),
    c(1.0, 0.4, 0.4, 0.4, 0.4, 0.2)
  )
})

test_that("get_tf_by() computes single-category frequencies correctly", {
  expect_equal(
    get_tf_by(
      x = c("A", "B", "A", "A", "B"),
      by = c("X", "X", "X", "X", "X")),
    c(0.6, 0.4, 0.6, 0.6, 0.4)
  )
})

test_that("get_tf_by() computes frequencies with equal numbers of terms and categories", {
  expect_equal(
    get_tf_by(
      x = c("A", "B", "C"),
      by = c("X", "Y", "Z")),
    c(1.0, 1.0, 1.0)
  )
})

test_that("get_tf_by() returns an error for mismatched vector lengths", {
  expect_error(
    get_tf_by(
      x = c("A", "B"),
      by = c("X")),
    "`x` and `by` must be vectors of the same length."
  )
})

test_that("get_tf_by() works with numeric vectors", {
  expect_equal(
    get_tf_by(
      x = c(1, 2, 1, 2, 3, 4),
      by = c("A", "A", "B", "B", "B", "B")
      ),
    c(0.5, 0.5, 0.25, 0.25, 0.25, 0.25)
  )
})

test_that("get_tf_by() works with logical vectors", {
  expect_equal(
    get_tf_by(
      x = c(TRUE, FALSE, TRUE, TRUE),
      by = c("A", "A", "B", "B")),
    c(0.5, 0.5, 1, 1)
  )
})
