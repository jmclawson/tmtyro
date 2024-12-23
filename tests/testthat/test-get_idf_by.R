test_that("get_idf_by() computes basic category-based frequencies correctly", {
  expect_equal(
    round(get_idf_by(
      x = c("A", "A", "B", "A", "B", "C"),
      by = c("X", rep("Y", 5))), 2),
    c(0, 0, 0.69, 0, 0.69, 0.69)
  )
})

test_that("get_idf_by() returns 0 for single category", {
  expect_equal(
    get_idf_by(
      x = c("A", "B", "C", "A", "B"),
      by = c("X", "X", "X", "X", "X")),
    c(0, 0, 0, 0, 0)
  )
})

test_that("get_idf_by() computes frequencies with equal numbers of terms and categories", {
  expect_equal(
    round(get_idf_by(
      x = c("A", "B", "C"),
      by = c("X", "Y", "Z")), 3),
    c(1.099, 1.099, 1.099)
  )
})

test_that("get_idf_by() returns an error for mismatched vector lengths", {
  expect_error(
    get_idf_by(
      x = c("A", "B", "C"),
      by = c("X", "X")),
    "`x` and `by` must be vectors of the same length."
  )
})

test_that("get_idf_by() works with numeric vectors", {
  expect_equal(
    round(get_idf_by(
      x = c(1, 2, 1, 2, 3, 4),
      by = c("A", "A", "B", "B", "B", "B")
    ), 2),
    c(0, 0, 0, 0, 0.69, 0.69)
  )
})

test_that("get_idf_by() works with logical vectors", {
  expect_equal(
    round(get_idf_by(
      x = as.character(c(TRUE, FALSE, TRUE, TRUE)),
      by = c("A", "A", "B", "B")), 2),
    c(0, 0.69, 0, 0)
  )
})
