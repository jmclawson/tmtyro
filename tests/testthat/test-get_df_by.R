test_that("get_df_by() returns basic category-based frequencies correctly", {
  expect_equal(
    as.numeric(get_df_by(
      x = c("A", "A", "B", "A", "B", "C"),
      by = c("X", rep("Y", 5)))),
    c(1, 1, 0.5, 1, 0.5, 0.5)
  )
})

test_that("get_df_by(percent = FALSE) counts basic category-based frequencies correctly", {
  expect_equal(
    as.numeric(get_df_by(
      x = c("A", "A", "B", "A", "B", "C"),
      by = c("X", rep("Y", 5)), percent = FALSE)),
    c(2, 2, 1, 2, 1, 1)
  )
})

test_that("get_df_by() returns an error for mismatched vector lengths", {
  expect_error(
    get_df_by(
      x = c("A", "B", "C"),
      by = c("X", "X")),
    "`x` and `by` must be vectors of the same length."
  )
})

test_that("get_df_by() works with numeric vectors", {
  expect_equal(
    as.numeric(get_df_by(
      x = c(1, 2, 1, 2, 3, 4),
      by = c("A", "A", "B", "B", "B", "B")
    )),
    c(1, 1, 1, 1, 0.5, 0.5)
  )
})

test_that("get_df_by() works with logical vectors", {
  expect_equal(
    as.numeric(get_df_by(
      x = as.character(c(TRUE, FALSE, TRUE, TRUE)),
      by = c("A", "A", "B", "B"))),
    c(1, 0.5, 1, 1)
  )
})
