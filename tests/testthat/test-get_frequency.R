test_that("get_frequency() returns basic counts and ratios correctly", {
  expect_equal(
    get_frequency(c("A", "B", "C", "A", "D", "B", "A", "A")),
    c(4, 2, 1, 4, 1, 2, 4, 4)
  )

  expect_equal(
    get_frequency(c("A", "B", "C", "A", "D", "B", "A", "A"), percent = TRUE),
    c(0.5, 0.25, 0.125, 0.5, 0.125, 0.25, 0.5, 0.5)
  )
})

test_that("get_frequency() handles empty vectors", {
  expect_equal(get_frequency(character(0)), integer(0))

  expect_equal(get_frequency(character(0), percent = TRUE), numeric(0))
})

test_that("get_frequency() handles vectors with one value", {
  expect_equal(get_frequency(c("A")), c(1))

  expect_equal(get_frequency(c("A"), percent = TRUE), c(1))
})

test_that("get_frequency() works with numeric vectors", {
  expect_equal(
    get_frequency(c(1, 2, 2, 3, 3, 3)),
    c(1, 2, 2, 3, 3, 3)
  )

  expect_equal(
    round(get_frequency(c(1, 2, 2, 3, 3, 3), percent = TRUE), 2),
    c(0.17, 0.33, 0.33, 0.50, 0.50, 0.50)
  )
})

test_that("get_frequency() returns counts and ratios for logical vectors", {
  expect_equal(
    get_frequency(c(TRUE, FALSE, TRUE, TRUE)),
    c(3, 1, 3, 3)
  )

  expect_equal(
    get_frequency(c(TRUE, FALSE, TRUE, TRUE), percent = TRUE),
    c(0.75, 0.25, 0.75, 0.75)
  )
})

test_that("get_frequency() shows case sensitivity", {
  expect_equal(
    get_frequency(c("a", "A", "a")),
    c(2, 1, 2)
  )

  expect_equal(
    get_frequency(c("a", "A", "a", "b"), percent = TRUE),
    c(0.5, 0.25, 0.5, 0.25)
  )
})
