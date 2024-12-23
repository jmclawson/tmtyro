test_that("get_match() works for basic uses", {
  lexicon <- data.frame(
    term = c("apple", "banana", "cherry"),
    value = c(1, 2, 3)) |>
    make_dictionary()

  x <- c("apple", "banana", "cherry", "apple")
  expect_equal(
    get_match(x, lexicon),
    c(1, 2, 3, 1)
  )
})

test_that("get_match() works with unmatched terms", {
  lexicon <- data.frame(
    term = c("apple", "banana", "cherry"),
    value = c(1, 2, 3)) |>
    make_dictionary()

  x <- c("apple", "pear", "cherry")
  expect_equal(
    get_match(x, lexicon),
    c(1, NA, 3)
  )
})

test_that("get_match() handles empty input", {
  lexicon <- data.frame(
    term = c("apple", "banana", "cherry"),
    value = c(1, 2, 3)) |>
    make_dictionary()

  x <- character(0)
  expect_equal(
    get_match(x, lexicon),
    numeric(0)
  )
})

test_that("get_match() works with NA values in the input", {
  lexicon <- data.frame(
    term = c("apple", "banana", "cherry"),
    value = c(1, 2, 3)) |>
    make_dictionary()

  x <- c("apple", NA, "banana")
  expect_equal(
    get_match(x, lexicon),
    c(1, NA, 2)
  )
  }
)

test_that("get_match() works with duplicate terms in the vector", {
  lexicon <- data.frame(
    term = c("apple", "banana", "cherry"),
    value = c(1, 2, 3)) |>
    make_dictionary()
  x <- c("apple", "apple", "banana")
  expect_equal(
    get_match(x, lexicon),
    c(1, 1, 2)
  )
})
