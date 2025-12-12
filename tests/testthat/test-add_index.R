test_that("add_index() numbers the rows", {
  result <- make_sample_table() |>
    add_index(label = FALSE) |>
    drop_log()

  expected <- c(1:4, 1:5, 1:7)

  expect_s3_class(result, "tmtyro")
  expect_equal(result$word_index, expected)
})

test_that("add_index() numbers rows with text column", {
  result <- make_sample_texts() |>
    add_index(label = FALSE) |>
    drop_log()

  expected <- c(1,1,1)

  expect_s3_class(result, "data.frame")
  expect_equal(result$word_index, expected)
})

test_that("add_index() numbers rows with original column", {
  result <- make_sample_texts() |>
    load_texts(keep_original = TRUE) |>
    add_index(label = FALSE) |>
    drop_log()

  expected <- c(1:4, 1:5, 1:7)

  expect_s3_class(result, "data.frame")
  expect_equal(result$word_index, expected)
})
