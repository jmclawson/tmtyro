test_that("standardize_titles() converts character vector to title case", {
  title_vector <- c("A HAPPY ENDING", "THE FINAL STORY", "Some other title")

  result <- standardize_titles(title_vector) |>
    drop_log()

  expected <- c("A Happy Ending", "The Final Story", "Some Other Title")

  expect_equal(result, expected)
})

test_that("standardize_titles(drop_articles = TRUE) removes opening articles in character vector", {
  title_vector <- c("A HAPPY ENDING", "THE FINAL STORY", "Some other title")

  result <- standardize_titles(title_vector, drop_articles = TRUE) |>
    drop_log()

  expected <- c("Happy Ending", "Final Story", "Some Other Title")

  expect_equal(result, expected)
})

test_that("standardize_titles(drop_articles = FALSE) keeps opening articles in character vector", {
  title_vector <- c("A HAPPY ENDING", "THE FINAL STORY", "Some other title")

  result <- standardize_titles(title_vector, drop_articles = FALSE) |>
    drop_log()

  expected <- c("A Happy Ending", "The Final Story", "Some Other Title")

  expect_equal(result, expected)
})

test_that("standardize_titles() converts factor vector to title case", {
  title_vector <- c("A HAPPY ENDING", "THE FINAL STORY", "Some other title") |>
    factor()

  result <- standardize_titles(title_vector) |>
    drop_log()

  expected <- c("A Happy Ending", "The Final Story", "Some Other Title") |>
    factor()

  expect_equal(result, expected)
})

test_that("standardize_titles() converts dataframe to title case", {
  title_vector <- c(A = "A HAPPY ENDING", B = "THE FINAL STORY", C = "Some other title")

  result <- make_sample_table() |>
    dplyr::mutate(doc_id = doc_id |>
             stringr::str_replace_all(title_vector)) |>
    standardize_titles() |>
    drop_log()

  expected <- c("A Happy Ending", "The Final Story", "Some Other Title")

  expect_s3_class(result, "tmtyro")
  expect_equal(unique(result$doc_id), expected)
})

test_that("standardize_titles(drop_articles = TRUE) drops articles in dataframe", {
  title_vector <- c(A = "A HAPPY ENDING", B = "THE FINAL STORY", C = "Some other title")

  result <- make_sample_table() |>
    dplyr::mutate(doc_id = doc_id |>
                    stringr::str_replace_all(title_vector)) |>
    standardize_titles(drop_articles = TRUE) |>
    drop_log()

  expected <- c("Happy Ending", "Final Story", "Some Other Title")

  expect_no_warning(result)
  expect_s3_class(result, "tmtyro")
  expect_equal(unique(result$doc_id), expected)
})

test_that("standardize_titles() converts dataframe with different column", {
  title_vector <- c(A = "A HAPPY ENDING", B = "THE FINAL STORY", C = "Some other title")

  result <- make_sample_table() |>
    dplyr::mutate(doc_id = doc_id |>
                    stringr::str_replace_all(title_vector)) |>
    dplyr::rename(my_title = doc_id) |>
    standardize_titles(title = my_title) |>
    drop_log()

  expected <- c("A Happy Ending", "The Final Story", "Some Other Title")

  expect_s3_class(result, "tmtyro")
  expect_equal(unique(result$my_title), expected)
})
