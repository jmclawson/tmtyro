test_that("empty `add_sentiment()` works by default", {
  tbl_checked <- make_sample_table() |>
    add_sentiment(label = FALSE)

  attr(tbl_checked, "tmtyro_log") <- NULL

  predicted <- c(
    NA, NA, NA, "negative",
    NA, NA, NA, NA, "positive",
    NA, "positive", NA, NA, NA, NA, "negative")

  expect_equal(tbl_checked$sentiment, predicted)
})

# Future tests:
# - works with other dictionaries
