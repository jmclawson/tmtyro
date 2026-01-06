test_that("`load_texts()` works with a data frame of texts", {
  tbl_checked <- make_sample_texts() |>
    load_texts()

  attr(tbl_checked, "tmtyro_log") <- NULL

  predicted <- make_sample_table()

  expect_equal(tbl_checked, predicted)
})

# Future tests:
# - works with files
# - arguments work
