test_that("`add_frequency()` adds a column of counts", {
  tbl_checked <- make_sample_table() |>
    add_frequency(label = FALSE)

  attr(tbl_checked, "tmtyro_log") <- NULL

  predicted <- c(rep(1, 9), 2, 1, 1, 1, 2, 1, 1)

  expect_equal(tbl_checked$n, predicted)
})
