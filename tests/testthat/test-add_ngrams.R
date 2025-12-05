test_that("`add_ngrams()` works as expected", {
  tbl_checked <- make_sample_table() |>
    add_ngrams(label = FALSE) |>
    head()

  attr(tbl_checked, "tmtyro_log") <- NULL

  predicted <- data.frame(
    doc_id = c("A", "A", "A", "A", "B", "B"),
    word_1 = c("the", "cat", "was", "bad", "the", "dog"),
    word_2 = c("cat", "was", "bad", NA, "dog", "was")
  ) |>
    tibble::as_tibble() |>
    add_class("ngrams")

  expect_equal(tbl_checked, predicted)
})
