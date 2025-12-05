test_that("empty `drop_stopwords()` works by default", {
  tbl_checked <- make_sample_table() |>
    drop_stopwords()

  predicted <- data.frame(
    doc_id = c("A", "A", "B", "B", "C", "C", "C"),
    word = c("cat", "bad", "dog", "good", "lavish", "lizard", "bad")
  ) |>
    add_class("tmtyro")

  expect_equal(tbl_checked, predicted)
})

# Future tests:
# - works with different wordlists
