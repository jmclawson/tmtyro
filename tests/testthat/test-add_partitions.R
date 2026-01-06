test_that("add_partitions() creates partitions", {
  result <- make_sample_table() |>
    add_partitions(size = 3, label = FALSE) |>
    drop_log()

  expected <- c(1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 3)

  expect_s3_class(result, "data.frame")
  expect_equal(result$partition, expected)
})
