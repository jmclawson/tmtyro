#' Divide documents in equal lengths
#'
#' @param df A tidy data frame, potentially containing a column called "word"
#' @param length Size of each partition
#' @param by A column containing document grouping
#'
#' @returns The original data frame with a column added for partition.
#' @export
#'
#' @examples
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' dubliners |>
#'   add_partitions() |>
#'   head()
add_partitions <- function(
    df,
    length = 1000,
    # overlap = NULL,
    by = doc_id) {
  df |>
    dplyr::mutate(
      partition = floor((dplyr::row_number() - 1) / length) + 1,
      .by = {{ by }})
}
