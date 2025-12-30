#' Add frequency of words or other features
#'
#' @param data A tidy data frame, potentially containing a column called "word"
#' @param feature The feature to count in each document
#' @param by A grouping column identifying a document, such as `doc_id`
#' @param label Whether to label variables added to data frame
#'
#' @returns The original data frame with a column added for frequency
#' @export
#'
#' @examples
#' \dontrun{
#'   my_corpus <- load_texts()
#'
#'   my_bigrams <- my_corpus |>
#'     add_frequency()
#' }
#'
#' \dontrun{
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' dubliners |>
#'   add_frequency() |>
#'   head()
#' }

add_frequency <- function(data, feature = word, by = doc_id, label = NULL){
  tmtyro_log <- attr(data, "tmtyro_log")
  out <- data |>
    dplyr::group_by({{ by }}, {{ feature }}) |>
    dplyr::mutate(
      n = dplyr::n()) |>
    dplyr::ungroup() |>
    add_class("frequency")# |>
    # set_feature(deparse(substitute(feature)))

  feature_string <- deparse(substitute(feature))
  by_string <- deparse(substitute(by))

  if (tmtyro_use_labels(label)) {
    out <- out |>
      assign_labels(
        c("n"),
        feature_string,
        by_string
        )
  }

  if (tmtyro_use_log()) {
    attr(out, "tmtyro_log") <- attr(data, "tmtyro_log")
    out <- out |>
      add_logstep(
        fn = "add_frequency",
        arguments = list(
          feature = feature_string,
          by = by_string))
  }

  out |>
    add_class("frequency")
}
