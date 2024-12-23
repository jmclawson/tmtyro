#' Add frequency of words or other features
#'
#' @param df A tidy data frame, potentially containing a column called "word"
#' @param feature The feature to count in each document
#' @param by A grouping column identifying a document, such as `doc_id`.
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
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' dubliners |>
#'   add_frequency() |>
#'   head()
add_frequency <- function(df, feature = word, by = doc_id){
  df |>
    dplyr::group_by({{ by }}, {{ feature }}) |>
    dplyr::mutate(
      n = dplyr::n()) |>
    dplyr::ungroup() |>
    add_class("frequency") |>
    set_feature(deparse(substitute(feature)))
}
