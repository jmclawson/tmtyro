#' Index document row numbers
#'
#' @param df A tidy data frame, divided by lines, words, or some other feature
#' @param by A grouping column identifying a document, such as `doc_id`
#' @param name The name to use for the added column
#'
#' @returns A data frame with one additional column
#' @export
#'
#' @examples
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' dubliners |>
#'   add_index() |>
#'   add_sentiment() |>
#'   drop_na() |>
#'   head()
add_index <- function(df, by = doc_id, name = word_index) {
  df <- df |>
    dplyr::mutate(
      {{ name }} := dplyr::row_number(),
      .by = {{ by }})
  if ("word" %in% colnames(df)) {
    df <- df |>
      dplyr::relocate(
      {{ name }}, .before = word)
    } else if ("text" %in% colnames(df)) {
      df <- df |>
        dplyr::relocate(
          {{ name }}, .before = text)
    }
  if ("original" %in% colnames(df)) {
    df <- df |>
      dplyr::relocate(
        {{ name }}, .before = original)
  }
  df
}
