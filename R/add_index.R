#' Index document row numbers
#'
#' @param data A tidy data frame, divided by lines, words, or some other feature
#' @param by A grouping column identifying a document, such as `doc_id`
#' @param name The name to use for the added column
#' @param label Whether to label variables added to data frame
#'
#' @returns A data frame with one additional column
#' @export
#'
#' @examples
#' \dontrun{
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
#' }
add_index <- function(data, by = doc_id, name = word_index, label = NULL) {
  data <- data |>
    dplyr::mutate(
      {{ name }} := dplyr::row_number(),
      .by = {{ by }})
  if ("word" %in% colnames(data)) {
    data <- data |>
      dplyr::relocate(
      {{ name }}, .before = word)
    } else if ("text" %in% colnames(data)) {
      data <- data |>
        dplyr::relocate(
          {{ name }}, .before = text)
    }
  if ("original" %in% colnames(data)) {
    data <- data |>
      dplyr::relocate(
        {{ name }}, .before = original)
  }

  if (tmtyro_use_labels(label)) {
    index_name <- deparse(substitute(name))
    data <- data |>
      assign_label_from(
        index_name,
        deparse(substitute(feature)),
        from_var = "index")
  }

  if (tmtyro_use_log()) {
    data <- data |>
      add_logstep(
        fn = "add_index",
        arguments = c(by = doc_id))
  }
  data
}
