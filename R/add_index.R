#' Track progress in documents
#'
#' @param data A tidy data frame, divided by lines, words, or some other feature
#' @param by A grouping column identifying a document, such as `doc_id`
#' @param unit Unit for measuring progress, either as a percentage or as an index of row number
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
add_progress <- function(data, by = doc_id, unit = c("percentage", "index"), name = progress, label = NULL) {
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
  unit <- unit[1]
  if (unit %in% c("percent", "percentage")) {
    data <- data |>
      dplyr::mutate(
        {{ name }} := {{ name }} / max({{ name }}, na.rm = TRUE),
        .by = {{ by }})
  }

  if (tmtyro_use_labels(label)) {
    # browser()
    # index_column <- deparse(substitute(name))
    # feature_name <- deparse(substitute(feature))
    index_column <- rlang::as_name(rlang::ensym(name))
    unit <- unit |>
      stringr::str_replace_all(
        "percent(age)?", "%")
    use_var <- ifelse(
      unit == "index",
      "progress",
      "progress_unit")

    data <- data |>
      assign_label_from(
        index_column,
        deparse(substitute(feature)),
        secondary = unit,
        from_var = use_var)
  }

  if (tmtyro_use_log()) {
    data <- data |>
      add_logstep(
        fn = "add_progress",
        arguments = list(by = doc_id))
  }
  data
}

#' @rdname add_progress
#' @export
add_index <- function(data, by = doc_id, name = word_index, label = NULL) {
  add_progress(data, by = {{ by }}, unit = "index", name = {{ name }}, label = label)
}


