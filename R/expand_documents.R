#' Convert data frame from long tidy format to wider format
#'
#' The resulting data frame is a simpler form of the document feature matrix used by other packages. `my_df |> expand_documents(percent = FALSE, sort = FALSE)` compares to `my_df |> count(doc_id, word) |> tidytext::cast_dfm(doc_id, word, n)`, but it is not equivalent. The latter prepares a DFM to be used with the quanteda package.
#'
#' @param df A tidy data frame, potentially containing a column called "word"
#' @param feature A column of words containing one word per row, to be counted for frequency
#' @param by A column containing document grouping
#' @param percent Whether frequencies should be converted to percentages on a per-document basis
#' @param sort Whether to sort features by frequency
#' @param columns The features to keep
#'
#' @returns A data frame with one row per document and as many features as words.
#' @export
#'
#' @examples
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' dubliners |>
#'   expand_documents()
expand_documents <- function(df, feature = word, by = doc_id, percent = TRUE, sort = TRUE, columns = NULL) {
  if (!"n" %in% colnames(df)) {
    df <- df |>
      count({{ by }}, {{ feature }})
  }
  if (percent) {
    df <- df |>
      dplyr::mutate(
        n = n / sum(n, na.rm = TRUE),
        .by = c({{ by }}))
  }
  if (sort) {
    sort_it <- function(df, feature) {
      df <- df |>
        dplyr::mutate(
          {{ feature }} := {{ feature }} |>
            forcats::fct_reorder(
              .x = n,
              .fun = median,
              .na_rm = TRUE,
              .desc = TRUE)
        )
    }
  } else {
    sort_it <- function(df, feature){
      df
    }
  }

  df <- sort_it(df, {{ feature }})

  if (percent) {
    # df <- df |>
    #   dplyr::arrange({{ feature }}) |>
    #   tidyr::pivot_wider(
    #     names_from = {{ feature }},
    #     values_from = n)
    df <- df |>
      dplyr::arrange({{ feature }}) |>
      tidyr::pivot_wider(
        names_from = {{ feature }},
        values_from = n, values_fill = 0) |>
      tidyr::pivot_longer(
        cols = -c({{ by }}),
        names_to = deparse(substitute(feature)),
        values_to = "n") |>
      sort_it({{ feature }}) |>
      dplyr::arrange({{ feature }}) |>
      tidyr::pivot_wider(
        names_from = {{ feature }},
        values_from = n,
        values_fill = 0)
  } else {
    df <- df |>
      dplyr::arrange({{ feature }}) |>
      tidyr::pivot_wider(
        names_from = {{ feature }},
        values_from = n, values_fill = 0) |>
      tidyr::pivot_longer(
        cols = -c({{ by }}),
        names_to = deparse(substitute(feature)),
        values_to = "n") |>
      sort_it({{ feature }}) |>
      dplyr::arrange({{ feature }}) |>
      tidyr::pivot_wider(
        names_from = {{ feature }},
        values_from = n,
        values_fill = 0)
  }
  if (!is.null(columns)) {
    columns <- c(0, columns) + 1
    df |>
      dplyr::select(tidyr::all_of(columns)) |>
      add_class("expanded")
  } else {
    df |>
      add_class("expanded")
  }
}
