#' Prepare a table of data
#'
#' `tabulize()` provides a simple method for sharing results. Based on previous functions used, `tabulize()` will choose a method, resolving to one of a set of tables.
#'
#' @param .data data processed with one or more functions from `tmtyro`
#' @param ... optional parameters passed along to methods
#' @inheritDotParams tabulize.default summary:rows
#'
#' @returns A gt table data object
#' @family table helpers
#' @export
#'
#' @section Examples:
#' ```r
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' # A data frame with `doc_id` and `word` columns will show word counts by default
#' dubliners |>
#'    tabulize()
#' ```
#' \if{html}{\out{<div style="text-align: center">}\figure{tabulizer_default.png}{options: style="width:470px;max-width:32\%;"}\out{</div>}}
#'
#' ```r
#' # Applying tmtyro functions will prepare other tables
#'   dubliners |>
#'    add_vocabulary() |>
#'    tabulize()
#' ```
#' \if{html}{\out{<div style="text-align: center">}\figure{tabulizer_vocabulary.png}{options: style="width:900px;max-width:60\%;"}\out{</div>}}
#'
#' ```r
#'   dubliners |>
#'      dplyr::filter(doc_id == "The Dead") |>
#'      add_sentiment() |>
#'      tabulize()
#' ```
#' \if{html}{\out{<div style="text-align: center">}\figure{tabulizer_sentiment.png}{options: style="width:700px;max-width:50\%;"}\out{</div>}}
tabulize <- function(.data,...){
  UseMethod("tabulize")
}

#' Prepare a default table view
#'
#' @inheritParams tabulize
#' @param summary Indicates whether to prepare a summary table or the rows as they exist
#' @param inorder Indicates whether labels in the `doc_id` column should have their order preserved
#' @param count Determines whether frequencies will be counted for individual features
#' @param rows Chooses rows to be shown
#' @param ... optional parameters (unused)
#'
#' @keywords internal
#' @export
tabulize.default <- function(.data, summary = TRUE, inorder = TRUE, count = FALSE, rows = NULL, ...){
  if (is.null(rows)) rows <- 1:6

  if ("doc_id" %in% colnames(.data) && inorder) {
    .data <- .data |>
      dplyr::mutate(doc_id = forcats::fct_inorder(doc_id))
  }

  if (summary && !count &&
      "doc_id" %in% colnames(.data) &&
      "word" %in% colnames(.data)) {
    .data |>
      dplyr::count(doc_id) |>
      gt::gt() |>
      gt::fmt_number(columns = n, decimals = 0) |>
      gt::cols_label(
        doc_id = "",
        n = "words"
      )
  } else if (summary && !count &&
             "text" %in% colnames(.data)) {
    id_cols <- colnames(.data)[colnames(.data) %in% c("gutenberg_id", "doc_id", "title", "author")]
    .data |>
      dplyr::group_by(dplyr::across(tidyr::all_of(id_cols))) |>
      dplyr::summarize(n = dplyr::n()) |>
      dplyr::ungroup() |>
      gt::gt() |>
      gt::fmt_number(columns = n, decimals = 0) |>
      gt::sub_missing()
  } else if (count &&
             "doc_id" %in% colnames(.data) &&
             "word" %in% colnames(.data)) {
    .data |>
      dplyr::count(doc_id, word, sort = TRUE) |>
      dplyr::slice(rows, .by = doc_id) |>
      gt::gt() |>
      gt::fmt_number(columns = n, decimals = 0) |>
      gt::sub_missing() |>
      collapse_rows(doc_id) |>
      gt::cols_label(doc_id = "")
  } else {
    .data |>
      dplyr::slice(rows) |>
      gt::gt() |>
      gt::sub_missing()
  }
}

#' Prepare a table of lexical variety
#'
#' @inheritParams tabulize
#' @inheritParams tabulize.default
#' @param digits The number of digits to show past the decimal point
#'
#' @keywords internal
#' @export
tabulize.vocabulary <- function(.data, digits = 3, ...) {
  .data |>
    dplyr::summarize(
      words = dplyr::n(),
      vocabulary = sum(new_word),
      hapax = sum(hapax),
      ttr = vocabulary / words,
      htr = hapax / words,
      .by = doc_id) |>
    dplyr::relocate(hapax, .before = htr) |>
    dplyr::rename(vocab = vocabulary,
                  hlemona = hapax) |>
    gt::gt() |>
    gt::fmt_number(columns = c(words, vocab, hlemona), decimals = 0) |>
    gt::fmt_number(columns = c(ttr, htr), decimals = digits) |>
    gt::cols_label(
      doc_id = "",
      vocab = "total",
      hlemona = "total",
      htr = "ratio",
      ttr = "ratio",
      words = "length") |>
    gt::tab_spanner("vocabulary", columns = vocab:ttr) |>
    gt::tab_spanner("hapax", columns = hlemona:htr)
}

#' Prepare a table for sentiment analysis
#'
#' @inheritParams tabulize
#' @inheritParams tabulize.default
#' @inheritParams tabulize.vocabulary
#' @param drop_empty Removes rows lacking sentiment
#' @param ignore Removes rows matching set sentiments
#' @param count Determines whether frequencies will be counted for sentiments
#'
#' @keywords internal
#' @export
tabulize.sentiment <- function(.data, inorder = TRUE, digits = 2, drop_empty = FALSE, ignore = NULL, rows = NULL, count = TRUE, ...) {
  if (is.null(rows)) rows <- 1:6
  if ("doc_id" %in% colnames(.data) && inorder) {
    .data <- .data |>
      dplyr::mutate(doc_id = forcats::fct_inorder(doc_id))
  }
  if (!is.null(ignore) && "sentiment" %in% colnames(.data)) {
    .data <- .data |>
      dplyr::mutate(
        sentiment = dplyr::case_when(
          sentiment %in% ignore ~ NA_character_,
          TRUE ~ sentiment
        ))
  }
  if (drop_empty) {
    .data <- .data |>
      tidyr::drop_na(sentiment)
  }
  if (count &&
      "sentiment" %in% colnames(.data) && # nrc_vad uses sentiment_valence
      is.character(.data$sentiment)) { # afinn is double
     .data |>
      dplyr::count(doc_id, sentiment) |>
      dplyr::mutate(percent = 100 * n / sum(n),
                    .by = doc_id) |>
      gt::gt() |>
      gt::fmt_number(columns = n, decimals = 0) |>
      gt::fmt_number(columns = percent, decimals = digits) |>
      gt::sub_missing() |>
      collapse_rows(doc_id) |>
      gt::cols_label(
        doc_id = "",
        percent = "%"
      )
  } else if (count &&
             "sentiment_valence" %in% colnames(.data)) {
    .data |>
      dplyr::summarize(
        valence = mean(sentiment_valence, na.rm = TRUE),
        .by = doc_id) |>
      gt::gt() |>
      gt::fmt_number(columns = valence, decimals = digits) |>
      gt::cols_label(
        doc_id = "",
        valence = "average valence")
  } else if (count &&
             "sentiment" %in% colnames(.data) &&
             is.double(.data$sentiment)) {
    .data |>
      dplyr::summarize(
        sentiment = mean(sentiment, na.rm = TRUE),
        .by = doc_id) |>
      gt::gt() |>
      gt::fmt_number(columns = sentiment, decimals = digits) |>
      gt::cols_label(
        doc_id = "",
        sentiment = "average sentiment")
  } else {
    .data |>
      dplyr::slice(rows) |>
      gt::gt() |>
      gt::sub_missing()
  }
}

#' Prepare a table of term frequency--inverse document frequency
#'
#' @inheritParams tabulize
#' @inheritParams tabulize.default
#' @inheritParams tabulize.vocabulary
#'
#' @keywords internal
#' @export
tabulize.tf_idf <- function(.data, rows = NULL, digits = 5, ...) {
  if (is.null(rows)) rows <- 1:6
  .data |>
    dplyr::slice(rows, .by = doc_id) |>
    dplyr::arrange(doc_id) |>
    gt::gt() |>
    gt::cols_label(doc_id = "") |>
    gt::fmt_number(n, decimals = 0) |>
    gt::fmt_number(tf:tf_idf, decimals = digits) |>
    collapse_rows(doc_id)
}

#' Prepare a table of n-gram frequencies
#'
#' @inheritParams tabulize
#' @inheritParams tabulize.default
#' @inheritParams tabulize.vocabulary
#'
#' @keywords internal
#' @export
tabulize.ngrams <- function(.data, ...) {
  .data |>
    combine_ngrams() |>
    tabulize.combined_ngrams(...)
}

#' Prepare a table of n-gram frequencies
#'
#' @inheritParams tabulize
#' @inheritParams tabulize.default
#' @inheritParams tabulize.vocabulary
#'
#' @keywords internal
#' @export
tabulize.combined_ngrams <- function(.data, rows = NULL, count = TRUE, digits = 2, ...) {
  if (is.null(rows)) rows <- 1:6
  if (count) {
    .data |>
      dplyr::count(doc_id, ngram, sort = TRUE) |>
      dplyr::mutate(percent = 100 * n / sum(n), .by = doc_id) |>
      dplyr::slice(rows, .by = doc_id) |>
      gt::gt() |>
      gt::fmt_number(columns = n, decimals = 0) |>
      gt::fmt_number(columns = percent, decimals = digits) |>
      gt::sub_missing() |>
      collapse_rows(doc_id) |>
      gt::cols_label(
        doc_id = "",
        percent = "%")
  } else {
    .data |>
      dplyr::slice(rows) |>
      gt::gt() |>
      gt::sub_missing()
  }
}

#' Collapse gt rows in the style of kableExtra
#'
#' @param df_g A gt table data object
#' @param col The column to collapse
#' @param lookleft Whether to depend any collapsing on the column one step left
#'
#' @returns A gt table data object
#' @family table helpers
#' @export
#'
#' @section Examples:
#' ```r
#' library(gt)
#' library(palmerpenguins)
#' library(tmtyro)
#'
#' penguins_gt <-
#'   penguins |>
#'   select(-year) |>
#'   summarize(
#'     across(
#'       ends_with("_mm"), mean, na.rm = TRUE),
#'     .by = c(species, island, sex)) |>
#'   gt() |>
#'   fmt_number() |>
#'   tab_spanner(
#'     "bill",
#'     columns = starts_with("bill_")) |>
#'   tab_spanner(
#'     "flipper",
#'     starts_with("flip")) |>
#'   cols_label(
#'     bill_length_mm = "length",
#'     bill_depth_mm = "depth",
#'     flipper_length_mm = "length") |>
#'   sub_missing()
#'
#' penguins_gt |>
#'   collapse_rows(species) |>
#'   collapse_rows(island)
#' ```
#' \if{html}{\out{<div style="text-align: center">}\figure{collapse_rows.png}{options: style="width:800px;max-width:57\%;"}\out{</div>}}
collapse_rows <- function(df_g, col, lookleft = TRUE){
  col_num <- grep(deparse(substitute(col)), colnames(df_g$`_data`))

  collapse_style <- gt::css(visibility = "hidden",
                        border_top = "0px")

  test_rows <- function(x) ifelse(is.na(x == dplyr::lag(x)), FALSE, x == dplyr::lag(x))

  if(col_num > 1 & lookleft) {
    col_left <- as.name(colnames(df_g$`_data`)[col_num - 1])
    df_g |>
      gt::tab_style(
        style = collapse_style,
        locations = gt::cells_body(columns = {{ col }},
                               rows = test_rows({{ col }}) & test_rows({{ col_left }})))
  } else {
    df_g |>
      gt::tab_style(
        style = collapse_style,
        locations = gt::cells_body(columns = {{ col }},
                               rows = test_rows({{ col }})))
  }
}
