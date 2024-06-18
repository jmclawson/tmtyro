#' Show a term in context
#'
#' @param df A data frame which most likely contains a column called "word"
#' @param term The term to search for, exactly
#' @param window The number of terms to show before and after
#' @param limit The number of results to return in the console using cli, if installed
#' @param by The document identifier, for limiting context window
#' @param feature The column to show for context. When `NULL`, `contextualize()` looks first for an "original" column and then for a "word" column.
#' @param match The column to use for matching
#' @param regex When defined, a regular expression for searches using greater control
#'
#' @details
#' # Hiding results
#' `contextualize()` uses [cli::style_underline()] and [fansi::to_html()], if these packages are installed, to show formatted results in the console or in a document rendered to HTML. These formatted results can be hidden by setting `limit = 0` in the function, by suppressing messages with [`suppressMessages()`] in the console, or by setting a `message: false` chunk option in Quarto or R Markdown.
#'
#' @returns Invisibly, a data frame with four columns: `<by>`, `<match>`, "index", and "context"
#' @export
#'
#' @examples
#' dubliners <- get_gutenberg_corpus(2814) |>
#'     load_texts(keep_original = TRUE)
#'
#' contextualize(dubliners, regex = "dog[s]?$")
contextualize <- function(df, term, window = 3, limit = 1:5, by = doc_id, feature = NULL, match = word, regex = NULL) {
  feature_str <- deparse(substitute(feature))
  if (length(limit) == 1 &
      any(is.na(limit), limit == 0)) {
    limit <- NA
  }
  if (feature_str=="NULL" & "original" %in% colnames(df)) {
    feature <- rlang::sym("original")
    feature_str <- "original"
  } else if (feature_str=="NULL" & "word" %in% colnames(df)) {
    feature <- rlang::sym("word")
    feature_str <- "word"
  }
  index_str <- paste0("index")

  if (is.null(regex)) {
    df <- df |>
      add_index({{ by }}, name = !!rlang::sym(index_str)) |>
      add_ngrams(n = -window:window,
                 feature = feature_str,
                 keep = TRUE) |>
      dplyr::filter(tolower({{ match }}) == tolower(term))
    df_t <- df
    df_t[[paste0(feature_str, "_0")]] <- toupper(df_t[[paste0(feature_str, "_0")]])
  } else {
    df <- df |>
      add_index({{ by }}, name = !!rlang::sym(index_str)) |>
      add_ngrams(n = -window:window,
                 feature = {{ feature }},
                 keep = TRUE) |>
      dplyr::filter(stringr::str_detect({{ match }}, regex))
    regex_str <- regex |>
      {\(x) if (stringr::str_detect(x, "^[[\\^]]")) {
        nohat <- paste0("-", stringr::str_remove_all(x, "[[\\^]]"))
        paste0(x, "|", nohat)
      } else {x}}() |>
      {\(x) if (stringr::str_detect(x, "[[\\$]]$")) {
        nobuck <- paste0(stringr::str_remove_all(x, "[[\\$]]$"), "-")
        paste0(x, "|", nobuck)
      } else {x}}()
    df_t <- df
    df_t <- df_t |>
      dplyr::mutate(dplyr::across(
        dplyr::starts_with(paste0(feature_str, "_")),
        .fns = {\(x) stringr::str_replace_all(x, regex_str, toupper)}
      ))
  }

  df <- df |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::starts_with(paste0(feature_str, "_0")),
      .fn = {\(x) ifelse(is.na(x), "-", x)})) |>
    combine_ngrams() |>
    dplyr::mutate(
      ngram = ngram |>
        stringr::str_replace_all(" NA ", " ") |>
        stringr::str_replace_all("\\bNA\\b", "") |>
        trimws()) |>
    dplyr::rename(context = ngram) |>
    dplyr::select({{ by }}, {{ match }}, index_str, context)

  df_t <- df_t |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::starts_with(paste0(feature_str, "_0")),
      .fn = {\(x) ifelse(is.na(x), "-", x)})) |>
    combine_ngrams() |>
    dplyr::mutate(
      ngram = ngram |>
        stringr::str_replace_all(" NA ", " ") |>
        stringr::str_replace_all("\\bNA\\b", "") |>
        trimws()) |>
    dplyr::rename(context = ngram) |>
    dplyr::select({{ by }}, {{ match }}, index_str, context)

  if (is.null(regex)) regex <- term
  regex <- regex |>
    stringr::str_replace_all("[\\^]", "") |>
    stringr::str_replace_all("[$]", "")
  if (rlang::is_installed("cli") &
      length(limit) > 1 |
      length(limit) == 1 && !is.na(limit)) {
    if (knitr::is_html_output() &
        rlang::is_installed("fansi")) {
      results <-  df |>
        dplyr::pull(context) |>
        {\(x) x[limit[limit %in% 1:length(x)]]}() |>
        stringr::str_replace_all(
          stringr::regex(regex, ignore_case = TRUE),
          function(x) cli::col_magenta(cli::style_underline(x))) |>
        cli::cli_bullets() |>
        fansi::to_html()
    } else {
      results <-  df |>
        dplyr::pull(context) |>
        {\(x) x[limit[limit %in% 1:length(x)]]}() |>
        stringr::str_replace_all(
          stringr::regex(regex, ignore_case = TRUE),
          function(x) cli::col_magenta(cli::style_underline(x))) |>
        cli::cli_bullets()
    }
    invisible(df_t)
  } else {
    df_t
  }

}
