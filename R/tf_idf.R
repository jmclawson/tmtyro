#' Compare usage across a corpus
#'
#' `summarize_tf_idf()` prepares a summary table for each term in a corpus, including their frequencies by document and "tf-idf" measurements for comparing the relative importance in comparison to other documents in a set.
#'
#' @param df A tidy data frame, potentially containing columns called "doc_id" and "word"
#' @param by A column containing document grouping
#' @param feature A column containing the terms to be measured across document groupings
#'
#' @returns A summary of the original data frame, with rows for each document and term pairing and columns for document identifier, term, n (the number of times this term was used in this document), tf (term's frequency in this document), idf (inverse document frequency), and tf_idf (previous two columns combined).
#' @export
#'
#' @examples
#' library(tmtyro)
#' library(dplyr)
#'
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen |>
#'   summarize_tf_idf()
summarize_tf_idf <- function(df, by = doc_id, feature = word) {
  df |>
    dplyr::count({{ by }}, {{ feature }}) |>
    tidytext::bind_tf_idf(
      term = {{ feature }},
      document = {{ by }},
      n = n) |>
    dplyr::arrange(dplyr::desc(tf_idf))
}

#' Visualize the top terms by tf-idf
#'
#' @param df A tidy data frame, potentially containing columns called "doc_id" and "word"
#' @param num The number of terms to chart in each document
#' @param by A column containing document grouping
#' @param feature A column containing the terms to be measured across document groupings
#' @param label Not yet working
#' @param label_tweak Not yet working
#' @param label_inside Not yet working
#' @param colorset The color palette to use, whether "default", "okabe", or one of the named qualitative palettes from Viridis or Color Brewer
#' @param outline_color The color to use for the outside of each bar. By default, no color is used.
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' library(tmtyro)
#' library(dplyr)
#'
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen |>
#'   plot_tf_idf()
#'
#' austen |>
#'   filter(pos %in% c("NN", "NNS")) |>
#'   plot_tf_idf(feature = lemma)
plot_tf_idf <- function(
    df,
    num = 10,
    by = doc_id,
    feature = word,
    label = FALSE,
    label_tweak = 2,
    label_inside = FALSE,
    colorset = "default",
    outline_color = NA
){
  if (!"tf_idf" %in% colnames(df)) {
    df <- df |>
      summarize_tf_idf(by = {{ by }}, feature = {{ feature }})
  }

  df <- df |>
    dplyr::slice_max(
      order_by = tf_idf,
      n = num,
      by = {{ by }}) |>
    dplyr::group_by({{ by }}) |>
    dplyr::arrange(dplyr::desc(tf_idf)) |>
    dplyr::mutate(
      {{ feature }} := tidytext::reorder_within(
        {{ feature }},
        by = tf_idf,
        within = {{ by }})) |>
    dplyr::ungroup()

  if (deparse(substitute(feature)) == "word") {
    prefix <- "term"
  } else {
    prefix <- deparse(substitute(feature))
  }

  x_lab <- paste(prefix, "frequency–inverse document frequency")

  df |>
    internal_plot_word_bars(
    tf_idf, rlang::enquo(by), rlang::enquo(feature), FALSE, label, label_tweak, label_inside, colorset, outline_color) +
    ggplot2::labs(x = x_lab)
}
