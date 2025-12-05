#' Compare usage across a corpus
#'
#' `add_tf_idf()` adds measurements including term frequency by document and "tf-idf" measurements for weighing relative importance in comparison to other documents in a set.
#'
#' @param data A tidy data frame, potentially containing columns called "doc_id" and "word"
#' @param by A column containing document grouping
#' @param feature A column containing the terms to be measured across document groupings
#' @param label Whether to label variables added to data frame
#'
#' @returns The original data frame with additional columns added for n, (the number of times a term was used in this document), tf (term's frequency in this document), idf (inverse document frequency), and tf_idf (previous two columns combined).
#' @family tf_idf helpers
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
#'   add_tf_idf()
#' }
add_tf_idf <- function(data, by = doc_id, feature = word, label = NULL) {
  df_tfidf <- data |>
    summarize_tf_idf({{ by }}, {{ feature }})

  out <- data |>
    dplyr::left_join(
      df_tfidf,
      by = dplyr::join_by({{ by }}, {{ feature }})) |>
    add_class("tf_idf")

  if (tmtyro_use_labels(label)) {
    out <- out |>
      assign_labels(
        c("n", "tf", "idf", "tf_idf"),
        deparse(substitute(feature)),
        deparse(substitute(by)))
  }

  if (tmtyro_use_log()) {
    out <- out |>
      add_logstep(
        fn = "add_tf_idf",
        arguments = c(
          by = deparse(substitute(by)),
          feature = deparse(substitute(feature))
        ))
  }

  out
}

#' Compare usage across a corpus
#'
#' `summarize_tf_idf()` prepares a summary table for each term in a corpus, including their frequencies by document and "tf-idf" measurements for comparing the relative importance in comparison to other documents in a set.
#'
#' @param data A tidy data frame, potentially containing columns called "doc_id" and "word"
#' @param by A column containing document grouping
#' @param feature A column containing the terms to be measured across document groupings
#'
#' @returns A summary of the original data frame, with rows for each document and term pairing and columns for document identifier, term, n (the number of times this term was used in this document), tf (term's frequency in this document), idf (inverse document frequency), and tf_idf (previous two columns combined).
#' @family tf_idf helpers
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
#'   summarize_tf_idf()
#' }
summarize_tf_idf <- function(data, by = doc_id, feature = word) {
  data |>
    dplyr::count({{ by }}, {{ feature }}) |>
    dplyr::ungroup() |>
    tidytext::bind_tf_idf(
      term = {{ feature }},
      document = {{ by }},
      n = n) |>
    dplyr::arrange(dplyr::desc(tf_idf)) |>
    add_class("tf_idf")
}

#' Visualize the top terms by tf-idf
#'
#' @param data A tidy data frame, potentially containing columns called "doc_id" and "word"
#' @param rows The rows of terms to chart in each document
#' @param by A column containing document grouping
#' @param feature A column containing the terms to be measured across document groupings
#' @param label Not yet working
#' @param label_tweak Not yet working
#' @param label_inside Not yet working
#'
#' @returns A ggplot object
#' @family visualizing helpers
#' @family tf-idf helpers
#' @keywords internal
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
#'   plot_tf_idf()
#' }
plot_tf_idf <- function(
    data,
    rows = 1:10,
    by = doc_id,
    feature = word,
    label = FALSE,
    label_tweak = 2,
    label_inside = FALSE
){
  if (!"tf_idf" %in% colnames(data)) {
    data <- data |>
      summarize_tf_idf(by = {{ by }}, feature = {{ feature }})
  }

  data <- data |>
    dplyr::select({{ by }},
                  {{ feature }},
                  n, tf, idf, tf_idf) |>
    dplyr::distinct()

  feature_col <- deparse(substitute(feature))
  if (!feature_col %in% colnames(data)) {
    target <- colnames(data)[!colnames(data) %in% c("doc_id", "n", "tf", "idf", "tf_idf")][1]
    feature <- as.name(target)
  }

  data <- data |>
    dplyr::group_by({{ by }}) |>
    dplyr::arrange(dplyr::desc(tf_idf), .by_group = TRUE) |>
    dplyr::ungroup() |>
    dplyr::slice(
      rows,
      .by = {{ by }}) |>
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

  x_lab <- paste(prefix, "frequency\u2013inverse document frequency")

  the_plot <- data |>
    internal_plot_word_bars(
    tf_idf, rlang::enquo(by), FALSE, rlang::enquo(feature), FALSE, label, label_tweak, label_inside) +
    tidytext::scale_y_reordered() +
    ggplot2::labs(x = x_lab)

  if (length(unique(data[[deparse(substitute(by))]])) > 1) {
    the_plot <- the_plot +
      ggplot2::facet_wrap(ggplot2::vars({{ by }}),
                          scales = "free",
                          labeller = ggplot2::labeller({{ by }} := ggplot2::label_wrap_gen(18)))
  }

  the_plot
}
