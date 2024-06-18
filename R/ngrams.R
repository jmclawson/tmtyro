#' Add ngram columns
#'
#' @param df A tidy data frame, potentially containing a column called "word"
#' @param n A range defining the extent of an ngram---for instance, from word 1 to word 3. Alternatively, a single number will signal the number of words to include in each ngram. Default value of `1:2` will produce bigrams.
#' @param feature The feature to use when constructing ngrams
#' @param keep Whether to keep the original feature column
#' @param collapse Whether to join the ngram parts into a single column called "ngram"
#' @param by A grouping column identifying a document, such as `doc_id`.
#'
#' @returns The original data frame with columns added for subsequent parts of ngrams
#' @family n-gram helpers
#' @export
#'
#' @examples
#' \dontrun{
#'   my_corpus <- load_texts()
#'
#'   my_bigrams <- my_corpus |>
#'     add_ngrams(3)
#' }
#'
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' dubliners |>
#'   add_ngrams(2) |>
#'   head()

add_ngrams <- function(df, n = 1:2, feature = word, keep = FALSE, collapse = FALSE, by = doc_id){

  make_range <- function(n = 6) {
    if(length(n)==1) {n <- 1:n}
    n
  }

  if (!0 %in% n) {
    num <- make_range(n) - 1
  } else {
    num <- make_range(n)
  }

  int_shift <- function (x, n = 1L, default = NULL, order_by = NULL, ...){
    if (n > 0L) {
      dplyr::lead(x, n = n, default = default, order_by = order_by)
    } else {
      dplyr::lag(x, n = -n, default = default, order_by = order_by)
    }
  }

  map_shift <- num |>
    purrr::map(~purrr::partial(int_shift, n = .x))

  by_col <- deparse(substitute(by))

  if (by_col %in% colnames(df)) {
    the_df <- df |>
      dplyr::ungroup() |>
      dplyr::group_by({{ by }})
  } else {
    the_df <- df |>
      dplyr::ungroup()
  }

  the_df <- df |>
    dplyr::mutate(dplyr::across(
      .cols = {{ feature }},
      .fns = map_shift,
      .names = '{.col}_{num}')) |>
    dplyr::ungroup()

  if (!keep) {
    the_df <- the_df |>
      dplyr::select(-{{ feature }})
  }

  if (!is.character(feature)) {
    col_string <- deparse(substitute(feature))
  } else {
    col_string <- feature
  }

  if (collapse) {
    the_df <- the_df |>
      tidyr::unite(
        "ngram",
        stringr::str_glue("{col_string}_{min(num)}"):stringr::str_glue("{col_string}_{max(num)}"),
        sep = " ")
  }

  if (mean(n > 0) == 1) {
    colnames(the_df)[colnames(the_df) %in% stringr::str_glue("{col_string}_{num}")] <- stringr::str_glue("{col_string}_{make_range(n)}")
  }

  the_df |>
    add_class("ngrams")
}

#' Combine ngram columns
#'
#' @param df A tidy data frame, potentially containing columns called "word_1", "word_2", etc.
#' @param feature The column name prefix for the numbered columns of ngrams
#' @param keep Whether to keep the original columns called "word_1", "word_2", etc., alongside the new "ngram" column.
#'
#' @returns A data frame with a column called ngram
#' @family n-gram helpers
#' @export
#'
#' @examples
#' \dontrun{
#'   my_corpus <- load_texts(n = 2)
#'
#'   my_bigrams <- my_corpus |>
#'     add_ngrams(collapse = FALSE) |>
#'     combine_ngrams()
#' }
#'
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' dubliners |>
#'   add_ngrams(2) |>
#'   combine_ngrams() |>
#'   head()
combine_ngrams <- function(df, feature = word, keep = FALSE){
  # browser()
  col_string <- deparse(substitute(feature))
  if (!paste0(col_string,"_1") %in% colnames(df)) {
    col_string <- colnames(df)[grepl("_[01]",colnames(df))][1] |>
      stringr::str_remove_all("_[01]")
    feature <- as.name(col_string)
  }

  col_names <- df |>
    dplyr::select(tidyr::starts_with(paste0(col_string,"_"))) |>
    dplyr::select(tidyr::matches("[_][-]?\\d$")) |>
    colnames()

  if (length(col_names) < 2) {
    df <- df |>
      add_ngrams(2, feature)

    col_names <- df |>
      dplyr::select(tidyr::starts_with(paste0(col_string,"_"))) |>
      dplyr::select(tidyr::matches("[_]\\d$")) |>
      colnames()
  }

  feature_range <- col_names |>
    stringr::str_remove_all(col_string) |>
    stringr::str_remove_all("_") |>
    as.numeric() |>
    range()

  the_df <- df |>
    tidyr::unite(
      "ngram",
      stringr::str_glue("{col_string}_{feature_range[1]}"):stringr::str_glue("{col_string}_{feature_range[2]}"),
      sep = " ",
      remove = !keep)

  the_df |>
    add_class("combined_ngrams", remove = "ngrams")
}

#' Separate one word per column
#'
#' @param df A tidy data frame containing a column called "ngram"
#' @param names_prefix The prefixed name of the new columns, as in "word_1", "word_2", etc.
#' @param ... Additional options passed to [tidyr::separate_wider_delim()]
#'
#' @returns A data frame with one column separated into many
#' @family n-gram helpers
#' @export
#'
#' @examples
#' \dontrun{
#'   my_corpus <- load_texts(n = 2)
#'
#'   my_bigrams <- my_corpus |>
#'     separate_ngrams()
#' }
#'
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' dubliners |>
#'   add_ngrams() |>
#'   combine_ngrams() |>
#'   separate_ngrams() |>
#'   head()
separate_ngrams <- function(df, names_prefix = "word", ...) {
  if(!"ngram" %in% colnames(df)){
    stop("`separate_ngrams()` only works on tables with an `ngram` column.")
  }

  df |>
    # dplyr::rename({{ names_prefix }} := ngram) |>
    tidyr::separate_wider_delim(
      ngram,
      delim = " ",
      names_sep = "_",
      ...) |>
    {\(x) setNames(x,
                   colnames(x) |>
                     stringr::str_replace_all(
                       pattern = "ngram_",
                       replacement = paste0(names_prefix, "_")))}() |>
    add_class("ngrams", remove = "combined_ngrams")
}


#' Visualize bigram chains
#'
#' @param df A tidy data frame potentially containing a column called "word" or columns called "word_1" and "word_2".
#' @param feature The feature to use when constructing ngrams
#' @param random_seed Whether to randomize the creation of the network chart.
#' @param set_seed A specific seed to use if not random
#' @param legend Whether to show a legend for the edge color
#' @param top_n The number of pairs to visualize
#'
#' @returns A ggplot2 object
#' @family visualizing helpers
#' @family n-gram helpers
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' # It isn't necessary to use add_ngrams()
#' df |>
#'   plot_bigrams()
#'
#' # Adding them first allows for filtering steps
#' df |>
#'   add_ngrams() |>
#'   drop_stopwords(word_1) |>
#'   drop_stopwords(word_2) |>
#'   plot_bigrams()
#'
#' # Only bigrams influence the visualization These show the same networks:
#' df |>
#'   add_ngrams() |>
#'   plot_bigrams()
#'
#' df |>
#'   add_ngrams(4) |>
#'   plot_bigrams()
#'
#' }
#'
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' dubliners |>
#'   plot_bigrams()
#'
#' # Loading `ggraph` enables edge to show connection strengths
#' library(ggraph)
#'
#' dubliners |>
#'   plot_bigrams()
#'
#' dubliners |>
#'   add_ngrams(2) |>
#'   drop_stopwords(feature = word_1) |>
#'   drop_stopwords(feature = word_2) |>
#'   plot_bigrams()
#'
#' dubliners |>
#'   dplyr::filter(doc_id == "The Dead") |>
#'   plot_bigrams(top_n = 70) |>
#'   change_colors(c("black", "orange"))
plot_bigrams <- function(
    df,
    feature = word,
    random_seed = TRUE,
    set_seed = NULL,
    legend = FALSE,
    top_n = 35) {

  if ("ggraph" %in% (.packages())) {
    dynamic <- TRUE
  } else {
    dynamic <- FALSE
  }

  if (!random_seed & is.null(set_seed)) {
    set.seed(2016)
  } else if (!random_seed) {
    set.seed(set_seed)
  } else {
    set.seed(NULL)
  }

  col_string <- deparse(substitute(feature))

  if (!stringr::str_glue("{col_string}_2") %in% colnames(df)) {
    df <- df |>
      add_ngrams()
  }

  df_export <- df |>
    dplyr::count(dplyr::across(
      stringr::str_glue("{col_string}_1"):stringr::str_glue("{col_string}_2")),
      sort = TRUE) |>
    dplyr::slice_head(n = top_n)

  the_plot <- df_export |>
    igraph::graph_from_data_frame() |>
    ggraph::ggraph(layout = "fr")

  if (dynamic) {
    the_plot <- the_plot +
      ggraph::geom_edge_link(
        ggplot2::aes(
          start_cap = ggraph::label_rect(node1.name),
          end_cap = ggraph::label_rect(node2.name),
          color = n),
        alpha = 0.8,
        show.legend = legend,
        lineend = "round",
        linejoin = "mitre",
        arrow = ggplot2::arrow(
          length = ggplot2::unit(1.5, 'mm'),
          type = "closed")) +
      ggraph::scale_edge_color_continuous(
        na.value = "white",
        trans = "log",
        labels = scales::label_comma(),
        guide = ggraph::guide_edge_colorbar())
  } else {
    the_plot <- the_plot +
      ggraph::geom_edge_link(
        ggplot2::aes(
          start_cap = ggraph::label_rect(node1.name),
          end_cap = ggraph::label_rect(node2.name)),
        color = "#444444",
        alpha = 0.6,
        show.legend = legend,
        lineend = "round",
        linejoin = "mitre",
        arrow = ggplot2::arrow(
          length = ggplot2::unit(1.5, 'mm'),
          type = "closed"))
  }
  the_plot +
    ggraph::geom_node_text(
      ggplot2::aes(label = name),
      vjust = 0.5, hjust = 0.5) +
    ggplot2::theme_void() +
    ggplot2::labs(edge_color = "connections")
}
