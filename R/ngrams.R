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
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen |>
#'   add_ngrams(2) |>
#'   head()

add_ngrams <- function(df, n = 1:2, feature = word, keep = FALSE, collapse = FALSE, by = doc_id){

  make_range <- function(n = 6) {
    if(length(n)==1) {n <- 1:n}
    n
  }

  n <- make_range(n) - 1

  map_lead <- n |>
    purrr::map(~purrr::partial(dplyr::lead, n = .x))

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
    dplyr::mutate(dplyr::across(.cols = {{ feature }},
                                .fns = map_lead,
                                .names = "{.col}_{n+1}")) |>
    dplyr::ungroup()

  if (!keep) {
    the_df <- the_df |>
      dplyr::select(-{{ feature }})
  }

  if (collapse) {
    col_string <- deparse(substitute(feature))
    the_df <- the_df |>
      tidyr::unite(
        "ngram",
        stringr::str_glue("{col_string}_{min(n+1)}"):stringr::str_glue("{col_string}_{max(n+1)}"),
        sep = " ")
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
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen |>
#'   add_ngrams(2) |>
#'   combine_ngrams() |>
#'   head()
combine_ngrams <- function(df, feature = word, keep = FALSE){
  # browser()
  col_string <- deparse(substitute(feature))
  if (!paste0(col_string,"_1") %in% colnames(df)) {
    col_string <- colnames(df)[grepl("_1",colnames(df))][1] |>
      stringr::str_remove_all("_1")
    feature <- as.name(col_string)
  }

  col_names <- df |>
    dplyr::select(tidyr::starts_with(paste0(col_string,"_"))) |>
    colnames()

  if (length(col_names) < 2) {
    df <- df |>
      add_ngrams(2, feature)

    col_names <- df |>
      dplyr::select(tidyr::starts_with(paste0(col_string,"_"))) |>
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
#'
#' @returns A data frame with one column separated into many
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
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen |>
#'   add_ngrams(2) |>
#'   combine_ngrams() |>
#'   separate_ngrams() |>
#'   head()
separate_ngrams <- function(df, names_prefix = "word") {
  if(!"ngram" %in% colnames(df)){
    cli::cli_abort("`separate_ngrams()` only works on tables with an `ngram` column.")
  }

  df |>
    dplyr::rename({{ names_prefix }} := ngram) |>
    tidyr::separate_wider_delim(
      {{ names_prefix }},
      delim = " ",
      names_sep = "_") |>
    add_class("ngrams", remove = "combined_ngrams")
}


#' Visualize bigram chains
#'
#' Adapted from `[visualize_bigrams()](https://www.tidytextmining.com/ngrams#visualizing-bigrams-in-other-texts)` in Julia Silge and David Robinson's *Text Mining with R.
#'
#' @param df A tidy data frame potentially containing a column called "word" or columns called "word_1" and "word_2".
#' @param feature The feature to use when constructing ngrams
#' @param random_seed Whether to randomize the creation of the network chart.
#' @param set_seed A specific seed to use if not random
#' @param node_color The color of the nodes
#' @param edge_color The color of the edges. When the `ggraph` package is not loaded, the default `edge_color=NULL` setting presents edges as gray. When that package is is loaded, a `NULL` edge color colors edges black. Whatever the color defined by this parameter, loading `ggraph` enables transparency to show strength of each bigram connection.
#' @param top_n The number of pairs to visualize
#'
#' @returns A ggplot2 object
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
#'   add_ngrams(2) |>
#'   plot_bigrams()
#'
#' df |>
#'   add_ngrams(4) |>
#'   plot_bigrams()
#'
#' }
#'
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen |>
#'   plot_bigrams()
#'
#' # Loading `ggraph` enables edge to show connection strengths
#' library(ggraph)
#'
#' austen |>
#'   plot_bigrams()
#'
#' austen |>
#'   add_ngrams(2) |>
#'   drop_stopwords(feature = word_1) |>
#'   drop_stopwords(feature = word_2) |>
#'   plot_bigrams()
#'
#' austen |>
#'   dplyr::filter(doc_id == "northanger") |>
#'   plot_bigrams(top_n = 80, node_color = "lightgreen")
plot_bigrams <- function(
    df,
    feature = word,
    random_seed = TRUE,
    set_seed = NULL,
    node_color = "lightblue",
    edge_color = NULL,
    top_n = 25) {

  if (is.null(edge_color)) {
    if ("ggraph" %in% (.packages())) {
      edge_color <- "black"
    } else {
      edge_color <- "gray"
    }
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

  df_export_2 <-
    df_export |>
    igraph::graph_from_data_frame()

  df_export_2 |>
    ggraph::ggraph(layout = "fr") +
    ggraph::geom_edge_link(
      ggplot2::aes(alpha = n),
      color = edge_color,
      show.legend = FALSE,
      lineend = "round",
      linejoin = "mitre",
      arrow = ggplot2::arrow(
        length = ggplot2::unit(.10, "inches")),
      end_cap = ggraph::circle(.07, "inches")) +
    ggraph::geom_node_point(
      color = node_color,
      size = 5) +
    ggraph::geom_node_text(
      ggplot2::aes(label = name),
      vjust = 0.5, hjust = 0.5) +
    ggplot2::theme_void()
}
