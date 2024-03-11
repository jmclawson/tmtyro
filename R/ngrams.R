#' Add ngram columns
#'
#' @param df A tidy data frame, potentially containing a column called "word"
#' @param n A range defining the extent of an ngram---for instance, from word 1 to word 3. Alternatively, a single number will signal the number of words to include in each ngram. Default value of `1:2` will produce bigrams.
#' @param feature The feature to use when constructing ngrams
#' @param keep Whether to keep the original feature column
#' @param collapse Whether to join the ngram parts into a single column called "ngram"
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

add_ngrams <- function(df, n = 1:2, feature = word, keep = FALSE, collapse = FALSE){

  make_range <- function(n = 6) {
    if(length(n)==1) {n <- 1:n}
    n
  }

  n <- make_range(n) - 1

  map_lead <- n |>
    purrr::map(~purrr::partial(dplyr::lead, n = .x))

  the_df <- df |>
    dplyr::mutate(dplyr::across(.cols = {{ feature }},
                  .fns = map_lead,
                  .names = "{.col}_{n+1}"))

  if (!keep) {
    the_df <- the_df |>
      dplyr::select(-{{ feature }})
  }

  if (collapse) {
    col_string <- deparse(substitute(feature))
    the_df <- the_df |>
      tidyr::unite(
        "ngram",
        glue::glue("{col_string}_{min(n+1)}"):glue::glue("{col_string}_{max(n+1)}"),
        sep = " ")
  }

  the_df
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
  col_string <- deparse(substitute(feature))

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

  if (keep) {
    the_df <- df |>
      tidyr::unite(
        "ngram",
        glue::glue("{col_string}_{feature_range[1]}"):glue::glue("{col_string}_{feature_range[2]}"),
        sep = " ",
        remove = FALSE)
  } else {
    the_df <- df |>
      tidyr::unite(
        "ngram",
        glue::glue("{col_string}_{feature_range[1]}"):glue::glue("{col_string}_{feature_range[2]}"),
        sep = " ")
  }
  the_df
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
      names_sep = "_")
}


#' Visualize ngram chains
#'
#' Mildly adapted from [Text Mining with R](https://www.tidytextmining.com), by Julia Silge and David Robinson
#'
#' @param df A tidy data frame potentially containing a column called "word" or columns called "word_1" and "word_2".
#' @param feature The feature to use when constructing ngrams
#' @param random_seed Whether to randomize the creation of the network chart.
#' @param set_seed A specific seed to use if not random
#' @param node_color The color of the nodes
#' @param edge_color The color of the edges
#' @param top_n The number of pairs to visualize
#'
#' @returns A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' # It isn't necessary to use add_ngrams()
#' df |>
#'   plot_ngrams()
#'
#' # Adding them first allows for filtering steps
#' df |>
#'   add_ngrams() |>
#'   drop_stopwords(word_1) |>
#'   drop_stopwords(word_2) |>
#'   plot_ngrams()
#'
#' # Only bigrams influence the visualization These show the same networks:
#' df |>
#'   add_ngrams(2) |>
#'   plot_ngrams()
#'
#' df |>
#'   add_ngrams(4) |>
#'   plot_ngrams()
#'
#' }
#'
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen |>
#'   plot_ngrams()
#'
#' austen |>
#'   add_ngrams(2) |>
#'   drop_stopwords(feature = word_1) |>
#'   drop_stopwords(feature = word_2) |>
#'   plot_ngrams()
#'
#' austen |>
#'   dplyr::filter(doc_id == "northanger") |>
#'   plot_ngrams(top_n = 90, node_color = "lightgreen")
plot_ngrams <- function(
    df,
    feature = word,
    random_seed = TRUE,
    set_seed = NULL,
    node_color = "lightblue",
    edge_color = "gray",
    top_n = 25) {

  if (!random_seed & is.null(set_seed)) {
    set.seed(2016)
  } else if (!random_seed) {
    set.seed(set_seed)
  } else {
    set.seed(NULL)
  }

  col_string <- deparse(substitute(feature))

  if (!glue::glue("{col_string}_2") %in% colnames(df)) {
    df <- df |>
      add_ngrams()
  }

  df |>
    dplyr::count(dplyr::across(
      glue::glue("{col_string}_1"):glue::glue("{col_string}_2")),
      sort = TRUE) |>
    dplyr::slice_head(n = top_n) |>
    igraph::graph_from_data_frame() |>
    ggraph::ggraph(layout = "fr") +
    ggraph::geom_edge_link(
      ggplot2::aes(edge_alpha = n),
      color = edge_color,
      show.legend = FALSE,
      arrow = ggplot2::arrow(
        type = "closed",
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


