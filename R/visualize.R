#' Visualize output
#'
#' @param .data data processed with one or more functions from `tmtyro`
#' @param ... optional parameters
#' @param type visualization type preferred, used after [add_lexical_variety()] and (eventually) [make_topic_model()]
#'
#' @returns a ggplot2 object
#' @export
#'
#' @examples
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' # A data frame with `doc_id` and `word` columns will visualize frequency by default
#' austen |>
#'    visualize()
#'
#' # Applying `tmtyro` functions will choose an appropriate visualization
#' austen |>
#'    add_sentiment() |>
#'    visualize()
#'
#' austen |>
#'    add_ngrams() |>
#'    visualize()
#'
#' austen |>
#'    add_ngrams() |>
#'    combine_ngrams() |>
#'    visualize()
#'
#' austen |>
#'    summarize_tf_idf() |>
#'    visualize()
#'
#' austen |>
#'    add_lexical_variety() |>
#'    visualize()
#'
#' # Some visualizations are specified with the `type` argument
#' austen |>
#'    add_lexical_variety() |>
#'    visualize(type = "ttr")
#'
#' austen |>
#'    add_lexical_variety() |>
#'    visualize(type = "hapax")
#'
#' # Other arguments get passed along
#' austen |>
#'    add_ngrams() |>
#'    visualize(node_color = "yellow", top_n = 50)
#'
#' austen |>
#'    add_lexical_variety() |>
#'    visualize()
visualize <- function(.data, ..., type = NULL) {
  last <- last_function(.data)
  if (is.null(last)) last <- "nothing"
  dots <- list(...)

  if(last == "add_sentiment") {
    .data |>
      plot_doc_word_bars(feature = sentiment, ...)
  } else if (last == "add_ngrams") {
    .data |>
      plot_bigrams(...)
  } else if (last == "combine_ngrams") {
    .data |>
      plot_doc_word_bars(feature = ngram, ...)
  } else if (last == "add_lexical_variety") {
    if (!is.null(type)) {
      if (type == "ttr") {
        the_plot <-
          .data |>
          plot_ttr(...)
      } else if (type == "htr") {
        the_plot <-
          .data |>
          plot_htr(...)
      } else if (type == "hapax") {
        the_plot <-
          .data |>
          plot_hapax(...)
      } else {
        the_plot <-
          .data |>
          plot_vocabulary(...)
      }
    } else {
      the_plot <-
        .data |>
        plot_vocabulary(...)
    }
    print(the_plot)
  } else if (last == "summarize_tf_idf") {
    .data |>
      plot_tf_idf(...)
  } else if (last == "make_topic_model") {
    .data |>
      plot_topic_distributions()
  } else if ("doc_id" %in% colnames(.data) &&
             "word" %in% colnames(.data)) {
    .data |>
      plot_doc_word_bars(...)
  } else if ("doc_id" %in% colnames(.data)) {
    the_plot <-
      .data |>
      count(doc_id) |>
      ggplot2::ggplot(ggplot2::aes(x = n, y = doc_id)) +
      ggplot2::geom_col() +
      ggplot2::labs(x = "words",
                    y = NULL)
    if ("reorder" %in% names(dots)){
      if (dots$reorder) {
        the_plot <-
          .data |>
          count(doc_id) |>
          ggplot2::ggplot(ggplot2::aes(x = n, y = reorder(doc_id, n))) +
          ggplot2::geom_col() +
          ggplot2::labs(x = "words",
                        y = NULL)
      }
    }
    print(the_plot)
  } else {
    message("Please use a recognized function on the data before visualizing.")
  }
}

log_function <- function(x, f) {
  extant <- attr(x, "last_fn")
  if (is.null(extant)) {
    attr(x, "last_fn") <- f
  } else {
    attr(x, "last_fn") <- c(extant, f)
  }
  return(x)
}

last_function <- function(x) {
  extant <- attr(x, "last_fn")
  extant[length(extant)]
}
