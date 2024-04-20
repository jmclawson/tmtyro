#' Visualize output
#'
#' `visualize()` is context sensitive and will choose an appropriate function for visualization, based on the previous functions used. This function will automatically resolve to one of the following: [plot_doc_word_bars()], [plot_bigrams()], [plot_vocabulary()], [plot_ttr()], [plot_htr()], [plot_tf_idf()], [plot_topic_distributions()], [plot_topic_bars()], or [plot_topic_wordcloud()].
#'
#' @param .data data processed with one or more functions from `tmtyro`
#' @param ... optional parameters
#' @param type visualization type preferred, useful after [add_lexical_variety()] or [make_topic_model()]
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
#' if (FALSE) { # sentiment requires interaction on first load
#'   austen |>
#'      add_sentiment() |>
#'      visualize()
#' }
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
#'    visualize(node_color = "yellow", top_n = 25)
#'
#' austen |>
#'    add_lexical_variety() |>
#'    visualize(labeling = "inline")
visualize <- function(.data, ..., type = NULL) {
  last <- last_function(.data)
  if (is.null(last)) last <- "nothing"
  dots <- list(...)
  attr(.data, "visualize") <- TRUE

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
    suppressMessages(suppressWarnings(print(the_plot)))
  } else if (last == "summarize_tf_idf") {
    .data |>
      plot_tf_idf(...)
  } else if (last == "make_topic_model") {
    if ("topics" %in% names(dots)) {
      the_plot <-
        .data |>
        substitute() |>
        deparse() |>
        plot_topic_bars(...)
      if (!is.null(type)) {
        if (type %in% c("cloud", "wordcloud")) {
          the_plot <-
            .data |>
            substitute() |>
            deparse() |>
            plot_topic_wordcloud(...)
        }
      }
    } else {
      the_plot <-
        .data |>
        substitute() |>
        deparse() |>
        plot_topic_distributions()
    }
    print(the_plot)
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

# unname <- function(x) {
#   x |>
#     substitute() |>
#     deparse() |>
#     as.name() |>
#     unname2()
# }
#
# unname2 <- function(x) {
#   if (is.str)
#
#   x |>
#     substitute() |>
#     deparse()
# }
