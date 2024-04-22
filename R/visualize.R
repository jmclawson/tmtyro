#' Visualize output
#'
#' `visualize()` is context sensitive and will choose an appropriate function for visualization, based on the previous functions used. This function will automatically resolve to one of the following: [plot_doc_word_bars()], [plot_doc_word_heatmap()], [plot_bigrams()], [plot_vocabulary()], [plot_ttr()], [plot_htr()], [plot_tf_idf()], [plot_topic_distributions()], [plot_topic_bars()], or [plot_topic_wordcloud()].
#'
#' @param .data data processed with one or more functions from `tmtyro`
#' @param ... optional parameters
#' @param type visualization type preferred, useful after [add_lexical_variety()] or [make_topic_model()] or to specify "heatmap" for [plot_doc_word_heatmap()]
#' @inheritDotParams plot_doc_word_bars num:label_inside na_rm
#' @inheritDotParams plot_bigrams feature:top_n
#' @inheritDotParams plot_vocabulary identity:labeling
#' @inheritDotParams plot_ttr log_y
#' @inheritDotParams plot_htr log_y
#' @inheritDotParams plot_topic_distributions top_n
#' @inheritDotParams plot_topic_bars topics
#' @inheritDotParams plot_topic_wordcloud topics
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

  if (is.null(last) && "LDA_Gibbs" %in% class(.data)) {
    last <- "make_topic_model"
  } else if (is.null(last)) {
    last <- "nothing"
  }

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
    the_plot
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
    the_plot
  } else if (!is.null(type) && type == "heatmap") {
    .data |>
      plot_doc_word_heatmap(...)
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
    the_plot
  } else {
    message("Please use a recognized tmtyro function before visualizing.")
  }
}

log_function <- function(x) {
  extant <- attr(x, "last_fn")
  f <- deparse(sys.calls()[[sys.nframe() - 1]]) |>
    stringr::str_extract_all("^[a-z _]+") |>
    unlist()
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

#' Choose other colors
#'
#' `change_colors()` standardizes three methods for choosing color palettes for color or fill mapping, providing access to Brewer and Viridis palettes alongside custom choices.
#'
#' @param x A visualization made with [visualize()]
#' @param colorset Either "brewer", "viridis", or a vector of colors.
#' @param palette The number or name of palette (dependent on setting `colorset` to either "brewer" or "viridis")
#' @param kind Used only for Brewer palettes to match numbered palette with the specific subset
#' @param direction The direction colors should be applied to the data. Setting to anything other than 1 will reverse the order.
#'
#' @returns A ggplot2 object
#' @export
#'
#' @examples
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' # By default, Brewer's "Dark2" palette is applied to categorical data
#' austen |>
#'   visualize() |>
#'   change_colors()
#'
#' # Brewer's "BuGn" palette is applied to sequential data
#' austen |>
#'   visualize(type = "heatmap") |>
#'   change_colors()
#'
#' austen |>
#'   visualize() |>
#'   change_colors("viridis")
#'
#' # Palettes can be numbered or named
#' austen |>
#'   visualize(type = "heatmap") |>
#'   change_colors("viridis", palette = 6)
#'
#' austen |>
#'   visualize(type = "heatmap") |>
#'   change_colors("viridis", palette = "mako")
#'
#' austen |>
#'   visualize() |>
#'   change_colors(c("brown", "red", "orange", "yellow", "#aaff00"))
change_colors <- function(x, colorset = "brewer", palette = 2, kind = "qualitative", direction = 1) {

  mapped <- names(x$mapping)[names(x$mapping) %in% c("color", "colour", "fill")]

  secondary <- if(!is.null(x$plot_env$sec_y)) {x$plot_env$sec_y} else {NULL}
  color_map <-
    c(deparse(x$mapping$colour),
      deparse(x$mapping$color),
      deparse(x$mapping$fill)) |>
    stringr::str_remove_all("~") |>
    unique() |>
    {\(x) x[x!="NULL"]}() |>
    {\(x) ifelse(length(x) > 0, x, NA)}()
  if (length(mapped) == 0) {
    mapped <- names(x$layers[[1]]$mapping)[names(x$layers[[1]]$mapping) %in% c("color", "colour", "fill")]

    color_map <-
      c(deparse(x$layers[[1]]$mapping$colour),
        deparse(x$layers[[1]]$mapping$color),
        deparse(x$layers[[1]]$mapping$fill)) |>
      stringr::str_remove_all("^.*~") |>
      stringr::str_remove_all(",.*$") |>
      stringr::str_remove_all("[a-z .]+\\(") |>
      stringr::str_remove_all("[)]+") |>
      unique() |>
      {\(x) x[x!="NULL"]}() |>
      {\(x) ifelse(length(x) > 0, x, NA)}()

  }
  color_map_length <- x$data[[color_map]] |>
    unique() |>
    length()
  is_sequential <-
    tryCatch(is.numeric(as.numeric(as.character(x$data[[color_map]]))), warning = function(e) return(FALSE))

  if (is_sequential) {
    kind <- "seq"
  }

  if (length(colorset) == 1 && tolower(colorset) == "viridis") {
    if (is.numeric(palette)) {
      palette <- LETTERS[palette]
    }
    the_colors <- color_map_length |>
      {\(x) viridis::viridis_pal(option = palette)(x)}()
  } else if (length(colorset) == 1 && tolower(colorset) == "brewer") {
     if (kind == "sequential") {
      kind <- "seq"
    } else if (kind == "diverging") {
      kind <- "div"
    } else {
      kind <- "qual"
    }
    if (is_sequential) {
      kind <- "seq"
    }
    if (is.numeric(palette)){
      palette <- RColorBrewer::brewer.pal.info |>
        dplyr::filter(category==kind) |>
        rownames() |>
        {\(x) x[palette]}()
    }
    if (!is_sequential) {
      the_colors <- color_map_length |>
        {\(x) RColorBrewer::brewer.pal(x, palette)}()
    }
  } else if (length(colorset) != color_map_length) {
    the_colors <- colorRampPalette(colorset)(color_map_length)
  } else {
    the_colors <- colorset
  }

  if (direction != 1) {
    if (length(colorset) == 1 && colorset == "brewer"){
      if (!is_sequential) {
        the_colors <- rev(the_colors)
      }
    } else if (length(colorset) == 1 && colorset != "brewer") {
      the_colors <- rev(the_colors)
    } else if (length(colorset) != 1) {
      the_colors <- rev(the_colors)
    }
  }

  if (!is.null(secondary)) {
    x +
      ggplot2::scale_fill_manual(aesthetics = mapped, values = the_colors) +
      ggplot2::scale_y_continuous(
        labels = scales::label_comma(),
        sec.axis = ggplot2::dup_axis(
          breaks = secondary$breaks,
          labels = secondary$labels,
          guide = ggh4x::guide_axis_color(
            color = rev(the_colors))))
  } else if (kind != "seq"){
    x +
      ggplot2::scale_fill_manual(aesthetics = mapped, values = the_colors)
  } else if (length(colorset)==1 && colorset == "brewer") {
    x +
      ggplot2::scale_fill_distiller(palette = palette,
                                    direction = ifelse(direction == 1, -1, 1),
                                    aesthetics = mapped,
                                    na.value = "white")
  } else if (length(colorset)==1 && colorset == "viridis") {
    x +
      ggplot2::scale_fill_viridis_c(option = palette,
                                    direction = ifelse(direction == 1, 1, -1),
                                    aesthetics = mapped,
                                    na.value = "white")
  } else if (length(colorset) > 1) {
    x +
      ggplot2::scale_fill_gradientn(colours = the_colors,
                                    aesthetics = mapped,
                                    na.value = "white")
  } else {
    message("Something went wrong.")
  }
}

