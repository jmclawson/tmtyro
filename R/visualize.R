#' Visualize output
#'
#' `visualize()` provides a simple method for displaying results. Based on previous functions used, `visualize()` will choose a method, resolving to one of the other visualizing helpers.
#'
#' @param .data data processed with one or more functions from `tmtyro`
#' @param ... optional parameters
#' @inheritDotParams plot_doc_word_bars rows:label_inside na_rm
#' @inheritDotParams plot_bigrams feature:top_n
#' @inheritDotParams plot_vocabulary identity:labeling
#' @inheritDotParams plot_ttr log_y
#' @inheritDotParams plot_htr log_y
#' @inheritDotParams plot_topic_distributions top_n
#' @inheritDotParams plot_topic_bars topics
#' @inheritDotParams plot_topic_wordcloud topics
#'
#' @note
#' For some visualizations, an optional `type` parameter may be helpful to change the visualization. For example, setting `type = "htr"`, `type = "ttr"`, or `type = "hapax"` after [add_vocabulary()] will emphasize different columns added by that function. Similarly, `type = "cloud"` or `type = "wordcloud"` will show topic word clouds after [make_topic_model()], and `type = "heatmap"` will show an alternative visualization for word frequencies.
#'
#' @returns a ggplot2 object
#' @family visualizing helpers
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
#'    add_vocabulary() |>
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
#'    add_vocabulary() |>
#'    visualize(type = "ttr")
#'
#' austen |>
#'    add_vocabulary() |>
#'    visualize(type = "hapax")
#'
#' # Other arguments get passed along
#' austen |>
#'    add_ngrams() |>
#'    visualize(node_color = "yellow", top_n = 25)
#'
#' austen |>
#'    add_vocabulary() |>
#'    visualize(labeling = "inline")
visualize <- function(.data,...){
  UseMethod("visualize")
}

#' @export
visualize.default <- function(.data, inorder = TRUE, type = NULL, ...){
  if ("doc_id" %in% colnames(.data) &&
      "word" %in% colnames(.data)) {
    if (!is.null(type) && type == "heatmap") {
      .data |>
        plot_doc_word_heatmap(...)
    } else {
      if ("doc_id" %in% colnames(.data) && inorder) {
        .data <- .data |>
          dplyr::mutate(doc_id = forcats::fct_inorder(doc_id))
      }
      .data |>
        plot_doc_word_bars(...)
    }
  } else if ("doc_id" %in% colnames(.data)) {
    the_plot <-
      .data |>
      count(doc_id) |>
      ggplot2::ggplot(ggplot2::aes(x = n, y = doc_id)) +
      ggplot2::geom_col() +
      ggplot2::labs(x = "words",
                    y = NULL)
  }
}

#' @export
visualize.vocabulary <- function(.data, type = NULL, ...) {
  if (!is.null(type)) {
    if (type == "ttr") {
      .data |>
        plot_ttr(...)
    } else if (type == "htr") {
      .data |>
        plot_htr(...)
    } else if (type == "hapax") {
      .data |>
        plot_hapax(...)
    } else {
      .data |>
        plot_vocabulary(...)
    }
  } else {
    .data |>
      plot_vocabulary(...)
  }
}

#' @export
visualize.ngrams <- function(.data, ...) {
  .data |>
    plot_bigrams(...)
}

#' @export
visualize.combined_ngrams <- function(.data, inorder = TRUE, color_y = TRUE, ...) {
  if ("doc_id" %in% colnames(.data) && inorder) {
    .data <- .data |>
      dplyr::mutate(doc_id = forcats::fct_inorder(doc_id))
  }
  .data |>
    plot_doc_word_bars(feature = ngram, color_y = color_y, ...)
}

#' @export
visualize.sentiment <- function(.data, inorder = TRUE, ignore = NULL, ...) {
  if ("doc_id" %in% colnames(.data) && inorder) {
    .data <- .data |>
      dplyr::mutate(doc_id = forcats::fct_inorder(doc_id))
  }

  if (!is.null(ignore)) {
    .data <- .data |>
      dplyr::mutate(
        sentiment = dplyr::case_when(
          sentiment %in% ignore ~ NA_character_,
          TRUE ~ sentiment
        ))
  }

  .data |>
    plot_doc_word_bars(feature = sentiment, reorder_y = FALSE, color_y = TRUE, ...)
}

#' @export
visualize.tf_idf <- function(.data, ...) {
  .data |>
    plot_tf_idf(...)
}

#' @export
visualize.LDA_Gibbs <- function(.data, topics = NULL, type = NULL, ...) {
  data_string <- deparse(substitute(.data))
  if (!is.null(topics)) {
    if (!is.null(type) && type %in% c("cloud", "wordcloud")) {
      the_plot <-
        data_string |>
        plot_topic_wordcloud(topics = topics, ...)
    } else {
      the_plot <-
        data_string |>
        plot_topic_bars(topics = topics, ...)
    }
  } else if (!is.null(type) && type == "interactive") {
    the_plot <-
      data_string |>
      interactive_topic_distributions(...)
  } else {
    the_plot <-
      data_string |>
      plot_topic_distributions(...)
  }
  the_plot
}

add_class <- function(x, class, remove = NULL) {
  class(x) <- append(class, class(x)[!class(x) %in% remove])
  x
}

#' Choose other colors
#'
#' `change_colors()` standardizes three methods for choosing color palettes for color or fill mapping, providing access to Brewer and Viridis palettes alongside custom choices.
#'
#' @param x A visualization made with [visualize()]
#' @param colorset Either "brewer", "viridis", "okabe-ito" or a vector of colors.
#' @param palette The number or name of palette (dependent on setting `colorset` to either "brewer" or "viridis")
#' @param kind Used only for Brewer palettes to match numbered palette with the specific subset
#' @param direction The direction colors should be applied to the data. Setting to anything other than 1 will reverse the order.
#' @param start Useful for predefined colorsets from Okabe-Ito and Brewer to start from a color other than 1
#'
#' @returns A ggplot2 object
#' @family visualizing helpers
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
#' austen |>
#'   add_vocabulary() |>
#'   visualize(labeling = "axis") |>
#'   change_colors()
#'
#' # Brewer's "BuGn" palette is applied to sequential data by default
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
#' # Manually listing colors will adopt them. When
#' # the number of colors differs from what's needed,
#' # other colors will be filled in between.
#' austen |>
#'   visualize() |>
#'   change_colors(c("brown", "red", "orange", "yellow", "#aaff00"))
change_colors <- function(
    x,
    colorset = "brewer",
    palette = 2,
    kind = "qualitative",
    direction = 1,
    start = 1) {

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
  } else if (length(colorset) == 1 && grepl("okabe", tolower(colorset))) {
    the_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000")[start:(color_map_length + start - 1)]
  } else if (length(colorset) == 1 && tolower(colorset) == "brewer") {
    rlang::check_installed("RColorBrewer")
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
      if (start != 1) {
        the_colors <-  RColorBrewer::brewer.pal(8, palette)[start:(color_map_length + start - 1)]
      } else {
        the_colors <- color_map_length |>
          {\(x) ifelse(x < 3, 3, x)}() |>
          {\(x) RColorBrewer::brewer.pal(x, palette)}() |>
          {\(x) x[1:color_map_length]}()
      }
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

