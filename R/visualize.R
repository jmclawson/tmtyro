#' Visualize output
#'
#' `visualize()` provides a simple method for displaying results. Based on previous functions used, `visualize()` will choose a method, resolving to one of the other visualizing helpers.
#'
#' @param data data processed with one or more functions from `tmtyro`
#' @param ... optional parameters
#' @inheritDotParams plot_doc_word_bars rows:label_inside na_rm
#' @inheritDotParams plot_bigrams feature:top_n
#' @inheritDotParams plot_vocabulary identity:labeling
#' @inheritDotParams plot_ttr log_y
#' @inheritDotParams plot_hir log_y
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
#' \dontrun{
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' # A data frame with `doc_id` and `word` columns will visualize frequency by default
#' dubliners |>
#'    visualize()
#'
#' # Applying `tmtyro` functions will choose an appropriate visualization
#'
#' dubliners |>
#'    add_ngrams() |>
#'    visualize()
#'
#' dubliners |>
#'    add_ngrams() |>
#'    combine_ngrams() |>
#'    visualize()
#'
#' dubliners |>
#'    summarize_tf_idf() |>
#'    visualize()
#'
#' dubliners |>
#'    add_vocabulary() |>
#'    visualize()
#'
#' if (FALSE) { # sentiment requires interaction on first load
#'   dubliners |>
#'      add_sentiment() |>
#'      visualize()
#' }
#'
#' # Some visualizations are specified with the `type` argument
#' dubliners |>
#'    add_vocabulary() |>
#'    visualize(type = "ttr")
#'
#' if (FALSE) { # puzzlingly broken for Dubliners, but usually works
#' dubliners |>
#'    add_vocabulary() |>
#'    visualize(type = "hapax")
#' }
#'
#' # Other arguments get passed along
#' dubliners |>
#'    add_ngrams() |>
#'    visualize(top_n = 25)
#'
#' dubliners |>
#'    add_vocabulary() |>
#'    visualize(x = progress_percent)
#' }
visualize <- function(data,...){
  UseMethod("visualize")
}

#' @export
visualize.default <- function(data, inorder = TRUE, count = NULL, rows = NULL, type = NULL, ...){
  if ("doc_id" %in% colnames(data) &&
      "word" %in% colnames(data)) {
    if (!is.null(type) && type == "heatmap") {
      data |>
        plot_doc_word_heatmap(...)
    } else {
      if ("doc_id" %in% colnames(data) && inorder) {
        data <- data |>
          dplyr::mutate(doc_id = forcats::fct_inorder(doc_id))
      }
      if (!is.null(count) && count) {
        data |>
          plot_doc_word_bars(...)
      } else {
        if ("doc_id" %in% colnames(data) && !inorder) {
          data <- data |>
            dplyr::mutate(doc_id = forcats::fct_infreq(doc_id) |> forcats::fct_rev())
        } else if ("doc_id" %in% colnames(data)) {
          data <- data |>
            dplyr::mutate(doc_id = forcats::fct_rev(doc_id))
        }
        # fill_colors <- rep("#595959", length(unique(data$doc_id)))
        data |>
          ggplot2::ggplot(ggplot2::aes(y = doc_id, fill = doc_id)) +
          ggplot2::geom_bar(show.legend = FALSE) +
          ggplot2::scale_x_continuous(labels = scales::label_comma(),
                                      expand = c(0,0)) +
          ggplot2::labs(y = NULL, x = "length (words)") +
          theme_tmtyro()
          # ggplot2::theme_minimal() +
          # ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
          #                panel.grid.major.y = ggplot2::element_blank(),
          #                panel.grid.minor.y = ggplot2::element_blank()) #+
          # ggplot2::scale_fill_manual(values = fill_colors)
      }
    }
  } else if ("doc_id" %in% colnames(data)) {
    the_plot <-
      data |>
      dplyr::count(doc_id) |>
      ggplot2::ggplot(ggplot2::aes(x = n, y = doc_id)) +
      ggplot2::geom_col() +
      ggplot2::labs(x = "words",
                    y = NULL)
  } else if (count && "n" %in% colnames(data)) {

  }
}

#' @export
visualize.frequency <- function(data, rows = NULL, ...) {
  # browser()
  if (is.null(rows)) rows <- 1:10
  if ("doc_id" %in% colnames(data) &&
      length(colnames(data)) > 2) {
    if ("feature" %in% names(attributes(data))) {
      # try to glean the feature name from attributes
      if ("name" %in% class(attr(data, "feature"))) {
        feature <- attr(data, "feature") |>
          as.name()
        the_feature <- rlang::enquo(feature)
      } else {
        feature <- attr(data, "feature") |>
          as.name()
        the_feature <- rlang::enquo(feature)
      }

      data |>
        plot_doc_word_bars(feature = !!the_feature, rows = rows, ...)
    } else {
      data |>
        plot_doc_word_bars(rows = rows, ...)
    }
  } else {
    other_col <- colnames(data)[colnames(data) != "n"][1]
    data |>
      dplyr::arrange(dplyr::desc(n)) |>
      dplyr::slice(rows) |>
      ggplot2::ggplot(ggplot2::aes(
        x = n,
        y = reorder(.data[[other_col]], n))) +
      ggplot2::geom_col() +
      ggplot2::scale_x_continuous(
        labels = scales::label_comma(),
        expand = c(0,0)) +
      theme_tmtyro()
      # ggplot2::theme_minimal() +
      # ggplot2::theme(
      #   panel.grid.major.y = ggplot2::element_blank(),
      #   panel.grid.minor.x = ggplot2::element_blank()) +
      # ggplot2::labs(x = NULL,
      #               y = NULL)
  }
}

#' @export
visualize.expanded <- function(data, columns = 1:6, digits = 2, ...) {
  if ("doc_id" %in% colnames(data)) {
    columns <- c(0, columns) + 1
    data <- data |>
      dplyr::select(tidyr::all_of(columns)) |>
      tidyr::pivot_longer(
        cols = -doc_id,
        names_to = "feature") |>
      dplyr::mutate(
        feature = forcats::fct_inorder(feature))

    midpoint <- data |>
      dplyr::pull(value) |>
      mean(na.rm = TRUE)

    data |>
      dplyr::mutate(
        label_color = ifelse(value > midpoint, "white", "black")
      ) |>
      ggplot2::ggplot(ggplot2::aes(
        y = forcats::fct_rev(doc_id),
        x = feature,
        fill = -value,
        label = value |>
          scales::label_percent(
            accuracy = 1 / (10 ^ digits)#,
            # suffix = ""
            )())) +
      ggplot2::geom_tile(
        color = "gray",
        show.legend = FALSE
      ) +
      ggplot2::geom_text(
        ggplot2::aes(color = label_color),
        show.legend = FALSE
      ) +
      ggplot2::scale_x_discrete(
        expand = c(0,0),
        position = "top") +
      ggplot2::scale_y_discrete(
        expand = c(0,0)) +
      ggplot2::labs(
        y = NULL,
        x = NULL) +
      ggplot2::scale_color_identity() +
      theme_tmtyro(grid_x = FALSE, grid_y = FALSE)
      # ggplot2::theme_minimal() +
      # ggplot2::theme(
      #   panel.grid.major = ggplot2::element_blank(),
      #   panel.grid.minor = ggplot2::element_blank(),
      # )
  }

}

#' @export
visualize.vocabulary <- function(data, type = NULL, ...) {
  if (!is.null(type)) {
    if (type == "ttr") {
      data |>
        plot_ttr(...)
    } else if (type == "hir") {
      data |>
        plot_hir(...)
    } else if (type == "hapax") {
      data |>
        plot_hapax(...)
    } else {
      data |>
        plot_vocabulary(...)
    }
  } else {
    data |>
      plot_vocabulary(...)
  }
}

#' @export
visualize.ngrams <- function(data, ...) {
  data |>
    plot_bigrams(...)
}

#' @export
visualize.combined_ngrams <- function(data, inorder = TRUE, color_y = TRUE, ...) {
  if ("doc_id" %in% colnames(data) && inorder) {
    data <- data |>
      dplyr::mutate(doc_id = forcats::fct_inorder(doc_id))
  }
  if (color_y & "ngram" %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(ngram = ngram |>
                      forcats::fct_infreq() |>
                      forcats::fct_rev())
  }
  data |>
    plot_doc_word_bars(feature = ngram, color_y = color_y, ...)
}

#' @export
visualize.sentiment <- function(data, inorder = TRUE, ignore = NULL, ...) {
  if ("doc_id" %in% colnames(data) && inorder) {
    data <- data |>
      dplyr::mutate(doc_id = forcats::fct_inorder(doc_id))
  }

  if (!is.null(ignore)) {
    data <- data |>
      dplyr::mutate(
        sentiment = dplyr::case_when(
          sentiment %in% ignore ~ NA_character_,
          TRUE ~ sentiment
        ))
  }

  data |>
    plot_doc_word_bars(feature = sentiment, reorder_y = FALSE, color_y = TRUE, ...)
}

#' @export
visualize.dictionary <- function(data, inorder = TRUE, ignore = NULL, reorder_y = TRUE, color_y = TRUE, ...) {
  feature <- attr(data, "feature") |>
    as.name()
  the_feature <- rlang::enquo(feature)
  if ("doc_id" %in% colnames(data) && inorder) {
    data <- data |>
      dplyr::mutate(doc_id = forcats::fct_inorder(doc_id))
  }

  if (!is.null(ignore)) {
    data <- data |>
      dplyr::mutate(
        {{ feature }} := dplyr::case_when(
          {{ feature }} %in% ignore ~ NA_character_,
          TRUE ~ {{ feature }}
        ))
  }

  cl <- match.call()

  data |>
    plot_doc_word_bars(feature = !!the_feature, reorder_y = reorder_y, color_y = color_y, ...)
}

#' @export
# Platypus TODO - check theme output
visualize.tf_idf <- function(data, simplify = TRUE, ...) {
  the_plot <- data |>
    plot_tf_idf(...)

  if ("doc_id" %in% colnames(data) &&
      length(unique(data$doc_id)) > 6 &&
      simplify) {
    the_plot <- the_plot +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank())
  }

  the_plot
}

#' @export
visualize.LDA_Gibbs <- function(data, topics = NULL, type = NULL, ...) {
  data_string <- deparse(substitute(data))
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


##### Modularize smart theming #####
# Things with S7 are kind of weird
conditional_grid <- function(theme = ggplot2::theme_gray()) {
  structure(
    list(theme = theme),
    class = "conditional_grid"
  )
}

if (requireNamespace("S7") &&
    utils::packageVersion("ggplot2") >= "4.0.1") {
  conditional_grid_class <- S7::new_S3_class("conditional_grid")

  S7::method(
    update_ggplot,
    list(conditional_grid_class, class_ggplot)
  ) <- function(object, plot, ...) {

    check_mapping <- function(plot, variable) {
      if (!is.null(plot$mapping[[variable]])) {
        internal_var <- rlang::quo_get_expr(plot$mapping[[variable]])
        if ("call" %in% class(internal_var)) {
          check_character_var <- TRUE
        } else {
          check_character_var <- any(class(plot$data[[rlang::as_string(internal_var)]]) %in% c("factor", "character"))
        }
      } else {
        if (is.null(plot$layers[[1]]$mapping[["y"]])) {
          return(FALSE)
        }
        check_character_var_vec <- c()
        for(i in seq_along(plot$layers)) {
          internal_var <- rlang::quo_get_expr(plot$layers[[i]]$mapping[[variable]])
          if ("call" %in% class(internal_var)) {
            check_character_var_vec <- c(check_character_var_vec, TRUE)
          } else {
            check_character_var_vec <- c(check_character_var_vec, class(plot$data[[rlang::as_string(internal_var)]]) %in% c("factor", "character"))
          }
        }
        check_character_var <- any(check_character_var_vec)
      }
      check_character_var
    }

    check_character_x <- check_mapping(plot, "x")
    check_character_y <- check_mapping(plot, "y")

    thm <- object$theme

    if (check_character_x) {
      thm <- thm + ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank())
    }

    if (check_character_y) {
      thm <- thm + ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank())
    }

    plot + thm
  }
}

#' Apply a smart tmtyro theme
#'
#' @param minor Whether to show minor grid lines
#' @param grid_x Whether to show grid lines on the X-axis. When `NULL`, this will be determined by the data being visualized.
#' @param grid_y Whether to show grid lines on the Y-axis. When `NULL`, this will be determined by the data being visualized.
#' @param base_theme The base ggplot2 theme used as a starting point
#' @param ... Additional arguments passed to base theme
#'
#' @returns a ggplot2 object
#' @family visualizing helpers
#' @export
#'
#' @examples
#' if (FALSE) {
#' library(ggplot2)
#'
#' mtcars |>
#'   ggplot(aes(disp, hp)) +
#'   geom_point() +
#'   theme_tmtyro()
#'
#' mtcars |>
#'   ggplot(aes(hp, factor(cyl))) +
#'   geom_point() +
#'   theme_tmtyro()
#'
#' mtcars |>
#'   ggplot(aes(factor(cyl), hp)) +
#'   geom_point() +
#'   theme_tmtyro()
#' }
#'
theme_tmtyro <- function(minor = FALSE, grid_x = NULL, grid_y = NULL, base_theme = ggplot2::theme_minimal, ...) {
  out <- base_theme(...)
  hide_x <- !is.null(grid_x) && !identical(grid_x, TRUE)
  hide_y <- !is.null(grid_y) && !identical(grid_y, TRUE)
  if (hide_x && hide_y) {
    out <- out +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank())
  } else if (hide_x) {
    out <- out +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank())
  } else if (hide_y) {
    out <- out +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank())
  }

  if (!minor) {
    out <- out +
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank()
      )
  }

  if (is.null(grid_x) && is.null(grid_y)) {
    conditional_grid(out)
  } else {
    out
  }
}

##### Du Bois theming options #####
#' Du Bois-style legends
#'
#' #export
# draw_key_dubois <- function(data, params, size) {
#   stroke_size <- data$stroke %||% 0.5
#   stroke_size[is.na(stroke_size)] <- 0
#   if(!is.null(data$size)){
#     data$size <- data$size * 6
#   }
#   grid::pointsGrob(
#     0.5, 0.5,
#     pch = 21,
#     gp = grid::gpar(
#       col = alpha(data$colour %||% "black", data$alpha),
#       fill = alpha(data$fill %||% "black", data$alpha),
#       fontsize = (data$size %||% 6) * .pt + stroke_size * .stroke / 2,
#       lwd = stroke_size * .stroke / 2))
# }

#' Apply a smart theme styled like Du Bois
#'
#' #export
# theme_dubois <- function(minor = FALSE, grid_x = NULL, grid_y = NULL, paper = "#faf0e6", ...) {
#   out <- ggplot2::theme_minimal(paper = paper, ...)
#   hide_x <- !is.null(grid_x) && !identical(grid_x, TRUE)
#   hide_y <- !is.null(grid_y) && !identical(grid_y, TRUE)
#   if (hide_x && hide_y) {
#     out <- out +
#       ggplot2::theme(
#         panel.grid.major = ggplot2::element_blank(),
#         panel.grid.minor = ggplot2::element_blank())
#   } else if (hide_x) {
#     out <- out +
#       ggplot2::theme(
#         panel.grid.major.x = ggplot2::element_blank(),
#         panel.grid.minor.x = ggplot2::element_blank())
#   } else if (hide_y) {
#     out <- out +
#       ggplot2::theme(
#         panel.grid.major.y = ggplot2::element_blank(),
#         panel.grid.minor.y = ggplot2::element_blank())
#   }
#
#   if (!minor) {
#     out <- out +
#       ggplot2::theme(
#         panel.grid.minor.x = ggplot2::element_blank(),
#         panel.grid.minor.y = ggplot2::element_blank()
#       )
#   }
#
#   if (is.null(grid_x) && is.null(grid_y)) {
#     out <- conditional_grid(out)
#   } else {
#     out <- out
#   }
#
#   out <- out +
#     theme(
#       # geom = element_geom(key_glyph = "dubois"),
#       palette.colour.discrete = c(
#         "#dc143c", "#ffd700", "#654321",
#         "#4682b4", "#ffc0cb", "#00aa00",
#         "#d2b48c", "#7e6583", "#000000")
#     )
#
#   out
# }
