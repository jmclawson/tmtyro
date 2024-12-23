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
visualize <- function(.data,...){
  UseMethod("visualize")
}

#' @export
visualize.default <- function(.data, inorder = TRUE, count = NULL, rows = NULL, type = NULL, ...){
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
      if (!is.null(count) && count) {
        .data |>
          plot_doc_word_bars(...)
      } else {
        if ("doc_id" %in% colnames(.data) && !inorder) {
          .data <- .data |>
            dplyr::mutate(doc_id = forcats::fct_infreq(doc_id) |> forcats::fct_rev())
        } else if ("doc_id" %in% colnames(.data)) {
          .data <- .data |>
            dplyr::mutate(doc_id = forcats::fct_rev(doc_id))
        }
        # fill_colors <- rep("#595959", length(unique(.data$doc_id)))
        .data |>
          ggplot2::ggplot(ggplot2::aes(y = doc_id, fill = doc_id)) +
          ggplot2::geom_bar(show.legend = FALSE) +
          ggplot2::scale_x_continuous(labels = scales::label_comma(),
                                      expand = c(0,0)) +
          ggplot2::labs(y = NULL, x = "length (words)") +
          ggplot2::theme_minimal() +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                         panel.grid.major.y = ggplot2::element_blank(),
                         panel.grid.minor.y = ggplot2::element_blank()) #+
          # ggplot2::scale_fill_manual(values = fill_colors)
      }
    }
  } else if ("doc_id" %in% colnames(.data)) {
    the_plot <-
      .data |>
      count(doc_id) |>
      ggplot2::ggplot(ggplot2::aes(x = n, y = doc_id)) +
      ggplot2::geom_col() +
      ggplot2::labs(x = "words",
                    y = NULL)
  } else if (count && "n" %in% colnames(.data)) {

  }
}

#' @export
visualize.frequency <- function(.data, rows = NULL, ...) {
  # browser()
  if (is.null(rows)) rows <- 1:10
  if ("doc_id" %in% colnames(.data) &&
      length(colnames(.data)) > 2) {
    if ("feature" %in% names(attributes(.data))) {
      feature <- attr(.data, "feature") |>
        as.name()
      the_feature <- rlang::enquo(feature)
      .data |>
        plot_doc_word_bars(feature = !!the_feature, rows = rows, ...)
    } else {
      .data |>
        plot_doc_word_bars(rows = rows, ...)
    }
  } else {
    other_col <- colnames(.data)[colnames(.data) != "n"][1]
    .data |>
      dplyr::arrange(dplyr::desc(n)) |>
      dplyr::slice(rows) |>
      ggplot2::ggplot(ggplot2::aes(
        x = n,
        y = reorder(.data[[other_col]], n))) +
      ggplot2::geom_col() +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_continuous(
        labels = scales::label_comma(),
        expand = c(0,0)) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank()) +
      ggplot2::labs(x = NULL,
                    y = NULL)
  }
}

#' @export
visualize.expanded <- function(.data, columns = 1:6, digits = 2, ...) {
  if ("doc_id" %in% colnames(.data)) {
    columns <- c(0, columns) + 1
    .data <- .data |>
      dplyr::select(tidyr::all_of(columns)) |>
      tidyr::pivot_longer(
        cols = -doc_id,
        names_to = "feature") |>
      dplyr::mutate(
        feature = forcats::fct_inorder(feature))

    midpoint <- .data |>
      dplyr::pull(value) |>
      mean(na.rm = TRUE)

    .data |>
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
      ggplot2::theme_minimal() +
      ggplot2::labs(
        y = NULL,
        x = NULL) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
      ) +
      ggplot2::scale_color_identity()
  }

}

#' @export
visualize.vocabulary <- function(.data, type = NULL, ...) {
  if (!is.null(type)) {
    if (type == "ttr") {
      .data |>
        plot_ttr(...)
    } else if (type == "hir") {
      .data |>
        plot_hir(...)
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
  if (color_y & "ngram" %in% colnames(.data)) {
    .data <- .data |>
      dplyr::mutate(ngram = ngram |>
                      forcats::fct_infreq() |>
                      forcats::fct_rev())
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
visualize.dictionary <- function(.data, inorder = TRUE, ignore = NULL, reorder_y = TRUE, color_y = TRUE, ...) {
  feature <- attr(.data, "feature") |>
    as.name()
  the_feature <- rlang::enquo(feature)
  if ("doc_id" %in% colnames(.data) && inorder) {
    .data <- .data |>
      dplyr::mutate(doc_id = forcats::fct_inorder(doc_id))
  }

  if (!is.null(ignore)) {
    .data <- .data |>
      dplyr::mutate(
        {{ feature }} := dplyr::case_when(
          {{ feature }} %in% ignore ~ NA_character_,
          TRUE ~ {{ feature }}
        ))
  }

  cl <- match.call()

  .data |>
    plot_doc_word_bars(feature = !!the_feature, reorder_y = reorder_y, color_y = color_y, ...)
}

#' @export
visualize.tf_idf <- function(.data, simplify = TRUE, ...) {
  the_plot <- .data |>
    plot_tf_idf(...)

  if ("doc_id" %in% colnames(.data) &&
      length(unique(.data$doc_id)) > 6 &&
      simplify) {
    the_plot <- the_plot +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank())
  }

  the_plot
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
