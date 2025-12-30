#' Plot a heatmap of ranked features
#'
#' @param data A tidy data frame, potentially containing columns called "doc_id" and "word"
#' @param rows The ranks to show, not counting ties
#' @param by The column used for document grouping, with doc_id as the default
#' @param feature The column to measure, as in "word" or "lemma"
#' @param label Whether to show the rank as a label in the heatmap
#'
#' @returns A ggplot object
#' @family visualizing helpers
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts(lemma = TRUE) |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' # Make a smaller example
#' selected_titles <-
#'   c("The Sisters", "An Encounter", "Araby",
#'     "Counterparts", "The Dead")
#'
#' dubliners |>
#'   dplyr::filter(doc_id %in% selected_titles) |>
#'   plot_doc_word_heatmap()
#'
#' dubliners |>
#'   dplyr::filter(doc_id %in% selected_titles) |>
#'   plot_doc_word_heatmap(feature = lemma, rows = 1:6)
#' }
plot_doc_word_heatmap <- function(
    data,
    rows = 1:10,
    by = doc_id,
    feature = word,
    label = TRUE){
  the_df <- data |>
    dplyr::count({{ by }}, {{ feature }}, sort = TRUE) |>
    dplyr::ungroup() |>
    dplyr::slice(
      rows,
      .by = {{ by }}) |>
    dplyr::group_by({{ by }}) |>
    dplyr::arrange(n) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::mutate(true_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by({{ feature }}) |>
    dplyr::mutate(ranksum = -sum(rank)) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(ranksum)) |>
    dplyr::mutate(
      {{ feature }} := forcats::fct_inorder({{ feature }},
                                ordered = TRUE)) |>
    dplyr::group_by({{ by }}) |>
    dplyr::mutate(docranksum = -sum(ranksum)) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(docranksum)) |>
    dplyr::mutate(
      {{ by }} := forcats::fct_inorder({{ by }},
                              ordered = TRUE),
      true_rank = factor(true_rank))

  feature_order <- the_df[[deparse(substitute(feature))]] |> levels()
  doc_order <- the_df[[deparse(substitute(by))]] |> unique()

  the_df <- the_df |>
    dplyr::select({{ by }}, {{ feature }}, true_rank) |>
    tidyr::pivot_wider(names_from = {{ by }},
                values_from = true_rank) |>
    tidyr::pivot_longer(
      cols = -c({{ feature }}),
      names_to = deparse(substitute(by)),
      values_to = "true_rank") |>
    dplyr::mutate(
      {{ by }} := factor({{ by }}, levels = doc_order,
                         ordered = TRUE),
      {{ feature }} := factor({{ feature }}, levels = feature_order), ordered = TRUE)

  max_rank <- the_df |>
    tidyr::drop_na(true_rank) |>
    dplyr::pull(true_rank) |>
    as.character() |>
    as.numeric() |>
    max()

  the_plot <- the_df |>
    ggplot2::ggplot(ggplot2::aes(x = {{ by }},
               y = {{ feature }}))

  if(label){
    the_plot <- the_plot +
      ggplot2::geom_tile(
        data = the_df,
        ggplot2::aes(fill = true_rank |>
              as.character() |>
              as.numeric()),
        color = "gray",
        show.legend = FALSE) +
      ggplot2::geom_text(
        data = ~ dplyr::filter(.x, !is.na(true_rank)),
        ggplot2::aes(label = true_rank,
            color = as.numeric(as.character(true_rank)) > (max_rank/2)),
        show.legend = FALSE) +
      ggplot2::scale_fill_viridis_c(
        option = "viridis",
        na.value = "white")
  } else {
    the_plot <- the_plot +
      ggplot2::geom_tile(ggplot2::aes(fill = true_rank),
                         color = "gray") +
      ggplot2::scale_fill_viridis_d(
        option = "viridis",
        na.value = "white",
        na.translate = FALSE)
  }

  the_plot <- the_plot +
    ggplot2::scale_color_manual(values = c("white", "black"))

  the_plot <- the_plot +
    ggplot2::scale_x_discrete(position = "top",
                     expand = c(0,0)) +
    ggplot2::labs(x = NULL, fill = "rank") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 0),
      panel.grid = ggplot2::element_blank())

  if (label) {
    the_plot +
      ggplot2::theme(legend.position = "none")
  } else {
    the_plot +
      ggplot2::guides(fill = ggplot2::guide_legend(keywidth = 0.8,
                                 keyheight = 0.8))
  }
}

#' Plot bar graphs of frequent features
#'
#' @param data A tidy data frame, potentially containing columns called "doc_id" and "word"
#' @param rows The features to show
#' @param by The column used for document grouping, with doc_id as the default
#' @param feature The column to measure, as in "word" or "lemma"
#' @param inorder Whether to retain the factor order of the "by" column
#' @param reorder_y Whether to reorder the Y-values by facet
#' @param color_y Whether bars should be filled by Y-values
#' @param allow_y_blanks The threshold number of blanks to allow on the Y-axis before dropping all blanks and reprinting values for each facet with a freely scaled Y-axis
#' @param percents Whether to display word frequencies as percentage instead of raw counts
#' @param label Whether to show the value as a label with each bar
#' @param label_tweak The numeric value by which to tweak the label, if shown. For percentages, this value adjusts the decimal-point precision. For raw counts, this value adjusts labels' offset from the bars
#' @param label_inside Whether to show the value as a label inside each bar
#' @param label_color The color to use for the label text. If no value is chosen, labels will be black when `label_inside` is `FALSE` and white when `TRUE`.
#' @param na_rm Whether to drop empty features
#'
#' @returns A ggplot object
#' @family visualizing helpers
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts(lemma = TRUE) |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' dubliners |>
#'   plot_doc_word_bars(rows = 1:4)
#'
#' dubliners |>
#'   dplyr::filter(doc_id %in% c("The Sisters", "The Dead")) |>
#'   plot_doc_word_bars(feature = lemma, rows = 1:20)
#' }
plot_doc_word_bars <- function(
    data,
    rows = 1:10,
    by = doc_id,
    feature = word,
    inorder = TRUE,
    reorder_y = NULL,
    color_y = FALSE,
    allow_y_blanks = 2,
    percents = TRUE,
    label = NULL,
    label_tweak = 2,
    label_inside = FALSE,
    label_color = NULL,
    na_rm = TRUE
){
  # browser()
  label <- label %||% label_inside
  precision <- label_tweak + 1
  offset <- label_tweak + 2

  if (!"n" %in% colnames(data)) {
    data <- data |>
      dplyr::count({{ by }}, {{ feature }}) |>
      dplyr::ungroup()
  }

  if (is.null(reorder_y) && color_y) {
    reorder_y <- FALSE
  } else if (is.null(reorder_y)) {
    reorder_y <- TRUE
  }

  if (inorder) {
    data <- data |>
      dplyr::mutate({{ by }} := forcats::fct_inorder({{ by }}))
  }

  if (percents) {
    data <- data |>
      dplyr::ungroup() |>
      dplyr::select({{ by }}, {{ feature }}, n) |>
      dplyr::distinct() |>
      dplyr::mutate(
        n = n / sum(n),
        .by = {{ by }})
  }

  if (na_rm) {
    data <- data |>
      tidyr::drop_na({{ feature }})
  }

  data <- data |>
    dplyr::ungroup() |>
    dplyr::select({{ by }}, {{ feature }}, n) |>
    dplyr::distinct() |>
    dplyr::group_by({{ by }}) |>
    dplyr::arrange(dplyr::desc(n), .by_group = TRUE) |>
    dplyr::ungroup() |>
    dplyr::slice(
      rows,
      .by = {{ by }}) |>
    dplyr::group_by({{ by }}) |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::ungroup()

  if (color_y) {
    data <- data |>
      dplyr::mutate(
        fill_it = reorder({{ feature }}, -n))
  }

  if (reorder_y) {
    data <- data |>
      dplyr::mutate(
        {{ feature }} := tidytext::reorder_within(
          {{ feature }},
          by = n,
          within = {{ by }}))
  } else {
    data <- data |>
      dplyr::mutate(
        {{ feature }} := forcats::fct_reorder(
          .f = {{ feature }},
          .x = n,
          .fun = sum,
          .na_rm = TRUE,
          .desc = FALSE))
  }

  # if (reorder_y && !color_y) {
  #   data <- data |>
  #     dplyr::mutate(
  #       {{ feature }} := tidytext::reorder_within(
  #         {{ feature }},
  #         by = n,
  #         within = {{ by }}))
  # } else if (reorder_y && color_y) {
  #   data <- data |>
  #     dplyr::mutate(
  #       fill_it = reorder({{ feature }}, -n),
  #       {{ feature }} := tidytext::reorder_within(
  #         {{ feature }},
  #         by = n,
  #         within = {{ by }}))
  # } else if (!reorder_y && color_y) {
  #   data <- data |>
  #     dplyr::mutate(
  #       fill_it = reorder({{ feature }}, -n),
  #       {{ feature }} := forcats::fct_reorder(
  #         .f = {{ feature }},
  #         .x = n,
  #         .fun = max,
  #         .na_rm = TRUE,
  #         .desc = FALSE))
  # } else if (!reorder_y) {
  #   data <- data |>
  #     dplyr::mutate(
  #       {{ feature }} := forcats::fct_reorder(
  #           .f = {{ feature }},
  #           .x = n,
  #           .fun = max,
  #           .na_rm = TRUE,
  #           .desc = FALSE))
  # }

  prefix <- data |>
    colnames() |>
    stringr::str_subset(
      "^doc_id$",
      negate = TRUE) |>
    stringr::str_subset(
      "^n$",
      negate = TRUE)

  if (percents) {
    x_lab <- paste(prefix, "frequency")
  } else {
    x_lab <- paste(prefix, "count")
  }

  # internal_data <<- data

  the_plot <- data |>
    internal_plot_word_bars(
      n, rlang::enquo(by), color_y, rlang::enquo(feature), percents, label, label_tweak, label_inside, label_color)

  if (reorder_y) {
    the_plot <- the_plot +
      tidytext::scale_y_reordered()
  }

  # --- Avoid reprinting Y-axis values where possible ---
  # If every feature is found in every document, only print
  # axis values once in each facet row. If some features are
  # missing in some document(s), it's ok to have some Y-axis
  # values with blanks, and have all Y-axis values printed
  # just once in each facet row. But some number of blanks
  # on the Y-axis is too much; when blanks surpass an
  # `allow_y_blanks` threshold, then drop all blanks and
  # reprint all Y-axis values in each facet. - v0.6
  # 1. Get the total number of unique features
  y_features <- data |>
    dplyr::pull({{ feature }}) |>
    as.character() |>
    unique() |>
    length()

  # 2. Get the total number of rows
  mode_y_length <- data |>
    dplyr::count({{ by }}) |>
    dplyr::pull(n) |>
    table() |>
    {\(x) {names(x)[which.max(x)]}}() |>
    as.numeric()

  # 3. Get the number of facets
  plot_facets <- data |>
    dplyr::pull({{ by }}) |>
    unique() |>
    length()

  # 4. Get the number of expected features
  expected_features <- ifelse(reorder_y, mode_y_length * plot_facets, mode_y_length)

  # 5. Is #1 is greater than #4 by more than allow_y_blanks?
  above_threshold <- y_features > (expected_features + allow_y_blanks)

  # For testing only
  # message(paste("Above threshold?", above_threshold))
  # message(paste("Num features is", y_features, "the mode is", mode_y_length, "the facets are", plot_facets, "and the expected features is", expected_features))

  # define facet scales logically - v0.6
  if (percents && (above_threshold | reorder_y)) {
    f_scales <- "free_y"
  } else if (!percents && (above_threshold | reorder_y)) {
    f_scales <- "free"
  } else if (!percents) {
    f_scales <- "free_x"
  } else {
    f_scales <- "fixed"
  }

  if (length(unique(data[[deparse(substitute(by))]])) > 1) {# 0.6
    the_plot <- the_plot +
      ggplot2::facet_wrap(
        ggplot2::vars({{ by }}),
        scales = f_scales,
        labeller = ggplot2::labeller({{ by }} := ggplot2::label_wrap_gen(18)))
  }

  the_plot +
    ggplot2::labs(x = x_lab)
}

internal_plot_word_bars <- function(
    data,
    x_value,
    by,
    color_y,
    feature,
    percents,
    label,
    label_tweak,
    label_inside,
    label_color
){
  precision <- label_tweak + 1
  offset <- label_tweak + 2

  if (color_y &&
      !"fill_it" %in% colnames(data)) {
    the_plot <- data |>
      ggplot2::ggplot(ggplot2::aes(x = {{ x_value }},
                                   y = !! feature,
                                   fill = !! feature))
  } else if (color_y) {
    the_plot <- data |>
      ggplot2::ggplot(ggplot2::aes(x = {{ x_value }},
                                   y = !! feature,
                                   fill = fill_it))
  } else {
    the_plot <- data |>
      ggplot2::ggplot(ggplot2::aes(x = {{ x_value }},
                                   y = !! feature,
                                   fill = !! by))
  }

  the_plot <- the_plot +
    ggplot2::geom_col(
      show.legend = FALSE,
      color = NA)

  if (label & percents & !label_inside) {
    the_plot <- the_plot +
      ggplot2::geom_text(
        ggplot2::aes(
          x = n + 0.0005,
          label = 100 *
            round(n,
                  precision)),
        color = label_color %||% "black",
        hjust = 0)
  } else if (label & percents & label_inside) {
    the_plot <- the_plot +
      ggplot2::geom_text(
        ggplot2::aes(
          x = n - 0.0005,
          label = 100 *
            round(n,
                  precision)),
        color = label_color %||% "white",
        hjust = 1)
  } else if (label & !label_inside) {
    the_plot <- the_plot +
      ggplot2::geom_text(
        ggplot2::aes(
          x = n + offset,
          label = n),
        color = label_color %||% "black",
        hjust = 0)
  } else if (label & label_inside) {
    the_plot <- the_plot +
      ggplot2::geom_text(
        ggplot2::aes(
          x = n - offset,
          label = n),
        color = label_color %||% "white",
        hjust = 1)
  }

  if (percents) {
    the_plot <- the_plot +
      ggplot2::scale_x_continuous(
        labels = scales::label_percent(),
        expand = ggplot2::expansion(mult = c(0, 0.05)))

    if (length(unique(data[[deparse(substitute(by))]])) > 1) {
      the_plot <- the_plot +
        ggplot2::facet_wrap(ggplot2::vars({{ by }}),
                            scales = "free_y")
    }

  } else {
    the_plot <- the_plot +
      ggplot2::scale_x_continuous(
        labels = scales::label_comma(),
        expand = ggplot2::expansion(mult = c(0, 0.05)))

    if (length(unique(data[[deparse(substitute(by))]])) > 1) {
      the_plot <- the_plot +
        ggplot2::facet_wrap(ggplot2::vars({{ by }}),
                            scales = "free")
    }
  }

  the_plot <- the_plot +
    ggplot2::labs(y = NULL)

  the_plot +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank())
}
