#' Plot a heatmap of ranked features
#'
#' @param .data A tidy data frame, potentially containing columns called "doc_id" and "word"
#' @param num The number of ranks to show, not counting ties
#' @param by The column used for document grouping, with doc_id as the default
#' @param feature The column to measure, as in "word" or "lemma"
#' @param label Whether to show the rank as a label in the heatmap
#' @param na_color The color to show NA values; default is "white"
#' @param colorset The viridis color palette to use; alternatively, a two-item vector of colors to use as gradient
#' @param line_color The color of grid lines in the heatmap
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen |>
#'   dplyr::filter(pos %in% c("NN", "NNS")) |>
#'   plot_doc_word_heatmap()
#'
#' austen |>
#'   dplyr::filter(pos %in% c("JJ", "RB")) |>
#'   plot_doc_word_heatmap(feature = lemma, colorset = c("white", "red"))
#'
plot_doc_word_heatmap <- function(
    .data,
    num = 10,
    by = doc_id,
    feature = word,
    label = TRUE,
    na_color = "white",
    colorset = "viridis",
    line_color = "gray"){
  the_df <- .data |>
    dplyr::count({{ by }}, {{ feature }}) |>
    dplyr::slice_max(
      n = num,
      by = {{ by }},
      order_by = n) |>
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
        color = line_color,
        show.legend = FALSE) +
      ggplot2::geom_text(
        data = ~ dplyr::filter(.x, !is.na(true_rank)),
        ggplot2::aes(label = true_rank,
            color = as.numeric(as.character(true_rank)) > (max_rank/2)),
        show.legend = FALSE
        )

    if(length(colorset) == 2) {
      if(colorset[2] == "white"){
        text_low <- "black"
        text_high <- "white"
      } else {
        text_low <- "white"
        text_high <- "black"
      }
      the_plot <- the_plot +
        ggplot2::scale_fill_gradient(na.value = na_color,
                            low = colorset[2],
                            high = colorset[1]) +
        ggplot2::scale_color_manual(values = c(text_low, text_high))
    } else {
      the_plot <- the_plot +
        ggplot2::scale_fill_viridis_c(option = colorset,
                             na.value = na_color) +
        ggplot2::scale_color_manual(values = c("white", "black"))
    }
  } else {
    the_plot <- the_plot +
      ggplot2::geom_tile(ggplot2::aes(fill = true_rank),
                color = line_color) +
      ggplot2::scale_fill_viridis_d(
        option = colorset,
        na.value = na_color,
        na.translate = TRUE,
        breaks = 1:max_rank)
  }

  the_plot <- the_plot +
    ggplot2::scale_x_discrete(position = "top",
                     expand = c(0,0)) +
    ggplot2::labs(x = NULL, fill = "rank") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0),
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
