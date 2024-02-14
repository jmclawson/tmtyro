measure_tf_idf <- function(df, by = doc_id, feature = word) {
  df |>
    dplyr::count({{ by }}, {{ feature }}) |>
    tidytext::bind_tf_idf(
      term = {{ feature }},
      document = {{ by }},
      n = n) |>
    dplyr::arrange(dplyr::desc(tf_idf))
}

plot_tf_idf <- function(
    df,
    num = 10,
    by = doc_id,
    feature = word,
    label = FALSE,
    label_tweak = 2,
    label_inside = FALSE,
    colorset = "default",
    outline_color = NA
){
  if (!"tf_idf" %in% colnames(df)) {
    df <- df |>
      measure_tf_idf(by = {{ by }}, feature = {{ feature }})
  }

  df <- df |>
    dplyr::slice_max(
      order_by = tf_idf,
      n = num,
      by = {{ by }}) |>
    dplyr::group_by({{ by }}) |>
    dplyr::arrange(dplyr::desc(tf_idf)) |>
    dplyr::mutate(
      {{ feature }} := tidytext::reorder_within(
        {{ feature }},
        by = tf_idf,
        within = {{ by }})) |>
    dplyr::ungroup()

  if (deparse(substitute(feature)) == "word") {
    prefix <- "term"
  } else {
    prefix <- deparse(substitute(feature))
  }

  x_lab <- paste(prefix, "frequency–inverse document frequency")

  df |>
    internal_plot_word_bars(
    tf_idf, rlang::enquo(by), rlang::enquo(feature), FALSE, label, label_tweak, label_inside, colorset, outline_color) +
    ggplot2::labs(x = x_lab)
}
