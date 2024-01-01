#' Construct a topic model
#'
#' `make_topic_model()` moves a table of texts through the necessary steps of preparation before building a topic model. The function applies seven steps:
#' 1. identifies text divisions by the `doc_id` column
#' 2. divides each of the texts into same-sized chunks of `sample_size` words (default is 1000 words)
#' 3. unnests text table into a table with one word per row
#' 4. removes stop words and proper nouns (identified as any word that only appears with a capitalized first letter)
#' 5. counts word frequencies for each chunk
#' 6. converts the table of frequencies into a document term matrix
#' 7. builds a topic model with `k` topics
#'
#' @param df A data frame with unnested text in a "word" column.
#' @param doc_id The column for identifying each document. By default, the "title" column will be used.
#' @param sample_size The sample size for each document chunk. By default, samples will include 1000 words.
#' @param k The number of topics to search for. By default, 15 topics will be sought.
#'
#' @returns A topic model.
#' @export
#'
#' @examples
#' \dontrun{
#' mysteries <- load_texts("mystery-novels", word = FALSE)
#'
#' mysteries_lda <- mysteries |>
#'   make_topic_model(k = 10)
#'   }
make_topic_model <- function(
    df,
    doc_id = title,
    sample_size = 1000,
    k = 15) {

  set_doc_samples <- function(
    df,
    size = 1000,
    doc_id = title,
    set_min = NULL,
    collapse_cols = TRUE) {

    df <- df |>
      dplyr::group_by({{doc_id}}) |>
      dplyr::mutate(set_id =
               ceiling(dplyr::row_number()/size)) |>
      dplyr::ungroup()

    if (!is.null(set_min)) {
      df <- df |>
        dplyr::group_by({{doc_id}}) |>
        dplyr::mutate(set_count = dplyr::n()) |>
        dplyr::filter(set_count > set_min)
    }

    if(collapse_cols) {
      df <- df |>
        tidyr::unite({{doc_id}}, {{doc_id}}, set_id)
    }

    return(df)
  }

  df |>
    unnest_without_caps() |>
    set_doc_samples(doc_id = {{doc_id}},
                    size = sample_size) |>
    dplyr::anti_join(tidytext::get_stopwords()) |>
    dplyr::count({{doc_id}}, word, sort = TRUE) |>
    dplyr::rename(
      document = {{doc_id}},
      term = word,
      value = n) |>
    tidytext::cast_dtm(document, term, value) |>
    topicmodels::LDA(k = k,
        method = "Gibbs",
        control = list(best = TRUE,
                       initialize = "random"))
}

#' Plot topic distributions
#'
#' `plot_document_topics()` prepares a visualization for exploring the most significant topics in each document over time.
#'
#' @param lda The topic model to be used.
#' @param top_n The number of topics to visualize. By default, the top 4 topics in each document will be shown.
#' @param direct_label By default, directly labeles topic numbers on the chart. Set it to FALSE to show a legend corresponding to each color.
#' @param title By default, the function will add a title to the chart, corresponding to the name of the object passed to the `lda` parameter. Set it to FALSE to return a chart with no title.
#' @param save By default, the visualization will be saved. Set to FALSE to skip saving.
#' @param saveas The filetype for saving resulting visualizations. By default, the files will be in "png" format, but other options such as "pdf" or "jpg will also work.
#' @param savedir The directory for saving output images. By default, this is set to "plots/".
#' @param omit Upon exploration, some topics may be found to contain common stop words or other unhelpful material. Use the `omit` parameter to define a vector of topic numbers you wish to omit from a visualization.
#' @param smooth After samples are rejoined, the measured value of each topic will vary wildly, even in samples that are beside each other in a document. This can make charts distractingly jittery. The default TRUE value of this parameter reduces chart noise by calculating rolling averages across three samples. Set the parameter to FALSE to skip this step and allow for visualization of extreme values.
#'
#' @returns A ggplot2 visualization showing vertical facets of texts. The length of each text is shown on the X-axis, and area plots on the Y-axis show the distribution of the strongest topics in each part of the text.
#' @export
#'
#' @examples
#' \dontrun{
#' mysteries <- load_texts("mystery-novels", word = FALSE)
#'
#' mysteries_lda <- mysteries |>
#'   make_topic_model(k = 10)
#'
#' plot_document_topics(mysteries_lda)
#' }
plot_document_topics <- function(
    lda,
    top_n = 4,
    direct_label = TRUE,
    title = TRUE,
    save = TRUE,
    saveas = "png",
    savedir = "plots",
    omit = NULL,
    smooth = TRUE) {

  df_string <- deparse(substitute(lda))

  plot_topic_parts <- function(df,
                               direct_label = TRUE) {
    plot <- df |>
      ggplot2::ggplot(ggplot2::aes(x = set, y = n))

    if (direct_label) {
      plot <- plot +
        ggplot2::geom_area(ggplot2::aes(fill = as.factor(topic)),
                  show.legend = FALSE) +
        ggplot2::geom_text(
          data = df |>
            dplyr::group_by(doc_id) |>
            dplyr::filter(set == max(set)) |>
            dplyr::arrange(dplyr::desc(topic)) |>
            dplyr::mutate(n = cumsum(n),
                   set = set + (3000 * (
                     dplyr::row_number() - 1
                   ))),
          ggplot2::aes(x = set + 800,
              label = topic,
              color = as.factor(topic)),
          show.legend = FALSE,
          hjust = 0
        )
    } else {
      plot <- plot +
        ggplot2::geom_area(ggplot2::aes(fill = as.factor(topic)),
                  show.legend = TRUE)
    }
    plot +
      ggplot2::facet_wrap(~ doc_id,
                 strip.position = "top",
                 ncol = 1,
                 labeller = ggplot2::labeller(groupwrap = ggplot2::label_wrap_gen(6))) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(c(0, 0.1)),
                         labels = scales::label_comma()) +
      ggplot2::theme_minimal() +
      ggplot2::labs(y = ggplot2::element_blank(),
           x = "words",
           fill = "topic") +
      ggplot2::theme(plot.title.position = "plot",
            strip.background = ggplot2::element_rect(fill = NA, color = NA),
            strip.text = ggplot2::element_text(colour = "black",
                                      hjust = 0),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::scale_fill_viridis_d(option = "turbo")  +
      ggplot2::scale_color_viridis_d(option = "turbo") +
      ggplot2::scale_y_continuous(
        position = "right",
        labels = scales::label_percent()) +
      ggplot2::coord_cartesian(clip = 'off')
  }

  k <- attributes(lda)$k

  plot <- lda |>
    prep_document_topics(top_n = top_n,
                         omit = omit,
                         smooth = smooth) |>
    plot_topic_parts(direct_label = direct_label)

  if (title) {
    plot <- plot + ggplot2::ggtitle(df_string)
  }

  if (save) {
    ifelse(!dir.exists(file.path(savedir)),
           dir.create(file.path(savedir)),
           FALSE)
    filename <- paste0(savedir, "/",
                       df_string,
                       " - document topics",
                       ".",
                       saveas)
    if (!saveas %in% c("pdf", "png")) {
      ggplot2::ggsave(filename,
             plot = plot,
             dpi = 300,
             bg = "white")
    } else {
      ggplot2::ggsave(filename, plot = plot, dpi = 300)
    }
  }
  plot
}

# Used internally
prep_document_topics <- function(
    lda,
    top_n = 4,
    omit = NULL,
    smooth = FALSE){

  df_string <- deparse(substitute(lda))

  k <- attributes(lda)$k

  doc_tops <- lda |>
    tidytext::tidy(matrix = "gamma") |>
    tidyr::separate(document,
             c("title", "set"),
             sep = "_") |>
    dplyr::mutate(set = as.integer(set),
           ordered = TRUE) |>
    dplyr::group_by(title, topic) |>
    dplyr::mutate(topic_mean = mean(gamma,
                             na.rm = TRUE))

  if (!is.null(omit)) {
    doc_tops <- doc_tops |>
      dplyr::filter(!topic %in% omit)
  }

  doc_tops <- doc_tops |>
    dplyr::group_by(title) |>
    dplyr::mutate(topic_rank =
             dplyr::dense_rank(-topic_mean)) |>
    dplyr::ungroup() |>
    # by default, n = 4 commonest topics
    dplyr::filter(topic_rank <= top_n) |>
    # combine author and text
    dplyr::mutate(doc_id = title) |>
    dplyr::group_by(doc_id, set, topic) |>
    dplyr::summarise(n = sum(gamma, na.rm = TRUE), .groups = "keep") |>
    dplyr::mutate(percentage = n / sum(n),
           set = set * 1000)

  top_terms <-
    lda |>
    tidytext::tidy() |>
    dplyr::group_by(topic) |>
    dplyr::arrange(dplyr::desc(beta)) |>
    dplyr::slice_head(n = 10) |>
    dplyr::summarize(words = paste0(term, collapse = ", "))

  result <- doc_tops |>
    dplyr::left_join(top_terms, by = "topic") |>
    dplyr::mutate(display = paste0("topic ", topic, ": ", words))

  if(smooth) {
    result <- result |>
      dplyr::ungroup() |>
      dplyr::group_by(topic) |>
      dplyr::arrange(set) |>
      # rolling average across three sets
      dplyr::mutate(n2 = dplyr::lead(n),
             n3 = dplyr::lead(n, n=2L)) |>
      dplyr::ungroup() |>
      dplyr::rowwise() |>
      dplyr::mutate(n_smooth = mean(c(n, n2, n3), na.rm = TRUE),
             .after = n3) |>
      dplyr::select(-n, -n2, -n3) |>
      dplyr::rename(n = n_smooth)
  }

  result
}

#' Explore topics interactively
#'
#' `interactive_document_topics()` uses plotly to prepare an interactive visualization to explore a topic model, showing the top "n" topics in each document. This kind of visualization is for use in the interactive IDE or as a web page.
#'
#' @param lda The topic model to be used.
#' @param top_n The number of topics to visualize. By default, the top 4 topics in each document will be shown.
#' @param title By default, the function will add a title to the chart, corresponding to the name of the object passed to the `lda` parameter. Set it to FALSE to return a chart with no title.
#' @param height The height of the resulting HTML widget.
#' @param omit Upon exploration, some topics may be found to contain common stop words or other unhelpful material. Use the `omit` parameter to define a vector of topic numbers you wish to omit from a visualization.
#' @param smooth After samples are rejoined, the measured value of each topic will vary wildly, even in samples that are beside each other in a document. This can make charts distractingly jittery. The default TRUE value of this parameter reduces chart noise by calculating rolling averages across three samples. Set the parameter to FALSE to skip this step and allow for visualization of extreme values.
#'
#' @returns Interactive plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' mysteries <- load_texts("mystery-novels", word = FALSE)
#'
#' mysteries_lda <- mysteries |>
#'   make_topic_model(k = 10)
#'
#' interactive_document_topics(mysteries_lda, top_n = 5)
#' }
interactive_document_topics <- function(
    lda,
    top_n = 4,
    title = FALSE,
    height = NULL,
    omit = NULL,
    smooth = TRUE) {

  df_string <- deparse(substitute(lda))

  plot <- lda |>
    prep_document_topics(top_n, omit = omit, smooth = smooth) |>
    dplyr::mutate(
      # Shorten title to first word, dropping articles and prepositions
      doc_id = doc_id |>
        stringr::str_remove_all("^The\\b|^A\\b|^In the\\b|^In\\b|^To\\b") |>
        stringr::str_remove_all("^ ") |>
        strsplit(split = " ") |>
        sapply(`[`, 1) |>
        stringr::str_extract("[A-Za-z]+"),
      topic = as.factor(topic)) |>
    ggplot2::ggplot(ggplot2::aes(x = set, y = n)) +
    ggplot2::geom_area(ggplot2::aes(fill = topic,
                  color = topic,
                  text = display),
              show.legend = FALSE) +
    ggplot2::facet_grid(doc_id ~ .,
               labeller = ggplot2::labeller(groupwrap = ggplot2::label_wrap_gen(6))) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(c(0, 0.1)),
                       labels = scales::label_comma()) +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = ggplot2::element_blank(),
         x = "words",
         fill = "topic") +
    ggplot2::theme(
      plot.title.position = "plot",
      strip.background = ggplot2::element_rect(fill = "white", color = "white"),
      strip.text = ggplot2::element_text(colour = "black",
                                hjust = 0),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::scale_fill_viridis_d(alpha = 0.8) +
    ggplot2::scale_color_viridis_d(alpha = 1) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent())

  if (title) {
    plot <- plot + ggplot2::ggtitle(df_string)
  }

  plot |>
    plotly::ggplotly(tooltip = "text", height = height) |>
    plotly::hide_legend() |>
    suppressWarnings()
}

plot_topic_bars <- function(
    df,
    topics,
    top_n = 10,
    expand_bars = TRUE,
    save = TRUE,
    savedir = "plots") {

  df_string <- deparse(substitute(df))

  plot <- tidytext::tidy(df) |>
    dplyr::filter(topic %in% topics) |>
    dplyr::mutate(topic = paste("topic", topic) |>
             factor(levels = paste("topic", topics))) |>
    dplyr::group_by(topic) |>
    dplyr::arrange(dplyr::desc(beta)) |>
    dplyr::slice_head(n=top_n) |>
    dplyr::ungroup() |>
    ggplot2::ggplot(ggplot2::aes(y = tidytext::reorder_within(term, beta, topic),
               x = beta)) +
    ggplot2::geom_col(ggplot2::aes(fill = topic),
             show.legend = FALSE) +
    tidytext::scale_y_reordered() +
    ggplot2::labs(y = NULL,
         x = NULL,
         title = df_string) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank())

  if(expand_bars) {
    plot <- plot +
      ggplot2::facet_wrap(~ topic, scales = "free")
  } else {
    plot <- plot +
      ggplot2::facet_wrap(~ topic, scales = "free_y")
  }

  if (save) {
    ifelse(!dir.exists(file.path(savedir)),
           dir.create(file.path(savedir)),
           FALSE)
    filename <- paste0(savedir,"/",
                       df_string,
                       " - topics ",
                       paste0(topics, collapse=", "),
                       ".png")
    ggplot2::ggsave(filename, plot=plot)
  }
  plot
}

#' Plot topic wordclouds
#'
#' `plot_topic_wordcloud()` prepares, saves, and displays word clouds of topics in a topic model. The function can display word clouds of one or more specific topics, or it can show word clouds for every topic.
#'
#' @param df A loaded topic model
#' @param topics Topic numbers to be visualized. If left undefined, all topics will be visualized
#' @param crop Whether to remove white space from visualized word clouds
#' @param savedir The directory to save plots in. Defaults to "plots"
#'
#' @returns Graphic(s) prepared with `knitr` for Quarto or RMarkdown
#' @export
#'
#' @examples
#' \dontrun{
#' mysteries <- load_texts("mystery-novels", word = FALSE)
#'
#' mysteries_lda <- mysteries |>
#'   make_topic_model(k = 10)
#'   }
plot_topic_wordcloud <- function(
    df,
    topics = NULL,
    crop = TRUE,
    savedir = "plots") {

  save_topic_wordcloud <- function(
    df,
    topics = NULL,
    dir = "plots",
    count = 150,
    df_string = NULL){

    if(is.null(df_string)) {
      df_string <- deparse(substitute(df))
      cat("df_string was null!")
    }

    df <- tidytext::tidy(df)

    if(!is.null(topics)) {
      df <- df |> dplyr::filter(topic %in% topics)
    }

    ifelse(!dir.exists(file.path(dir)),
           dir.create(file.path(dir)),
           FALSE)

    for(t in unique(df$topic)){
      filename <- paste0(dir,"/", df_string, " - topic ", t, ".png")
      grDevices::png(filename, width = 12,
          height = 8, units = "in",
          res = 300)
      wordcloud::wordcloud(words = df |>
                  dplyr::filter(topic == t) |>
                  dplyr::pull(term),
                freq = df |>
                  dplyr::filter(topic == t) |>
                  dplyr::pull(beta),
                max.words = count,
                random.order = FALSE,
                scale=c(3, .3),
                rot.per = 0.2,
                colors=viridis::turbo(
                  n=9,
                  direction =-1)[1:8])
      grDevices::dev.off()
    }
  }

  df_string <- deparse(substitute(df))

  save_topic_wordcloud(df, topics, df_string = df_string)

  if (!is.null(topics)) {
    paths <- paste0(savedir,"/", df_string, " - topic ", topics, ".png")
  } else {
    paths <- list.files(savedir, pattern = paste0(df_string, " - topic "),
                        full.names = TRUE)
  }

  if (crop) {
    knitr::include_graphics(paths[1]) |>
      knitr::plot_crop()
  } else {
    knitr::include_graphics(paths)
  }
}
