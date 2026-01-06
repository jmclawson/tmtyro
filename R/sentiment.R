#' Add sentiment markers
#'
#' [add_sentiment()] provides simple lexicon-based measures of sentiment, comparing words in a text to one of a number of controlled dictionaries.
#'
#' @param data A tidy data frame, potentially containing a column called "word"
#' @param lexicon The sentiment lexicon to use from the \link[tidytext]{tidytext} package. Options include "bing", "afinn", "loughran", "nrc", "nrc_eil", or "nrc_vad"
#' @param feature A column of words containing one word per row, to be used for dictionary look-up
#' @param label Whether to label variables added to data frame
#'
#' @returns The original data frame with one or more sentiment columns added.
#' @export
#'
#' @examples
#' \dontrun{
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' dubliners |>
#'   add_sentiment() |>
#'   drop_na() |>
#'   head()
#' }
add_sentiment <- function(
    data,
    lexicon = c("bing", "afinn", "loughran",
                "nrc", "nrc_eil", "nrc_vad"),
    feature = word,
    label = NULL) {
  lexicon <- tolower(lexicon)
  lex <- match.arg(lexicon)
  lex_name <- lex |>
    stringr::str_replace_all(
      c(
        "bing" = "Bing",
        "afinn" = "AFINN",
        "loughran" = "Loughran",
        "nrc" = "NRC",
        "nrc_eil" = "NRC EIL",
        "nrc_vad" = "NRC VAD"
      )
    )
  rlang::check_installed("textdata", reason = "for downloading sentiment lexicons")
  if (lex %in% c("bing", "afinn", "loughran",
                 "nrc")) {
    lex_df <- tidytext::get_sentiments(lex)
  } else {
    lex_df <- switch(
      lex,
      "nrc_eil"  = textdata::lexicon_nrc_eil(),#score = num, AfDi=chr
      "nrc_vad"  = textdata::lexicon_nrc_vad(),# v/a/d = num/num/num
      stop("Unexpected lexicon", call. = FALSE)
    )
  }

  colnames(lex_df) <- colnames(lex_df) |>
    tolower() |>
    stringr::str_replace_all("affectdimension", "affect_dimension")

  lex_df <- lex_df |>
    dplyr::rename({{ feature }} := word)

  if (lex == "afinn") {
    lex_df <- lex_df |>
      dplyr::rename(sentiment = value)
  } else if (lex == "nrc_eil") {
    lex_df <- lex_df |>
      dplyr::rename(sentiment_score = score,
                    sentiment_dimension = affect_dimension)
  } else if (lex == "nrc_vad") {
    lex_df <- lex_df |>
      dplyr::rename(sentiment_valence = valence,
                    sentiment_arousal = arousal,
                    sentiment_dominance = dominance)
  }

  if (any(colnames(dplyr::select(lex_df, -{{ feature }})) %in% colnames(data))) {
    stop("The following sentiment columns already exist:",
         intersect(colnames(dplyr::select(lex_df, -{{ feature }})),
                   colnames(data)))
  }

  out <- suppressMessages(dplyr::left_join(data, lex_df, by = {{ feature }}))

  feature_string <- deparse(substitute(feature))
  by_string <- deparse(substitute(by))

  if (tmtyro_use_labels(label)) {
    out <- assign_labels(
      out, "sentiment",
      feature_string,
      secondary = lex_name)
  }

  if (tmtyro_use_log()) {
    attr(out, "tmtyro_log") <- attr(data, "tmtyro_log")
    out <- out |>
      add_logstep(
        fn = "add_sentiment",
        arguments = list(
          feature = feature_string,
          lexicon = lex_name))
  }

  out |>
    add_class("sentiment")
}
