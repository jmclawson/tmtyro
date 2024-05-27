#' Measure sentiment
#'
#' [add_sentiment()] provides simple lexicon-based measures of sentiment, comparing words in a text to one of a number of controlled dictionaries.
#'
#' @param df A tidy data frame, potentially containing a column called "word"
#' @param lexicon The sentiment lexicon to use from the [tidytext] package. Options include "bing", "afinn", "loughran", "nrc", "nrc_eil", or "nrc_vad".
#' @param feature A column of words containing one word per row, to be used for dictionary look-up.
#'
#' @returns The original data frame with one or more sentiment columns added.
#' @export
#'
#' @examples
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' dubliners |>
#'    add_sentiment() |>
#'    drop_empty() |>
#'    head()
add_sentiment <- function(
    df,
    lexicon = c("bing", "afinn", "loughran",
                "nrc", "nrc_eil", "nrc_vad"),
    feature = word) {
  lex <- match.arg(lexicon)
  rlang::check_installed("textdata", reason = "for downloading sentiment lexicons")
  lex_df <- switch(lex,
    "afinn"    = textdata::lexicon_afinn(),#value = numeric
    "nrc"      = textdata::lexicon_nrc(),#senti = chr
    "nrc_eil"  = textdata::lexicon_nrc_eil(),#score = num, AfDi=chr
    "nrc_vad"  = textdata::lexicon_nrc_vad(),# v/a/d = num/num/num
    "loughran" = textdata::lexicon_loughran(),#senti = chr
    "bing"     = textdata::lexicon_bing(),#senti = chr
    stop("Unexpected lexicon", call. = FALSE)
  )

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

  if (any(colnames(dplyr::select(lex_df, -{{ feature }})) %in% colnames(df))) {
    stop("The following sentiment columns already exist:",
         intersect(colnames(dplyr::select(lex_df, -{{ feature }})),
                   colnames(df)))
  }

  suppressMessages(dplyr::left_join(df, lex_df, by = {{ feature }})) |>
    add_class("sentiment")
}
