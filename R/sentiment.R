#' Measure sentiment
#'
#' [add_sentiment()] provides simple lexicon-based measures of sentiment, comparing words in a text to one of a number of controlled dictionaries.
#'
#' @param df A tidy data frame, potentially containing a column called "word"
#' @param lexicon The sentiment lexicon to use from the [tidytext] package. Options include "bing", "afinn", "loughran", "nrc", "nrc_eil", or "nrc_vad".
#' @param word A column of words containing one word per row, to be used for dictionary look-up.
#'
#' @returns The original data frame with one or more sentiment columns added.
#' @export
#'
#' @examples
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen |>
#'    add_sentiment() |>
#'    tidyr::drop_na() |>
#'    head()
add_sentiment <- function(
    df,
    lexicon = c("bing", "afinn", "loughran",
                "nrc", "nrc_eil", "nrc_vad"),
    word = word) {
  lex <- match.arg(lexicon)

  lex_df <- switch(lex,
    "afinn"    = textdata::lexicon_afinn(),#value = numeric
    "nrc"      = textdata::lexicon_nrc(),#senti = chr
    "nrc_eil"  = textdata::lexicon_nrc_eil(),#score = num, AfDi=chr
    "nrc_vad"  = textdata::lexicon_nrc_vad(),# v/a/d = num/num/num
    "loughran" = textdata::lexicon_loughran(),#senti = chr
    "bing"     = textdata::lexicon_bing(),#senti = chr
    stop("Unexpected lexicon", call. = FALSE)
  ) |>
    janitor::clean_names() |>
    dplyr::rename({{ word }} := word)

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

  if (any(colnames(dplyr::select(lex_df, -{{ word }})) %in% colnames(df))) {
    stop("The following sentiment columns already exist:",
         intersect(colnames(dplyr::select(lex_df, -{{ word }})),
                   colnames(df)))
  }

  df |>
    dplyr::left_join(lex_df, by = {{ word }})
}
