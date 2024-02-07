#' Remove stopwords
#'
#' @param df A tidy data frame, potentially containing a column called "word"
#' @param wordlist A list of stopwords
#' @param word A column of words containing one word per row to be checked for stopwords.
#'
#' @returns The original data frame with fewer rows.
#' @export
#'
#' @examples
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen |>
#'    drop_stopwords()
drop_stopwords <- function(df, wordlist = stopwords::stopwords(), word = word) {
  df |>
    dplyr::filter(!tolower({{ word }}) %in% wordlist)
}
