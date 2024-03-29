% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ngrams.R
\name{separate_ngrams}
\alias{separate_ngrams}
\title{Separate one word per column}
\usage{
separate_ngrams(df, names_prefix = "word")
}
\arguments{
\item{df}{A tidy data frame containing a column called "ngram"}

\item{names_prefix}{The prefixed name of the new columns, as in "word_1", "word_2", etc.}
}
\value{
A data frame with one column separated into many
}
\description{
Separate one word per column
}
\examples{
\dontrun{
  my_corpus <- load_texts(n = 2)

  my_bigrams <- my_corpus |>
    separate_ngrams()
}

austen <- "austen.rds" |>
  system.file(package = "tmtyro") |>
  readRDS()

austen |>
  add_ngrams(2) |>
  combine_ngrams() |>
  separate_ngrams() |>
  head()
}
