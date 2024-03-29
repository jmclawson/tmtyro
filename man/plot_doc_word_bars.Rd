% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize_document_top_n.R
\name{plot_doc_word_bars}
\alias{plot_doc_word_bars}
\title{Plot bar graphs of frequent features}
\usage{
plot_doc_word_bars(
  df,
  num = 10,
  by = doc_id,
  feature = word,
  percents = TRUE,
  label = FALSE,
  label_tweak = 2,
  label_inside = FALSE,
  colorset = "default",
  outline_color = NA,
  na_rm = TRUE
)
}
\arguments{
\item{df}{A tidy data frame, potentially containing columns called "doc_id" and "word"}

\item{num}{The number of features to show}

\item{by}{The column used for document grouping, with doc_id as the default}

\item{feature}{The column to measure, as in "word" or "lemma"}

\item{percents}{Whether to display word frequencies as percentage instead of raw counts; defaults to TRUE}

\item{label}{Whether to show the value as a label with each bar; defaults to FALSE}

\item{label_tweak}{The numeric value by which to tweak the label, if shown. For percentages, this value adjusts the decimal-point precision. For raw counts, this value adjusts labels' offset from the bars}

\item{label_inside}{Whether to show the value as a label inside each bar; defaults to FALSE}

\item{colorset}{The color palette to use, whether "default", "okabe", or one of the named qualitative palettes from Viridis or Color Brewer}

\item{outline_color}{The color to use for the outside of each bar. By default, no color is used.}

\item{na_rm}{Whether to drop empty features}
}
\value{
A ggplot object
}
\description{
Plot bar graphs of frequent features
}
\examples{
library(tmtyro)
library(dplyr)

austen <- "austen.rds" |>
  system.file(package = "tmtyro") |>
  readRDS()

austen |>
  plot_doc_word_bars()

austen |>
  mutate(
    pos_pair = paste(pos, lead(pos)),
    `adjective + noun bigram` = paste(word, lead(word))) |>
  filter(stringr::str_detect(pos_pair, "JJ N")) |>
  standardize_titles() |>
  plot_doc_word_bars(
    num = 5,
    feature = `adjective + noun bigram`,
    colorset = "Pastel2",
    percents = FALSE,
    label = TRUE,
    label_inside = TRUE,
    label_tweak = -1)
}
