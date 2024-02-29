% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lexical_variety.R
\name{plot_vocabulary}
\alias{plot_vocabulary}
\title{Show vocabulary growth}
\usage{
plot_vocabulary(
  df,
  x = progress_words,
  by = doc_id,
  identity = doc_id,
  descriptive_labels = TRUE,
  labeling = c("point", "inset", "inline", "axis")
)
}
\arguments{
\item{df}{A tidy data frame, potentially containing columns called "doc_id" and "word"}

\item{x}{A column showing the cumulative count of words}

\item{by}{A grouping column for colors and labels}

\item{identity}{A grouping column for lines}

\item{descriptive_labels}{A toggle for disabling descriptive labels of progress_percent on the X-axis}

\item{labeling}{Options for labeling groups:
\itemize{
\item \code{"point"} labels the final value
\item \code{"inline"} prints the label within a smoothed curve
\item \code{"axis"} prints labels where a secondary Y-axis might go
\item \code{"inset"} prints a legend within the plot area
\item Anything else prints a legend to the right of the plot area.
}}
}
\value{
A ggplot object
}
\description{
\code{plot_vocabulary()} visualizes the vocabulary growth as new words are used in each document.
}
\examples{
austen <- "austen.rds" |>
  system.file(package = "tmtyro") |>
  readRDS()

austen_measured <- austen |>
  measure_lexical_variety()

austen_measured |>
  standardize_titles() |>
  plot_vocabulary(progress_percent)

austen_measured |>
  standardize_titles() |>
  plot_vocabulary()

\dontrun{
  micusp_corpus(
    discipline \%in\% c("Physics", "Economics")) |>
    load_texts() |>
    measure_lexical_variety() |>
    plot_vocabulary(by = discipline)
}
}