% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topic_model.R
\name{plot_topic_wordcloud}
\alias{plot_topic_wordcloud}
\title{Plot topic wordclouds}
\usage{
plot_topic_wordcloud(lda, topics = NULL, crop = TRUE, savedir = "plots")
}
\arguments{
\item{lda}{The topic model to be used.}

\item{topics}{Topic numbers to be visualized. If left undefined, all topics will be visualized}

\item{crop}{Whether to remove white space from visualized word clouds}

\item{savedir}{The directory to save plots in. Defaults to "plots"}
}
\value{
Graphic(s) prepared with \code{knitr} for Quarto or RMarkdown
}
\description{
\code{plot_topic_wordcloud()} prepares, saves, and displays word clouds of topics in a topic model. The function can display word clouds of one or more specific topics, or it can show word clouds for every topic.
}
\examples{
\dontrun{
austen <-
  get_gutenberg_corpus(c(105, 121, 141, 158, 161, 946, 1342)) |>
  select(doc_id = title, text)

# Creating the topic model for the package
austen_lda <- make_topic_model(austen, k = 30)

saveRDS(austen_lda, "inst/austen_lda.rds")
}

# Loading from the package
austen_lda <- "austen_lda.rds" |>
  system.file(package = "tmtyro") |>
  readRDS()

austen_lda |>
  plot_topic_wordcloud(topic = 28)
}
