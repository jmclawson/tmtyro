% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/change_colors.R
\name{change_colors}
\alias{change_colors}
\title{Choose other colors}
\usage{
change_colors(
  x,
  colorset = "brewer",
  palette = 2,
  kind = "qualitative",
  direction = 1,
  start = 1
)
}
\arguments{
\item{x}{A visualization made with \code{\link[=visualize]{visualize()}}}

\item{colorset}{Either "brewer", "viridis", "okabe-ito", "dubois", or a vector of colors.}

\item{palette}{The number or name of palette (dependent on setting \code{colorset} to either "brewer" or "viridis")}

\item{kind}{Used only for Brewer palettes to match numbered palette with the specific subset}

\item{direction}{The direction colors should be applied to the data. Setting to anything other than 1 will reverse the order.}

\item{start}{Useful for predefined colorsets from Okabe-Ito and Brewer to start from a color other than 1}
}
\value{
A ggplot2 object
}
\description{
\code{change_colors()} standardizes three methods for choosing color palettes for color or fill mapping, providing access to Brewer and Viridis palettes alongside custom choices.
}
\examples{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

# Too many titles for categorical data.
selected_titles <- dubliners$doc_id |>
  {\(x) x[grepl("^The |^A |^An ", x)]}() |>
  unique()

dubliners2 <- dubliners |>
  dplyr::filter(doc_id \%in\% selected_titles)

### CATEGORICAL DATA ##

# By default, ggplot2's palette is applied
dubliners2 |>
  visualize()

# change_color() starts with Brewer's "Dark2" palette
dubliners2 |>
  visualize() |>
  change_colors()

# Other color sets and palettes can be chosen
dubliners2 |>
  visualize() |>
  change_colors(colorset = "okabe")

dubliners2 |>
  visualize() |>
  change_colors(colorset = "viridis", palette = "turbo")

dubliners2 |>
  visualize() |>
  change_colors(colorset = "brewer", palette = "Set1")

# Named cases can be highlighted
dubliners2 |>
  visualize(inorder = FALSE) |>
  change_colors(c(
    rep("darkgray", 6),
    "A Painful Case" = "blue"))

### SEQUENTIAL DATA ###

# By default, the "viridis" palette is applied
dubliners2 |>
  visualize(type = "heatmap")

# change_colors()  starts with Brewer's "BuGn" palette
dubliners2 |>
  visualize(type = "heatmap") |>
  change_colors()

# Palettes can be numbered or named
dubliners2 |>
  visualize(type = "heatmap") |>
  change_colors("viridis", palette = 6)

dubliners2 |>
  visualize(type = "heatmap") |>
  change_colors("viridis", palette = "mako")

### N-GRAMS ###
library(ggraph)

dubliners |>
  add_ngrams() |>
  visualize() |>
  change_colors(c("#444488","orange"))
}
\seealso{
Other visualizing helpers: 
\code{\link{plot_bigrams}()},
\code{\link{plot_doc_word_bars}()},
\code{\link{plot_doc_word_heatmap}()},
\code{\link{plot_hapax}()},
\code{\link{plot_htr}()},
\code{\link{plot_tf_idf}()},
\code{\link{plot_topic_bars}()},
\code{\link{plot_topic_distributions}()},
\code{\link{plot_topic_wordcloud}()},
\code{\link{plot_ttr}()},
\code{\link{plot_vocabulary}()},
\code{\link{visualize}()}
}
\concept{visualizing helpers}
