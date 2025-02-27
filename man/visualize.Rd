% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize.R
\name{visualize}
\alias{visualize}
\title{Visualize output}
\usage{
visualize(.data, ...)
}
\arguments{
\item{.data}{data processed with one or more functions from \code{tmtyro}}

\item{...}{
  Arguments passed on to \code{\link[=plot_doc_word_bars]{plot_doc_word_bars}}, \code{\link[=plot_bigrams]{plot_bigrams}}, \code{\link[=plot_vocabulary]{plot_vocabulary}}, \code{\link[=plot_ttr]{plot_ttr}}, \code{\link[=plot_hir]{plot_hir}}, \code{\link[=plot_topic_distributions]{plot_topic_distributions}}, \code{\link[=plot_topic_bars]{plot_topic_bars}}, \code{\link[=plot_topic_wordcloud]{plot_topic_wordcloud}}
  \describe{
    \item{\code{rows}}{The features to show}
    \item{\code{by}}{The column used for document grouping, with doc_id as the default}
    \item{\code{feature}}{The column to measure, as in "word" or "lemma"}
    \item{\code{inorder}}{Whether to retain the factor order of the "by" column}
    \item{\code{reorder_y}}{Whether to reorder the Y-values by facet}
    \item{\code{color_y}}{Whether bars should be filled by Y-values}
    \item{\code{percents}}{Whether to display word frequencies as percentage instead of raw counts}
    \item{\code{label}}{Whether to show the value as a label with each bar}
    \item{\code{label_tweak}}{The numeric value by which to tweak the label, if shown. For percentages, this value adjusts the decimal-point precision. For raw counts, this value adjusts labels' offset from the bars}
    \item{\code{label_inside}}{Whether to show the value as a label inside each bar}
    \item{\code{na_rm}}{Whether to drop empty features}
    \item{\code{random_seed}}{Whether to randomize the creation of the network chart.}
    \item{\code{set_seed}}{A specific seed to use if not random}
    \item{\code{legend}}{Whether to show a legend for the edge color}
    \item{\code{top_n}}{The number of pairs to visualize}
    \item{\code{identity}}{A grouping column for lines}
    \item{\code{descriptive_labels}}{A toggle for disabling descriptive labels of progress_percent on the X-axis}
    \item{\code{labeling}}{Options for labeling groups:
\itemize{
\item \code{"point"} labels the final value
\item \code{"inline"} prints the label within a smoothed curve
\item \code{"axis"} prints labels where a secondary Y-axis might go
\item \code{"inset"} prints a legend within the plot area
\item Anything else prints a legend to the right of the plot area.
}}
    \item{\code{log_y}}{A toggle for logarithmic scaling to the Y-axis; defaults to TRUE}
    \item{\code{topics}}{The topic numbers to view}
  }}
}
\value{
a ggplot2 object
}
\description{
\code{visualize()} provides a simple method for displaying results. Based on previous functions used, \code{visualize()} will choose a method, resolving to one of the other visualizing helpers.
}
\note{
For some visualizations, an optional \code{type} parameter may be helpful to change the visualization. For example, setting \code{type = "htr"}, \code{type = "ttr"}, or \code{type = "hapax"} after \code{\link[=add_vocabulary]{add_vocabulary()}} will emphasize different columns added by that function. Similarly, \code{type = "cloud"} or \code{type = "wordcloud"} will show topic word clouds after \code{\link[=make_topic_model]{make_topic_model()}}, and \code{type = "heatmap"} will show an alternative visualization for word frequencies.
}
\examples{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

# A data frame with `doc_id` and `word` columns will visualize frequency by default
dubliners |>
   visualize()

# Applying `tmtyro` functions will choose an appropriate visualization

dubliners |>
   add_ngrams() |>
   visualize()

dubliners |>
   add_ngrams() |>
   combine_ngrams() |>
   visualize()

dubliners |>
   summarize_tf_idf() |>
   visualize()

dubliners |>
   add_vocabulary() |>
   visualize()

if (FALSE) { # sentiment requires interaction on first load
  dubliners |>
     add_sentiment() |>
     visualize()
}

# Some visualizations are specified with the `type` argument
dubliners |>
   add_vocabulary() |>
   visualize(type = "ttr")

if (FALSE) { # puzzlingly broken for Dubliners, but usually works
dubliners |>
   add_vocabulary() |>
   visualize(type = "hapax")
}

# Other arguments get passed along
dubliners |>
   add_ngrams() |>
   visualize(top_n = 25)

dubliners |>
   add_vocabulary() |>
   visualize(x = progress_percent)
}
\seealso{
Other visualizing helpers: 
\code{\link{change_colors}()},
\code{\link{plot_bigrams}()},
\code{\link{plot_doc_word_bars}()},
\code{\link{plot_doc_word_heatmap}()},
\code{\link{plot_hapax}()},
\code{\link{plot_hir}()},
\code{\link{plot_tf_idf}()},
\code{\link{plot_topic_bars}()},
\code{\link{plot_topic_distributions}()},
\code{\link{plot_topic_wordcloud}()},
\code{\link{plot_ttr}()},
\code{\link{plot_vocabulary}()}
}
\concept{visualizing helpers}
