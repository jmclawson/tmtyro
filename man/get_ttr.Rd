% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/skills_ramp.R
\name{get_ttr}
\alias{get_ttr}
\title{Cumulative type-token ratio}
\usage{
get_ttr(x)
}
\arguments{
\item{x}{A vector, such as a column of character strings}
}
\value{
A vector of ratios
}
\description{
TTR reports the ratio of unique word types to the total size of a text.
}
\examples{
c("cat", "dog", "dog", "bat", "dog") |>
  get_ttr()
}
\seealso{
Other vectorized functions: 
\code{\link{get_cumulative_vocabulary}()},
\code{\link{get_frequency}()},
\code{\link{get_hir}()},
\code{\link{get_htr}()},
\code{\link{get_idf_by}()},
\code{\link{get_match}()},
\code{\link{get_sentiment}()},
\code{\link{get_tf_by}()},
\code{\link{get_tfidf_by}()},
\code{\link{is_hapax}()},
\code{\link{is_new}()}
}
\concept{vectorized functions}
