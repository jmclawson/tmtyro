% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_once.R
\name{download_once}
\alias{download_once}
\title{Download a file once}
\usage{
download_once(url, filename = NULL, destdir = "data")
}
\arguments{
\item{url}{The URL of an online document.}

\item{filename}{The file name to be saved locally. In many cases this parameter isn't necessary, since the file name can automatically be parsed from the URL, but some web addresses will obscure it.}

\item{destdir}{The destination directory to save the file. By default, this is the "data/" folder, which will be created if it doesn't yet exist.}
}
\value{
Path to the local file, returned invisibly
}
\description{
\code{download_once()} checks for a local copy of a file found online. If a copy doesn't exist, it downloads a copy for local use.
}
\examples{
\dontrun{
download_once("example.com/sample.csv")
}
}
