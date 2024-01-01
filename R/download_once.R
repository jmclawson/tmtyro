#' Download a file once
#'
#' `download_once()` checks for a local copy of a file found online. If a copy doesn't exist, it downloads a copy for local use.
#'
#' @param url The URL of an online document.
#' @param filename The file name to be saved locally. In many cases this parameter isn't necessary, since the file name can automatically be parsed from the URL, but some web addresses will obscure it.
#' @param destdir The destination directory to save the file. By default, this is the "data/" folder, which will be created if it doesn't yet exist.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' download_once("example.com/sample.csv")
#' }
download_once <- function(
  url,
  filename = NULL,
  destdir = "data"
) {

  if(is.null(filename)){
    the_filename <- url |> stringr::str_extract("[a-z A-Z 0-9 \\- _]+[.]{1,1}+[a-zA-Z]{1,4}$")
  } else {
    the_filename <- filename
  }

  if(!dir.exists(destdir)){
    dir.create(destdir)
  }

  filepath <- file.path(destdir, the_filename)

  if(!file.exists(filepath)) {
    utils::download.file(url, destfile = filepath)
  }
}
