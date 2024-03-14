#' Build and load a corpus from Project Gutenberg
#'
#' `get_gutenberg_corpus()` improves upon the functionality
#' of [gutenbergr::gutenberg_download] in two key ways. First,
#' it downloads the ".txt" format of ebooks rather than the
#' ".zip" format. This change improves coverage dramatically.
#' This change was made with consideration for server bandwidth,
#' so a two-second delay is introduced between each download
#' attempt. Second, it caches a copy of the file locally to avoid
#' repeated downloads. This change helps with code portability and
#' offline access.
#'
#' @param gutenberg_id A vector of ID numbers from Project Gutenberg or a data frame containing a `gutenberg_id` column, such as from the results of a call to [gutenbergr::gutenberg_works()].
#' @param dir The directory for storing downloaded `.txt` files. Default value is "gutenberg".
#' @param mirror Optionally a mirror URL to retrieve the books from. By default uses the mirror from [gutenbergr::gutenberg_get_mirror()].
#' @param meta_fields Additional fields to add from [gutenbergr::gutenberg_metadata] describing each book. By default, title and author are added.
#' @param ... Additional parameters passed along to [gutenbergr::gutenberg_download()] in the final step.
#'
#' @returns A data frame with one row for each line of the texts in the corpus.
#' @export
#'
#' @examples
#' library(gutenbergr)
#'
#' dalloway <- gutenberg_works(author == "Woolf, Virginia",
#'                             title == "Mrs. Dalloway") |>
#'   get_gutenberg_corpus()
get_gutenberg_corpus <- function(gutenberg_id, dir = "gutenberg",
                                 mirror = NULL,
                                 meta_fields = c("title", "author"),
                                 ...) {

  # adapted from gutenberg_download
  if (is.null(mirror)) {
    mirror <- gutenbergr::gutenberg_get_mirror(verbose = FALSE)
  }

  if (inherits(gutenberg_id, "data.frame")) {
    gutenberg_id <- gutenberg_id[["gutenberg_id"]]
  }

  id <- as.character(gutenberg_id)

  path <- id |>
    stringr::str_sub(1, -2) |>
    stringr::str_split("") |>
    purrr::map_chr(stringr::str_c, collapse = "/")

  path <- ifelse(nchar(id) == 1, "0", path)

  # modifications away from gutenbergr::gutenberg_download

  # .txt files don't seem to be against policy
  full_url <- stringr::str_c(mirror, path, id,
                             stringr::str_c(id, ".txt"),
                             sep = "/")
  names(full_url) <- id

  # save downloads locally to avoid repeats
  try_download <- function(url) {
    ret <- suppressWarnings(download_once(url, destdir=dir))
    if (!is.null(ret)) {
      return(ret)
    }
    base_url <- stringr::str_replace(url, ".txt$", "")
    base_id <- stringr::str_extract(base_url, "[0-9]*$")
    for (suffix in c("-8", "-0")) {
      Sys.sleep(2)
      new_url <- glue::glue("{base_url}{suffix}.txt")
      ret <- suppressWarnings(download_once(new_url,
                                            filename = paste0(base_id, ".txt"),
                                            destdir = dir))
      if (!is.null(ret)) {
        return(ret)
      }
    }
    cli::cli_warn(
      c(
        "!" = "Could not download a book at {url}.",
        "i" = "The book may have been archived.",
        "i" = "Alternatively, You may need to select a different mirror.",
        ">" = "See https://www.gutenberg.org/MIRRORS.ALL for options."
      )
    )
    NULL
  }

  # add a 2-second delay between books
  download_slowly <- purrr::slowly(\(x) try_download(x), rate = purrr::rate_delay(2), quiet = TRUE)

  # extra checks to avoid attempting redownload for ids with suffixes
  if (dir.exists(dir)) {
    if (length(dir(path = dir)) > 0) {
      already_existing <- dir(path = dir) |>
        stringr::str_remove_all("[.]txt") |>
        stringr::str_remove_all("-8") |>
        stringr::str_remove_all("-0") |>
        paste0(".txt") |>
        unique() |>
        paste0(collapse="|")
      some_urls <- full_url[!grepl(already_existing, full_url)]
      message(some_urls)
      if(length(some_urls) > 0) {
        some_urls |>
          purrr::walk(download_slowly)
      }
    } else {
      full_url |>
        purrr::walk(download_slowly)
    }
  } else {
    full_url |>
      purrr::walk(download_slowly)
  }

  # send filenames to gutenberg_download
  ids <- sort(id)
  collapsed_ids <- paste0(ids, collapse = "|")
  guten_files <- dir(path = dir, full.names = TRUE) |>
    {\(x) x[grepl(collapsed_ids, x)]}()

  gutenbergr::gutenberg_download(ids, files = guten_files, meta_fields = meta_fields, ...)
}
