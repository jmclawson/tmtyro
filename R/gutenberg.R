#' Build and load a corpus from Project Gutenberg
#'
#' `get_gutenberg_corpus()` improves upon the functionality
#' of [gutenbergr::gutenberg_download()] in three key ways.
#' \enumerate{
#' \item Retrieving the ".htm" version of texts
#' instead of the ".zip" version typically used by
#' gutenberger dramatically improves file coverage.
#' \item Parsing HTML headers allows texts to be studied
#' by sections and chapters. Parsing is handled by
#' [parse_html()], with [move_header_to_text()]
#' available for corrections.
#' \item Caching files locally avoids repeated
#' downloads, thereby improving code portability,
#' allowing offline access, and reducing network use.
#' }
#' All changes are made with consideration for server bandwidth,
#' so a two-second delay is introduced between each download
#' attempt. This will slow down the initial acquisition of
#' corpora, but offline caching speeds things up considerably
#' in subsequent use.
#'
#' @param gutenberg_id A vector of ID numbers from Project Gutenberg or a data frame containing a `gutenberg_id` column, such as from the results of a call to [gutenbergr::gutenberg_works()]
#' @param download Whether files should be automatically downloaded into a project subdirectory as needed (the default), always downloaded into the project folder, temporarily downloaded once per-session, or never downloaded
#' @param dir The project subdirectory for storing downloaded `.htm` files
#' @param meta_fields Additional fields to add from [gutenbergr::gutenberg_metadata] describing each book
#' @param html_title Whether to use the h1 header from an HTML file to determine a document's title instead of [gutenbergr::gutenberg_metadata]
#' @param ... Additional parameters passed along to [gutenbergr::gutenberg_strip()]
#'
#' @returns A data frame with one row for each line of the texts in the corpus
#' @export
#
#' @examples
#' \dontrun{
#' library(gutenbergr)
#'
#' dalloway <- gutenberg_works(author == "Woolf, Virginia",
#'                             title == "Mrs Dalloway in Bond Street") |>
#'   get_gutenberg_corpus()
#' }
get_gutenberg_corpus <- function(
    gutenberg_id,
    download = c("auto", "always", "temp", "never"),
    dir = "gutenberg",
    meta_fields = c("gutenberg_id", "title", "author"),
    html_title = FALSE,
    ...) {

  download_opt <- match.arg(download)
  dir <- ifelse(download_opt == "temp", tempdir(), dir)

    if ("data.frame" %in% class(gutenberg_id) &&
      "gutenberg_id" %in% colnames(gutenberg_id)) {
    gutenberg_id <- gutenberg_id[["gutenberg_id"]]
  } else if ("data.frame" %in% class(gutenberg_id)) {
    stop("A `gutenberg_id` column is necessary when using a data frame.")
  }

  ## Platypus TODO: Check the format of `gutenberg_get_mirror()` to match it with the fallback
  # get_safe_mirror <- purrr::possibly(
  #   {\(x) gutenbergr::gutenberg_get_mirror(verbose = FALSE)},
  #   "https://www.gutenberg.org/files"
  # )

  get_safe_mirror <- function() {
    tryCatch(
      gutenbergr::gutenberg_get_mirror(verbose = FALSE),
      error = function(e) {
        "https://www.gutenberg.org/files"
      },
      warning = function(w) {
        "https://www.gutenberg.org/files"
      }
    )
  }

  make_url <- function(x) {
    y <- if (as.numeric(x) > 10) {
      strsplit(x, "") |>
        unlist() |>
        {\(x) x[1:(length(x)-1)]}() |>
        paste0(collapse = "/")
    } else {
      "0"
    }
    stringr::str_c(
      get_safe_mirror(),
      y, x,
      stringr::str_c(x, "-h"),
      stringr::str_c(x, "-h.htm"),
      sep = "/")
  }

  gut_df <-
    data.frame(
      id = as.character(gutenberg_id)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      url = make_url(id)) |>
    dplyr::ungroup()

  # save downloads locally to avoid repeats
  get_book <- function(url) {
    base_id <- stringr::str_extract(
      url,
      "\\d*(?=\\-h\\.htm)")
    the_book <- suppressWarnings(
      download_once(
        url,
        filename = paste0(base_id, ".htm"),
        destdir = dir))
    if (!is.null(the_book)) {
      return(the_book)
    }
    warnings(paste("Could not get", url))
    NULL
  }

  # add a 2-second delay between books
  download_slowly <- purrr::slowly(\(x) get_book(x),
                  rate = purrr::rate_delay(2), quiet = TRUE)

  if (dir.exists(dir) &&
      length(dir(path = dir)) > 0 &&
      download_opt %in% c("auto", "temp")) {
    already_existing <- dir(path = dir, pattern = "htm") |>
      stringr::str_replace_all(
        "(?<=\\d)[.]htm",
        "-h.htm") |>
      paste0(collapse="|")
    some_urls <- gut_df$url |>
      stringr::str_subset(already_existing, negate = TRUE)
    if(length(some_urls) > 0) {
      some_urls |>
        purrr::walk(\(x) download_slowly(x))
    }
  } else if (download_opt != "never") {
    gut_df$url |>
      purrr::walk(\(x) download_slowly(x))
  } else if (download_opt == "never") {
    if (dir.exists(dir) &&
        length(dir(path = dir)) > 0) {
      already_existing <- dir(path = dir, pattern = "htm") |>
        stringr::str_replace_all(
          "(?<=\\d)[.]htm",
          "-h.htm") |>
        paste0(collapse="|")
      some_urls <- gut_df$url |>
        stringr::str_subset(already_existing, negate = TRUE)
      if (length(some_urls) > 1) {
        some_ids <- stringr::str_extract(
          some_urls,
          "\\d*(?=\\-h\\.htm)")
        warning(paste("IDs", stringr::str_flatten_comma(some_ids, last = ", and "), 'have not been downloaded, and `download` is set to "none". Set `download` to "auto" or "always" to get these texts.'))
      } else if (length(some_urls) == 1) {
        some_id <- stringr::str_extract(
          some_urls,
          "\\d*(?=\\-h\\.htm)")
        warning(paste("ID", some_ids, 'has not been downloaded, and `download` is set to "none". Set `download` to "auto" or "always" to get this text.'))
      }
    }
  }

  ids <- sort(gut_df$id)
  collapsed_ids <- paste0("/", ids, ".htm", collapse = "|")
  all_files <- dir(path = dir,
                   pattern = paste0(".htm$"),
                   full.names = TRUE)
  guten_files <- all_files[grepl(collapsed_ids, all_files)]
  if (length(guten_files) < 1) {
    stop(paste("No relevant files exist in", file.path(getwd(), dir)))
  }
  id <- guten_files |>
    stringr::str_remove_all(paste0(dir, "/")) |>
    stringr::str_remove_all(".htm")

  the_books <- guten_files |>
    stats::setNames(id) |>
    purrr::map(\(x) parse_html(x, standardize_headers = TRUE, title = html_title)) |>
    purrr::discard(is.null) |>
    dplyr::bind_rows(.id = "gutenberg_id") |>
    dplyr::relocate(text, .after = tidyr::last_col()) |>
    dplyr::mutate(gutenberg_id = as.integer(gutenberg_id))

  if (length(meta_fields) > 0) {
    if (html_title) {
      meta_fields <- meta_fields[meta_fields != "title"]
    }
    the_books <- the_books[,!colnames(the_books) %in% meta_fields[meta_fields != "gutenberg_id"]]
    meta_fields <- unique(c("gutenberg_id", meta_fields))
    md <- gutenbergr::gutenberg_metadata[meta_fields]
    the_books <- dplyr::right_join(md, by = "gutenberg_id", the_books)
  }

  if (tmtyro_use_log()) {
    id_string <- paste(stringr::str_flatten_comma(gutenberg_id, last = ", and"))
    if (length(gutenberg_id) > 1) {
      id_string <- paste("ID numbers", id_string)
    } else {
      id_string <- paste("ID number", id_string)
    }

    the_books <- the_books |>
      add_logstep(
        fn = "get_gutenberg_corpus",
        arguments = list(parameters = list(gutenberg_id = gutenberg_id)))
  }

  the_books
}


#' Read HTML headers and text from file
#'
#' @param html A file in HTML format
#' @param headers The HTML header levels to consider
#' @param standardize_headers Whether to standardize HTML headers to useful column names
#' @param title Whether to keep H1 tags even when there is only one unique value
#'
#' @returns A data frame with a column called "text" and header columns limited to page elements like h1, h2, and h3, as included in the numeric range of `headers`
#' @export
#'
#' @examples
#' if (FALSE) {
#'   library(dplyr)
#'   library(stringr)
#'   library(tmtyro)
#'
#'   orlando <-
#'     "http://gutenberg.net.au/ebooks02/0200331h.html" |>
#'     download_once() |>
#'     parse_html() |>
#'     filter(str_detect(part, "CHAPTER")) |>
#'     mutate(
#'       chapter = str_extract(part, "\\d"),
#'       author = "Virginia Woolf") |>
#'     select(author, title, chapter, text) |>
#'     drop_na(chapter) |>
#'     identify_by(title, chapter) |>
#'     load_texts()
#' }
#'
parse_html <- function(html, headers = 1:6, standardize_headers = TRUE, title = TRUE){
  user_headers <- paste0("h", headers)

  relevant_elements <- user_headers |>
    c("p") |>
    paste(collapse = ", ")

  found <- html |>
    rvest::read_html() |>
    rvest::html_elements(relevant_elements)

  types <- found |>
    rvest::html_name()

  contents <- found |>
    rvest::html_text2() |>
    stringr::str_replace_all("\n", " ") |>
    stringr::str_replace_all("\r", " ") |>
    stringr::str_replace_all("[ ]+", " ") |>
    trimws()

  out <- data.frame(tag = types, text = contents) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      h1 = dplyr::if_else(tag == "h1", text, NA_character_),
      h2 = dplyr::if_else(tag == "h2", text, NA_character_),
      h3 = dplyr::if_else(tag == "h3", text, NA_character_),
      h4 = dplyr::if_else(tag == "h4", text, NA_character_),
      h5 = dplyr::if_else(tag == "h5", text, NA_character_),
      h6 = dplyr::if_else(tag == "h6", text, NA_character_),
      .before = text
    ) |>
    dplyr::mutate(
      text = dplyr::if_else(tag == "p", text, NA_character_)
    ) |>
    dplyr::select(-tag) |>
    tidyr::fill(h1, h2, h3, h4, h5, h6) |>
    tidyr::drop_na(text) |>
    dplyr::select(dplyr::where(function(x) mean(is.na(x)) < 1))

  present_headers <- intersect(colnames(out), paste0("h", 1:6))

  out <- out |>
    dplyr::select(-setdiff(present_headers, user_headers))

  if (title) {
    out <- out |>
      dplyr::select(
        c(colnames(out)[colnames(out) == "h1"],
          dplyr::where(~dplyr::n_distinct(.) > 1))
      )

    # section_names <- colnames(out)[colnames(out) != "text"]

    # colnames(out)[colnames(out) != "text"][1:ifelse(length(section_names) >= 4, 4, length(section_names))] <- c("title","part", "section", "subsection")[1:length(section_names)]

  } else {
    out <- out |>
      dplyr::select(
        dplyr::where(~dplyr::n_distinct(.) > 1)
      )

    # section_names <- colnames(out)[colnames(out) != "text"]

    # colnames(out)[colnames(out) != "text"][1:ifelse(length(section_names) >= 3, 3, length(section_names))] <- c("part", "section", "subsection")[1:length(section_names)]
  }

  if (standardize_headers) {
    out <- out |>
      standardize_headers(title = title)
  }

  out
}

#' Standardize column names from HTML
#'
#' needed tests: unnamed arguments, values repurposed from defaults, mix of named and unnamed arguments, title = FALSE/TRUE
#'
#' @param data A tidy data frame, potentially containing header columns "h1" through "h6"
#' @param ... Optionally, a named list of columns and values to which they should be renamed, with defaults as faullback
#' @param title Whether any "h1" column should be renamed to "title"
#'
#' @returns A data frame with column names adjusted
#' @export
#'
#' @examples
#' if (FALSE) {
#'   joyce2 <- joyce |>
#'     standardize_titles() |>
#'     move_column_to_text(subsection, title == "Ulysses")
#' }
#'
standardize_headers <- function(data, ..., title = TRUE) {
  present_headers <- colnames(data)[colnames(data) %in% paste0("h", 1:6)]

  if (length(present_headers) == 0) {
    return(data)
  }

  default_set <- c("part", "section", "subsection", "subsubsection")

  user_set <- list(...) |> unlist()

  if (is.null(names(user_set))) {
    unnamed_user <- seq_along(user_set)
  } else {
    unnamed_user <- which(names(user_set) == "")
  }

  if (length(unnamed_user) > 0) {
    available_headers <- present_headers[!present_headers %in% names(user_set)]
    if (title) {
      available_headers <- setdiff(available_headers, "h1")
    }
    names(user_set)[unnamed_user] <- available_headers[seq_along(unnamed_user)]
  }

  not_title <- setdiff(present_headers, names(user_set))

  if (title) {
    not_title <- setdiff(not_title, "h1")
  }

  default_set <- default_set |>
    {\(x) x[!x %in% user_set]}()

  names(default_set)[1:min(4, length(not_title))] <- not_title

  default_set <- default_set[1:length(not_title)]

  if (title && "h1" %in% present_headers) {
    default_set <- c(h1 = "title", default_set)
  }

  default_subset <- default_set[!names(default_set) %in% names(user_set)] |>
    {\(x) x[!x %in% user_set]}()
  the_set <- c(user_set, default_subset) |>
    {\(x) x[names(x) != ""]}()
  names(the_set) <-
    paste0("\\b", names(the_set), "\\b")

  out <- data

  colnames(out) <- colnames(data) |>
    stringr::str_replace_all(the_set)

  out
}

#' Move a header column to text
#'
#' In some texts, header tags of a particular level indicate typographical variance that shouldn't be confused with other section tags. `move_header_to_text()` provides a simple method to adjust the table.
#'
#' @param data A data frame with a column called `text` and at least one other column indicating parts, chapters, or sections.
#' @param column The header column to move
#' @param ... (optional) Filtering condition, such as `title == "Ulysses"`.
#'
#' @returns A data frame with the header column moved into `text`, conditional on `...`
#' @export
#'
#' @examples
#' if (FALSE) {
#'   joyce2 <- joyce |>
#'     move_column_to_text(subsection, title == "Ulysses")
#' }
#'
move_header_to_text <- function(data, column, ...){
  relevant_cols <- c(deparse(substitute(column)), "text")
  if (!missing(..1)) {
    data <- data |>
      dplyr::mutate(
        .test1 = ...,
        .test2 = duplicated({{ column }}))
    colnames(data)[ncol(data) - 1] <- ".test1"
  } else {
    data <- data |>
      dplyr::mutate(
        .test1 = TRUE,
        .test2 = duplicated({{ column }}))
  }

  data |>
    dplyr::mutate(
      {{ column }} := dplyr::case_when(
        .test1 & .test2 ~ NA_character_,
        TRUE ~ {{ column }})) |>
    tidyr::pivot_longer(cols = tidyr::all_of(relevant_cols)) |>
    dplyr::mutate(name = dplyr::case_when(
      .test1 ~ "text",
      TRUE ~ name)) |>
    tidyr::drop_na(value) |>
    dplyr::select(-c(.test1, .test2)) |>
    tidyr::pivot_wider(values_fn = list) |>
    dplyr::select(dplyr::where(function(x) mean(is.na(x)) < 1)) |>
    tidyr::unchop(text)
}
