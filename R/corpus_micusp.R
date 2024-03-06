#' Get MICUSP metadata
#'
#' Explore metadata of available MICUSP (Michigan Corpus of Upper-level Student Papers) texts to choose a corpus. On first use, the function creates a folder in the working directory and downloads the "micusp_metadata.csv" file before opening it. Subsequent use of the function will load from the local copy.
#'
#' @param micusp_dir The name of the directory for storing local copies of MICUSP materials. Defaults to "micusp/".
#'
#' @returns A data frame with 1 row for each document in the corpus and 8 columns of metadata: `paper_id`, `title`, `discipline`, `paper_type`, `student_level`, `sex`, `nativeness`, and `textual_features`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' micusp_metadata() |> head()
#' }
micusp_metadata <- function(micusp_dir = "micusp"){
  download_once("https://elicorpora.info/browse?mode=download&start=1&sort=dept&direction=desc",
                filename = "micusp_metadata.csv",
                destdir = micusp_dir)

  readr::read_csv("micusp/micusp_metadata.csv",
                  show_col_types = FALSE) |>
    janitor::clean_names()
}

parse_micusp_paper <- function(paperid,
                               htmldir = "micusp/corpus_html",
                               textdir = "micusp/corpus"){
  filename_text <- paperid |>
    stringr::str_replace_all("[.]","_") |>
    paste0(".txt") |>
    {\(x) paste0(textdir,"/",x)}()

  filename_html <- paperid |>
    stringr::str_replace_all("[.]","_") |>
    paste0(".html") |>
    {\(x) paste0(htmldir,"/",x)}()

  if(!dir.exists(textdir)){dir.create(textdir)}
  if(!file.exists(filename_text)){
    filename_html |>
      rvest::read_html() |>
      rvest::html_element(css = "div#paperBody") |>
      rvest::html_text() |>
      readr::write_lines(filename_text)
  }

  readr::read_lines(filename_text) |>
    paste0(collapse = "\n")
}

#' Get a MICUSP corpus
#'
#' The function accepts filters on columns from [micusp_metadata()] and downloads and parses MICUSP (Michigan Corpus of Upper-level Student Papers) texts locally if copies don't yet exist. It returns a table combining metadata and text data for further processing.
#'
#' @param ... A filter on rows and columns from [micusp_metadata()]. Accepted columns include the following: `paper_id`, `title`, `discipline`, `paper_type`, `student_level`, `sex`, `nativeness`, and `textual_features`.
#'
#' @returns A data frame with 1 row for each document in the corpus and 9 columns. The first 8 columns contain metadata, and the final column called `text` contains the full text of each document.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' physics_f <- get_micusp_corpus(discipline == "Physics", sex == "Female")
#' physics_m <- get_micusp_corpus(discipline == "Physics", sex == "Male")
#'
#' discipline_by_sex <-
#'   micusp_metadata() |>
#'   count(discipline, sex) |>
#'   tidyr::pivot_wider(
#'     names_from = "sex",
#'     values_from = "n") |>
#'   dplyr::mutate(
#'     ratio_f = (Female) / (Male + Female)) |>
#'     dplyr::arrange(ratio_f)
#'
#'  disciplines_low_f <-
#'   discipline_by_sex |>
#'   head(3) |>
#'   dplyr::pull(discipline)
#'
#'  disciplines_low_m <-
#'   discipline_by_sex |>
#'   tail(3) |>
#'   dplyr::pull(discipline)
#'
#' low_representation_f <- get_micusp_corpus(
#'   sex == "Female",
#'   discipline %in% disciplines_low_f)
#'
#'  low_representation_m <- get_micusp_corpus(
#'   sex == "Male",
#'   discipline %in% disciplines_low_m)
#' }
get_micusp_corpus <- function(...){
  the_df <-
    micusp_metadata() |>
    dplyr::filter(...)

  the_urls <-
    the_df |>
    dplyr::pull(paper_id) |>
    {\(x) paste0("https://elicorpora.info/view?pid=", x)}()

  the_filenames <-
    the_df |>
    dplyr::pull(paper_id) |>
    stringr::str_replace_all("[.]", "_") |>
    paste0(".html")

  the_urls |>
    purrr::walk2(.x = the_urls,
                 .y = the_filenames,
                 .f = ~ download_once(.x, .y, destdir = "micusp/corpus_html"))

  the_df |>
    dplyr::rowwise() |>
    dplyr::mutate(text = parse_micusp_paper(paper_id))
}
