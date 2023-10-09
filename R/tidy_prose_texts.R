tidy_prose_texts <- function(folder = "data",
                             name = ".txt",
                             word = TRUE,
                             paragraph = TRUE) {
  tidy_one_text <- function(file, directory) {
    tibble::tibble(document = stringr::str_remove_all(file, "[.].*"),
                   text = readLines(paste0(directory, "/", file))) |>
      dplyr::mutate(par_num = cumsum(text == "") + 1,
                    .after = document) |>
      dplyr::filter(text != "")
  }

  the_files <-
    list.files(path = paste0(folder, "/"),
               pattern = name)

  full_prose <-
    do.call(rbind,
            lapply(the_files,
                   tidy_one_text,
                   directory = folder))

  if (word) {
    full_prose <- full_prose |>
      tidytext::unnest_tokens(word, text)
  }


  if (!paragraph) {
    full_prose <- full_prose |>
      dplyr::select(-par_num)
  }

  full_prose
}
