#' Split a column into tokens without names
#'
#' Split a column into tokens, flattening the table into one-token-per-row while omitting any token that is present only in a capitalized form.
#'
#' @param df A data frame
#' @param output Output column to be created.
#' @param input Input column that gets split by word.
#' @param to_lower Whether to convert final words to lowercase.
#'
#' @returns A data frame
#' @export
#'
#' @examples
#' \dontrun{
#' mysteries <-
#'   load_texts("mystery-novels",
#'              to_lower = FALSE) |>
#'   unnest_without_caps()
#'
#' # Since `unnest_without_caps()` is
#' # incorporated into `load_texts()`,
#' # it may be unnecessary for many
#' # scenarios.
#' mysteries <-
#'   load_texts("mystery-novels",
#'              remove_names = TRUE)
#'   }
unnest_without_caps <- function(
    df,
    output = "word",
    input = "text",
    to_lower = TRUE) {

  full <- df |>
    tidytext::unnest_tokens({{output}},
                            {{input}},
                            to_lower = FALSE)

  big <- full |>
    dplyr::filter(stringr::str_detect({{output}},
                                      "^[A-Z]")) |>
    dplyr::pull({{output}}) |>
    unique()

  small <- full |>
    dplyr::filter(stringr::str_detect({{output}},
                                      "^[a-z]")) |>
    dplyr::pull({{output}}) |>
    unique()

  only_caps <- base::setdiff(tolower(big), small)

  df |>
    tidytext::unnest_tokens({{output}}, {{input}}, to_lower = to_lower) |>
    dplyr::filter(!tolower({{output}}) %in% tolower(only_caps))
}
