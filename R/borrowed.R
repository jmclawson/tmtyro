#' @importFrom tidyr drop_na
#' @inherit tidyr::drop_na
#' @export
#' @examples
#' data.frame(
#'   A = 1:3,
#'   B = c("red", NA, "green"),
#'   C = c(TRUE, TRUE, TRUE)) |>
#' drop_na()
#'
drop_na <- function(data, ...){
  tidyr::drop_na(data, ...)
}

#' #' Count values in one or more columns
#' #'
#' #' @param x A data frame
#' #' @param ... The columns to count. If more than one column is chosen, combinations of values are counted.
#' #'
#' #' @returns A data frame with one column for every grouping identified by `...` and an additional column for the number of values counted
#' #' @export
#' #'
#' #' @examples
#' #' mtcars |>
#' #'   count(cyl)
#' count <- function(x, ...){
#'   dplyr::count(x, ...) |>
#'     add_class("count")
#' }
#'
