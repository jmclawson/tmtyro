#' Drop rows containing missing values
#'
#' Leave `...` blank to drop rows containing any missing values, or specify column names to filter specific variables. `drop_empty()` repurposes [tidyr::drop_na()].
#'
#' @param data A data frame
#' @param ... Columns to inspect for missing values. If empty, all columns are used.
#'
#' @export
#' @aliases tidyr::drop_na
drop_empty <- function(data, ...){
  tidyr::drop_na(data, ...)
}

#' @rdname drop_empty
#' @export
drop_na <- function(data, ...){
  tidyr::drop_na(data, ...)
}
