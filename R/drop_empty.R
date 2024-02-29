#' Drop rows containing missing values
#'
#' `drop_empty()` drops rows where any column specified by `...` contains a missing value. Internally, the function repurposes [tidyr::drop_na()].
#'
#' @param data A data frame
#' @param ... Columns to inspect for missing values. If empty, all columns are used.
#'
#' @export
#' @aliases tidyr::drop_na
drop_empty <- function(data, ...){
  tidyr::drop_na(data, ...)
}
