#' Replace with previous
#'
#' If the first value in the vector is NA, set it to 0
#' If any other value is NA, set it to the previous value.
#'
#' Then return the same vector.
#'
#' @export
replace_with_prev <- function(x) {
  if(is.na(x[1])) x[1] = 0
  dplyr::if_else(is.na(x), dplyr::lag(x), x)
}
