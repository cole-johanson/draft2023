#' @export
replace_with_prev <- function(x) {
  if(is.na(x[1])) x[1] = 0
  dplyr::if_else(is.na(x), lag(x), x)
}
