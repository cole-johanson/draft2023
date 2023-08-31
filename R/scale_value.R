#' @export
scale_value <- function(x) {
  (x - min(x)) / (max(x) - min(x)) * 100
}
