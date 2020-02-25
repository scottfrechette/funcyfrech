#' Generate summary statistics on data
#'
#' @name standardize
#' @param x A numeric vector
#' @export

standardize <- function(x) {

  (x - mean(x)) / sd(x)

}

#' Generate summary statistics on data
#'
#' @name normalize
#' @param x A numeric vector
#' @export

normalize <- function(x) {

  (x - min(x)) / (max(x) - min(x))

}
