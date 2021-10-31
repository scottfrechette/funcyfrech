#' Standardize data to z-score
#'
#' @name standardize
#' @param x A numeric vector
#' @export

standardize <- function(x) {

  (x - mean(x)) / sd(x)

}

#' Normalize data to 0-1 scale
#'
#' @name normalize
#' @param x A numeric vector
#' @export

normalize <- function(x) {

  (x - min(x)) / (max(x) - min(x))

}
