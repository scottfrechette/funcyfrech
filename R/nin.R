#' Find Matching (or Non-Matching) Elements
#'
#' @name nin
#' @description \code{\%nin\%} is a binary operator, which returns a logical vector indicating
#' if there is a match or not for its left operand. A true vector element
#' indicates no match in left operand, false indicates a match.
#' @param x a vector (numeric, character, factor)
#' @param table a vector (numeric, character, factor), matching the mode of \code{x}
#'
#' @export

"%nin%" <- function(x, table) match(x, table, nomatch = 0) == 0
