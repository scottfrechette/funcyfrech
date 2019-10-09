#' Filter based on selected text
#'
#' @name str_filter
#' @param .data A tibble, or an object that can be coerced into a tibble.
#' @param string Input vector. Either a character vector, or something coercible to one.
#' @param pattern Pattern to look for.
#' @import rlang
#' @export

str_filter <- function(.data, string, pattern) {

  string_quo <- enquo(string)

  dplyr::filter(.data, stringr::str_detect(!!string_quo, pattern))

}
