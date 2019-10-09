#' Select top (or bottom) n rows for each group (by value)
#'
#' @name top_n_group
#' @param x A tibble to filter
#' @param n number of rows to return for each group
#' @param wt (Optional). The variable to use for ordering. If not specified, defaults to the last variable in the tbl.
#' @param ... Optional columns to group by
#' @import dplyr
#' @export

top_n_group <- function(x, n, wt, ...) {

  x %>%
    group_by(...) %>%
    top_n({{n}}, {{wt}}) %>%
    ungroup()

}
