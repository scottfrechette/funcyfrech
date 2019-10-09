#' Average observations by group
#'
#' @name bully
#' @param df a tibble to count
#' @param col column to average
#' @param ... variables to group by
#' @param name The output column name. If omitted, it will be mean.
#' @param na.rm Whether to remove NA values
#' @param sort if TRUE will sort in descending order of mean
#' @import dplyr
#' @export

bully <- function(df, col, ...,
                  name = "mean",
                  na.rm = TRUE, sort = TRUE) {

  col_quo <- enquo(col)
  group_quos <- quos(...)

  df <- df %>%
    group_by(!!! group_quos) %>%
    summarize(!!name := mean(!!col_quo, na.rm = na.rm)) %>%
    ungroup()

  if(sort) {

    df <- df %>%
      arrange(desc(!!sym(name)))

  }

  return(df)

}
