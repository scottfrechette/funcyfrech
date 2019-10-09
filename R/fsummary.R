#' Generate summary statistics on data
#'
#' @name fsummary
#' @param df A tibble to summarize
#' @param n Column to summarize
#' @param ... Optional columns to group by
#' @param sort if TRUE will sort output in descending order of n
#' @import dplyr
#' @export

fsummary <- function(df, n, ..., sort = T) {

  grouping <- quos(...)

  df_summary <- df %>%
    group_by(!!! grouping) %>%
    summarise(avg = mean(n, na.rm = T),
              sd = sd(n, na.rm = T),
              p0 = min(n, na.rm = T),
              p25 = quantile(n, 0.25, na.rm = T),
              p50 = median(n, na.rm = T),
              p75 = quantile(n, 0.75, na.rm = T),
              p100 = max(n, na.rm = T))

  if(sort) {

    df_summary <- df_summary %>%
      arrange(-p50)

  }

  df_summary

}

