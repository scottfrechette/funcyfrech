#' Generate summary statistics on data
#'
#' @name fsummary
#' @param df A tibble to summarize
#' @param n Column to summarize
#' @param ... Optional columns to group by
#' @param sort if TRUE will sort output in descending order
#' @param sort_by Column to sort by
#' @import dplyr
#' @export

fsummary <- function(df, n, ...,
                     sort = T, sort_by = p50) {

  grouping <- quos(...)
  n_quo = enquo(n)

  df_summary <- df %>%
    group_by(!!! grouping) %>%
    summarise(count = n(),
              avg = mean(!! n_quo, na.rm = T),
              sd = sd(!! n_quo, na.rm = T),
              p0 = min(!! n_quo, na.rm = T),
              p25 = quantile(!! n_quo, 0.25, na.rm = T),
              p50 = median(!! n_quo, na.rm = T),
              p75 = quantile(!! n_quo, 0.75, na.rm = T),
              p100 = max(!! n_quo, na.rm = T)) %>%
    ungroup()

  if(sort) {

    df_summary <- df_summary %>%
      arrange(-!! enquo(sort_by))

  }

  df_summary

}

