#' Add proportion to counts
#'
#' @name add_prop
#' @param df a tibble with counts
#' @param n column of counts
#' @param ... variables to group by
#' @param sort if TRUE will sort output in descending order of n
#' @param pct if TRUE will add percent column
#' @param round if TRUE will round to nearest percent
#' @param denom denominator for calculating percent. If sum then will use sum of n otherwise will use value entered
#' @param pct_formatting should percent formatting be applied
#' @param cum_sum if TRUE will add column for cumulative sum
#' @param cum_prop if TRUE will add column for cumulative percent
#' @param head returns all values by default
#' @import dplyr
#' @export

add_prop <- function(df,
                     n,
                     ...,
                     sort = TRUE,
                     round = FALSE,
                     denom = "sum",
                     pct_formatting = FALSE,
                     cum_prop = FALSE) {

  col_quos <- quos(...)

  if(denom == "sum") {

    df <- df %>%
      group_by(!!! col_quos) %>%
      mutate(prop = n / sum(n))


  } else {

    df <- df %>%
      group_by(!!! col_quos) %>%
      mutate(prop = n / denom)

  }

  if(round) {

    df <- df %>%
      mutate(prop = round(prop, 2))

  }

  if(cum_prop) {

    df <- df %>%
      mutate(cum_prop = cumsum(prop))

  }

  if(pct_formatting) {

    df <- df %>%
      mutate(percent = scales::percent(prop, accuracy = 1)) %>%
      select(-prop)

    if(cum_prop) {

      df <- df %>%
        mutate(cum_prop = scales::percent(cum_prop))

    }

  }

  df %>%
    ungroup()

}
