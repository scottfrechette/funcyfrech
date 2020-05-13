#' Generate counts on data
#'
#' @name fcount
#' @param df a tibble to count
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

fcount <- function(df, ..., sort = TRUE, pct = TRUE,
                   round = FALSE, denom = "sum", pct_formatting = TRUE,
                   cum_sum = FALSE, cum_prop = FALSE, head = NULL) {

  col_quos <- quos(...)

  df <- df %>%
    count(!!! col_quos, sort = sort)

  if(pct) {

    if(denom == "sum") {

      df <- df %>%
        mutate(prop = n / sum(n))



    } else {

      df <- df %>%
        mutate(prop = n / denom)

    }

    if(round) {

      df <- df %>%
        mutate(prop = round(prop, 2))

    }

  }

  if(cum_sum) {

    df <- df %>%
      mutate(cum_sum = cumsum(n))

  }

  if(cum_prop) {

    df <- df %>%
      mutate(cum_prop = cumsum(prop))

  }

  if(pct_formatting) {

    df <- df %>%
      mutate(percent = scales::percent(prop)) %>%
      select(-prop)

    if(cum_prop) {

      df <- df %>%
        mutate(cum_prop = scales::percent(cum_prop)) %>%
        select(-cum_prop)

    }

  }

  if(!is.null(head)) {

    df <- head(df, 20)

  }

  df

}
