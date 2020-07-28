#' Convert decimal to various options
#'
#' @name convert_decimal
#' @param x Decimal to convert
#' @param output Desired output
#' @param top Number of results to return
#' @import dplyr
#' @export

convert_decimal <- function(x,
                            output = c("all", "ratio", "percent",
                                       "decimal", "fraction", "odds"),
                            top = 1) {

  output <- output[[1]]

  df <- ratios %>%
    arrange(abs(decimal - x)) %>%
    head(top) %>%
    select(decimal, percent, fraction, ratio, odds)

  if (output == "all") {

    return(df)

  } else {

    return(df[[output]])

  }

}

library(dplyr)

ratios <- tidyr::crossing(num = 1:100,
                          denom = 1:1000) %>%
  mutate(decimal = num / denom,
         percent = paste0(round(num / denom, 2) * 100, "%"),
         fraction = paste0(num, "/", denom),
         ratio = paste(num, "in", denom),
         odds = paste0(denom - num, ":", num)) %>%
  group_by(percent) %>%
  filter(denom == min(denom)) %>%
  ungroup() %>%
  filter(decimal < 1) %>%
  mutate(keep = denom <= 12 | num == 1)
