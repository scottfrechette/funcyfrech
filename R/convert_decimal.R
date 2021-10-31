#' Convert decimal to various options
#'
#' @name convert_decimal
#' @param x Decimal to convert
#' @param output Desired output
#' @param top Number of results to return
#' @export

convert_decimal <- function(x,
                            output = c("all", "ratio", "percent",
                                       "decimal", "fraction", "odds"),
                            top = 1) {

  output <- output[[1]]

  df <- ratios %>%
    dplyr::arrange(abs(decimal - x)) %>%
    dplyr::head(top) %>%
    dplyr::select(decimal, percent, fraction, ratio, odds)

  if (output == "all") {

    return(df)

  } else {

    return(df[[output]])

  }

}

ratios <- tidyr::crossing(num = 1:100,
                          denom = 1:1000) %>%
  dplyr::filter(num < denom) %>%
  dplyr::mutate(decimal = num / denom,
                percent = paste0(round(num / denom, 2) * 100, "%"),
                fraction = paste0(num, "/", denom),
                ratio = paste(num, "in", denom),
                odds = paste0(denom - num, ":", num),
                keep = denom <= 12 | num == 1 | denom %% 100 == 0) %>%
  dplyr::group_by(decimal) %>%
  dplyr::filter(denom == min(denom)) %>%
  dplyr::ungroup()
