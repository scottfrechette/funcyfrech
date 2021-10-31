#' Trend chart
#'
#' Create trend chart with points and smoothed line
#'
#' @param tbl A tidy dataset with one count per date per group
#' @param .date Date column
#' @param .n Count or Percent column
#' @param ... additional data to pass to ggplot
#' @param percent Whether to use percent labels on y-axis
#' @param alpha geom_point alpha
#' @param se geom_smooth error bars
#' @param x_lab label of x-axis
#' @param y_lab label of y-axis
#' @param title title of plot
#' @param facet group for faceting
#' @param scales scales passed to facet_wrap
#' @export

geom_trend <- function(tbl,
                       .date,
                       .n,
                       ...,
                       percent = FALSE,
                       alpha = 0.2,
                       se = FALSE,
                       x_lab = NULL,
                       y_lab = "Count",
                       title = NULL,
                       facet = NULL,
                       scales = "free_y") {

  p <- tbl %>%
    ggplot(aes({{.date}}, {{.n}},
               ...)) +
    geom_point(alpha = alpha) +
    geom_smooth(se = se) +
    expand_limits(y = 0) +
    labs(x = x_lab,
         y = y_lab,
         title = title)

  if(percent) {

    p <- p + scale_y_continuous(labels = scales::percent)

  }

  if(!is.null(facet)) {

    p <- p + facet_wrap(~ {{facet}}, scales = scales)

  }

  suppressMessages(print(p))

}
