#' Create dot or lollipop chart
#'
#' @name dot_chart
#' @param df Tibble
#' @param key Feature to include on y-axis
#' @param n Count to include on x-axis
#' @param top How many keys to include
#' @param facet (Optional) Feature to facet by
#' @param lollipop Create add line to create lollipop chart
#' @export

dot_chart <- function(df, key, n,
                      top = 25,
                      facet = NULL,
                      lollipop = FALSE) {

  y <- enquo(key)
  x <- enquo(n)
  f <- enquo(facet)

  if(is.null(facet)) {

    p <- df %>%
      top_n(top, !!x) %>%
      arrange(desc(eval(x))) %>%
      mutate(!!y := fct_inorder(!!y) %>%
               fct_rev) %>%
      ggplot(aes(!!x, !!y)) +
      geom_point(color = "#004F71") +
      scale_x_continuous(expand = c(0.01, 1)) #+
    # theme(panel.grid.major.y = element_blank())

  } else {

    p <- df %>%
      top_n_group(top, !!x, !!f) %>%
      arrange(desc(eval(x))) %>%
      mutate(!!y := reorder_within(!!y, !!x, !!f)) %>%
      ggplot(aes(!!x, !!y)) +
      geom_point(color = "#004F71") +
      scale_x_continuous(expand = c(0.01, 1)) +
      facet_wrap(vars(!!f), scales = "free_y") +
      scale_y_reordered() #+
    # theme(panel.grid.major.y = element_blank())

  }


  if(lollipop) {

    p <- p +
      geom_segment(aes(xend = 0, yend = !!y),
                   color = "#808080")

  }

  return(p)

}
