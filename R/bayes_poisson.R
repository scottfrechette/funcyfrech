#' Calculate gamma-poisson posterior
#'
#' @param prior_shape Shape set by prior
#' @param prior_rate Rate set by prior
#' @param data_count Count of collected data
#' @param data_size Size of data
#' @param plot Choose to list posterior parameters or plot output
#' @export

estimate_counts <- function(prior_shape,
                            prior_rate,
                            data_count,
                            data_size,
                            plot = F) {

  post_shape <- prior_shape + data_count
  post_rate <- prior_rate + data_size

  if(plot) {

    lower = min(qgamma(0.05, prior_shape, prior_rate),
                qgamma(0.05, data_count, data_size),
                qgamma(0.05, post_shape, post_rate))

    upper = max(qgamma(0.95, prior_shape, prior_rate),
                qgamma(0.95, data_count, data_size),
                qgamma(0.95, post_shape, post_rate))

    ggplot(data = tibble(x = c(lower, upper)), aes(x)) +
      stat_function(fun = dgamma, n = 101,
                    args = list(shape = prior_shape, rate = prior_rate),
                    linetype = 2, color = "blue") +
      stat_function(fun = dgamma, n = 101,
                    args = list(shape = data_count, rate = data_size),
                    linetype = 2, color = "red") +
      stat_function(fun = dgamma, n = 101,
                    args = list(shape = post_shape, rate = post_rate)) +
      theme_minimal()

  } else {

    list(shape = post_shape, rate = post_rate)

  }

}

#' Calculate chance a > b based on closed gamma-poisson form
#'
#' @param shape1 Shape of 1st vector
#' @param rate1 Rate of 1st vector
#' @param shape2 Shape of 2nd vector
#' @param rate2 Rate of 2nd vector
#' @export

compare_counts <- function(shape1, rate1, shape2, rate2) {

  k <- 0:(shape1 - 1)
  sum(exp(k * log(rate1) + shape2 * log(rate2) - (k + shape2) * log(rate1 + rate2) - log(k + shape2) - lbeta(k + 1, shape2)))

}

add_count_post <- function(df,
                           x,
                           ...,
                           .shape0 = NULL,
                           .rate0 = NULL) {

  if(is.null(.shape0)) {.shape0 <- sum(df$x)}
  if(is.null(.rate0)) {.rate0 <- sd(df$x)}

  df %>%
    group_by(...) %>%
    summarize(shape = sum(x), rate = n()) %>%
    mutate(post = map2(shape, rate, ~ estimate_normal(.shape0, .rate0, .x, .y))) %>%
    hoist(post, post_shape = "shape", post_rate = "rate")

}

add_count_comparison <- function(df) {

  df

}
