#' Calculate normal-normal posterior
#'
#' @param prior_mean Mean set by prior
#' @param prior_sd SD set by prior
#' @param data_mean Mean of collected data
#' @param data_sd SD of collected data
#' @param data_n Number of records for collected data
#' @param plot Choose to list posterior parameters or plot output
#' @export

estimate_normal <- function(prior_mean,
                            prior_sd,
                            data_mean,
                            data_sd,
                            data_n,
                            plot = F) {

  prior_precision <- 1 / prior_sd ^ 2
  data_se <- data_sd / sqrt(data_n)
  data_precision <- 1 / data_se ^ 2
  post_precision <- prior_precision + data_precision
  post_sd <- sqrt(1 / post_precision)
  post_mean <- weighted.mean(c(prior_mean, data_mean),
                             c(prior_precision, data_precision))

  if(plot) {

    lower = min(qnorm(0.05, prior_mean, prior_sd),
                qnorm(0.05, data_mean, data_se),
                qnorm(0.05, post_mean, post_sd))

    upper = max(qnorm(0.95, prior_mean, prior_sd),
                qnorm(0.95, data_mean, data_se),
                qnorm(0.95, post_mean, post_sd))

    ggplot(data = tibble(x = c(lower, upper)), aes(x)) +
      stat_function(fun = dnorm, n = 101,
                    args = list(mean = prior_mean, sd = prior_sd),
                    linetype = 2, color = "blue") +
      stat_function(fun = dnorm, n = 101,
                    args = list(mean = data_mean, sd = data_se),
                    linetype = 2, color = "red") +
      stat_function(fun = dnorm, n = 101,
                    args = list(mean = post_mean, sd = post_sd)) +
      theme_minimal()

  } else {

    list(mean = post_mean, sd = post_sd)

  }

}

#' Calculate chance a > b based on closed normal form
#'
#' @param mean1 Mean of 1st vector
#' @param sd1 SD of 1st vector
#' @param mean2 Mean of 2nd vector
#' @param sd2 SD of 2nd vector
#' @param log Whether to use log values
#' @export

compare_normal <- function(mean1, sd1, mean2, sd2, log = FALSE) {

  var1 <- sd1^2
  var2 <- sd2^2

  pnorm(0, mean2 - mean1, sqrt(var1 + var2), log.p = log)

}

#' Add normal posterior to data frame
#'
#' @param df Dataframe
#' @param x Vector
#' @param ... Grouping (optional)
#' @param .mean0 Prior mean, if null then calculated empirically
#' @param .sd0 Prior SD, if null then calculated empirically
#' @export

add_normal_post <- function(df,
                            x,
                            ...,
                            .mean0 = NULL,
                            .sd0 = NULL) {

  if(is.null(.mean0)) {.mean0 <- mean(df$x)}
  if(is.null(.sd0)) {.sd0 <- sd(df$x)}

  df %>%
    group_by(...) %>%
    summarize(mean = mean(x), sd = sd(x), n= n()) %>%
    mutate(post = pmap(list(mean, sd, n), ~ estimate_normal(.mean0, .sd0, ..1, ..2, ..3))) %>%
    hoist(post, post_mean = "mean", post_sd = "sd")

}

add_normal_comparison <- function(df,
                                  x) {


}
