#' Calculate normal-normal posterior
#'
#' @name update_normal
#' @param prior_mean Mean set by prior
#' @param prior_sd SD set by prior
#' @param data_mean Mean of collected data
#' @param data_se SD of data divided by square root of observations
#' @export

normal_update <- function(prior_mean, prior_sd,
                          data_mean, data_sd, data_n,
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

    list(post_mean, post_sd)

  }

}
