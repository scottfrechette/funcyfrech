#' Bind the weighted log odds to a tidy dataset
#'
#' Calculate HDI range from any common distribution
#'
#' @param name Distribution name
#' @param width Width of HDI
#' @param tol Tolerance to pass to \code{optimize}
#' @param ... parameters to pass for particular distribution
#'
#' @details Function was derived by Solomon Kurz's bookdown
#' converting Kruschke's book into [brms] and [tidyverse]
#'
#' @source <https://bookdown.org/content/3686/goals-power-and-sample-size.html#computing-power-and-sample-size/>
#'
#' @export

hdi_of_icdf <- function(name, width = .95, tol = 1e-8, ... ) {

  incredible_mass <- 1.0 - width
  interval_width <- function(low_tail_prob, name, width, ...) {
    name(width + low_tail_prob, ...) - name(low_tail_prob, ...)
  }
  opt_info <- optimize(interval_width, c(0, incredible_mass),
                       name = name, width = width,
                       tol = tol, ...)
  hdi_lower_tail_prob <- opt_info$minimum

  return(c(name(hdi_lower_tail_prob, ...),
           name(width + hdi_lower_tail_prob, ...)))

}
