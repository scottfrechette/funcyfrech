#' Calculate gamma-poisson posterior
#'
#' @param p Vector of p-values
#' @param neg_estimate Is estimated value negative?
#' @param two_tailed Was two-tailed test used?
#' @export

p_to_z <- function(p, neg_estimate = F, two_tailed = T) {

  qnorm(p / (1 + two_tailed), lower.tail = neg_estimate)

}

#' Calculate gamma-poisson posterior
#'
#' @param z Vector of z-values
#' @param two_tailed Was two-tailed test used?
#' @export

z_to_p <- function(z, two_tailed = T) {

  pnorm(abs(z), lower.tail = F) * (1 + two_tailed)

}
