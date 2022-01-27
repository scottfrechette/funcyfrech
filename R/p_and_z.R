#' Convert p-value to z-score
#'
#' @param p Vector of p-values
#' @param neg_estimate Is estimated value negative?
#' @param two_tailed Was two-tailed test used?
#' @export

p_to_z <- function(p, neg_estimate = F, two_tailed = T) {

  z <- qnorm(p / (1 + two_tailed))

  if_else(neg_estimate, z, -z)

}

#' Convert z-score to p-value
#'
#' @param z Vector of z-values
#' @param two_tailed Was two-tailed test used?
#' @export

z_to_p <- function(z, two_tailed = T) {

  pnorm(abs(z), lower.tail = F) * (1 + two_tailed)

}
