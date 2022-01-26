#' Calculate minimum sample size needed for binomial experiment
#'
#' Either \code{null_value} or \code{hdi_max_width} must be null,
#' depending whether the goal is reject a particular null value
#' or estimating parameter with specified precision
#'
#' @param gen_prior_mode Expected mode of prior
#' @param gen_prior_n N of prior (higher leads to more precision)
#' @param hdi_max_width Maximum width of HDI
#' @param null_value Null value
#' @param rope Region of practical equivalence around parameter
#' @param desired_power Level of power
#' @param aud_prior_mode Prior understanding of mode from audience
#' @param aud_prior Prior understanding of variation from audience
#' @param hdi_mass Range of HDI
#' @param init_samp_size Initial sample size
#' @param verbose Should each attempt be displayed?
#'
#' @details Function was derived by Solomon Kurz's bookdown
#' converting Kruschke's book into [brms] and [tidyverse]
#'
#' @source <https://bookdown.org/content/3686/goals-power-and-sample-size.html#computing-power-and-sample-size/>
#'
#' @export

power_analysis_binomial <- function(gen_prior_mode,
                                    gen_prior_n,
                                    hdi_max_width = NULL,
                                    null_value = NULL,
                                    rope = c(max(0, null_value - 0.02), min(1, null_value + 0.02)),
                                    desired_power = 0.8,
                                    aud_prior_mode = 0.5,
                                    aud_prior_n = 2,
                                    hdi_mass = 0.95,
                                    init_samp_size = 20,
                                    verbose = TRUE) {

  # Check for argument consistency:
  if (!xor(is.null(hdi_max_width), is.null(null_value))) {
    stop("One and only one of `hdi_max_width` and `null_value` must be specified.")
  }

  # Convert prior mode and N to a, b parameters of beta distribution:
  gen_prior_a <-        gen_prior_mode  * (gen_prior_n - 2) + 1
  gen_prior_b <- (1.0 - gen_prior_mode) * (gen_prior_n - 2) + 1
  aud_prior_a <-        aud_prior_mode  * (aud_prior_n - 2) + 1
  aud_prior_b <- (1.0 - aud_prior_mode) * (aud_prior_n - 2) + 1

  # Initialize loop for incrementing `sample_size`:
  sample_size <- init_samp_size
  not_powerful_enough = TRUE

  # Increment `sample_size` until desired power is achieved:
  while(not_powerful_enough) {
    z_vec <- 0:sample_size # vector of all possible z values for N flips.

    # Compute probability of each z value for data-generating prior:
    p_z_vec <- exp(lchoose(sample_size, z_vec)
                   + lbeta(z_vec + gen_prior_a, sample_size - z_vec + gen_prior_b)
                   - lbeta(gen_prior_a, gen_prior_b))

    # For each z value, compute posterior HDI:
    # `hdi_matrix` will hold HDI limits for each z:
    hdi_matrix <- matrix(0, nrow = length(z_vec), ncol = 2)
    for (z_id_x in 1:length(z_vec)) {
      z <- z_vec[z_id_x]
      hdi_matrix[z_id_x, ] <- hdi_of_icdf(qbeta,
                                          shape1 = z + aud_prior_a,
                                          shape2 = sample_size - z + aud_prior_b,
                                          width  = hdi_mass)
    }

    # Compute HDI widths:
    hdi_width <- hdi_matrix[, 2] - hdi_matrix[, 1]

    # Sum the probabilities of outcomes with satisfactory HDI widths:
    if (!is.null(hdi_max_width)) {
      power_hdi <- sum(p_z_vec[hdi_width < hdi_max_width])
    }

    # Sum the probabilities of outcomes with HDI excluding `rope`:
    if (!is.null(null_value)) {
      power_hdi <- sum(p_z_vec[hdi_matrix[, 1] > rope[2] | hdi_matrix[, 2] < rope[1]])
    }

    if (verbose) {
      cat(" For sample size = ", sample_size, ", power = ", power_hdi,
          "\n", sep = ""); flush.console()
    }

    if (power_hdi > desired_power) {  # If desired power is attained,
      not_powerful_enough = FALSE
    } else {
      sample_size <- sample_size + 1
      # set flag to stop,
      # otherwise
      # increment the sample size.
    }

  } # End while( not_powerful_enough )

  # Return the sample size that achieved the desired power:
  return(sample_size)
}
