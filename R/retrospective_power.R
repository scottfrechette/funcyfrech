power_retro <- function(effect_size,
                        std_error,
                        alpha = 0.05,
                        df = Inf,
                        sims = 1e6){

  # retrospective design analysis
  # http://www.stat.columbia.edu/~gelman/research/unpublished/retropower.pdf
  # type S: a statistically significant finding has this chance of appearing with wrong sign
  # type M or exaggeration: a statistically significant finding would overestimate magnitude of effect by this much

  z <- qt(1 - alpha / 2, df)
  p_hi <- 1 - pt(z - effect_size / std_error, df)
  p_lo <- pt(-z - effect_size / std_error, df)
  power <- p_hi + p_lo
  sign <- p_lo / power
  estimate <- effect_size + std_error * rt(sims, df)
  significant <- abs(estimate) > std_error * z
  exaggeration <- mean(abs(estimate)[significant]) / effect_size

  return(list(power = power, typeS = sign, typeM = exaggeration))

}
