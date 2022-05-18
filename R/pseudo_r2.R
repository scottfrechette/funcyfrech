pseudo_r2 <- function(df, success, total, ...) {

  s <- sum(pull(df, {{success}}))
  t <- sum(pull(df, {{total}}))

  null <- log_lik_binomial(s, t)

  df %>%
    group_by(...) %>%
    summarize(success = sum({{success}}), total = sum({{total}}),
              .groups = "drop") %>%
    mutate(ll = map2_dbl(success, total, log_lik_binomial)) %>%
    summarize(model = sum(ll)) %>%
    mutate(pseudo_r2 = 1 - model / null) %>%
    pull(pseudo_r2)

}

log_lik_binomial <- function(success, total) {

  fail <- total - success
  p <- success / total
  not_p <- 1 - p

  log(p^success * not_p^fail)

}
