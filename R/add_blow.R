#' Bind the weighted log odds to a tidy dataset
#'
#' Calculate and bind the log odds ratio, weighted by a Dirichlet
#' prior, of a tidy dataset to the dataset itself. The weighted log odds ratio
#' is added as a column. This functions supports non-standard evaluation through
#' the tidyeval framework.
#'
#' @param tbl A tidy dataset with one row per feature and set
#' @param group Column of groups between which to compare features, such as
#' documents for text data
#' @param feature Column of features for identifying differences, such as words or
#' bigrams with text data
#' @param n Column containing feature-set counts
#' @param topic (Optional) topic to compare groups within
#' @param .prior Whether prior should be based on g-prior from empirical Bayes (informed),
#' total frequency count (empirical), or uninformed with set alpha (uninformed)
#' @param .alpha (Optional) Frequency of each feature for uninformed prior
#' @param .k_prior Penalty term for informed prior
#' @param .compare Whether to compare group-feature to entire dataset or
#' against all other groups
#' @param .complete Whether to complete all topic-group-feature combinations
#' @param .unweighted Whether to include point estimate log odds
#' @param .variance Whether to include variance of feature
#' @param .odds Whether to include odds of seeing feature within group
#' @param .prob Whether to include probability for feature within group
#'
#' @details The arguments \code{group}, \code{feature}, \code{n}, and \code{topic}
#' are passed by expression and support \link[rlang]{quasiquotation};
#' you can unquote strings and symbols. Grouping is preserved but ignored.
#'
#' The dataset must have exactly one row per topic-group-feature combination for
#' this calculation to succeed. Read Monroe, Colaresi, and Quinn (2017) for
#' more on the weighted log odds ratio.
#'
#' @source <https://doi.org/10.1093/pan/mpn018>
#'
#' @importFrom rlang sym
#' @importFrom dplyr count left_join mutate rename group_by ungroup group_vars
#' @export

add_blow <- function (tbl,
                      group,
                      feature,
                      n,
                      topic = NULL,
                      .prior = c("informed", "empirical", "uninformed"),
                      .k_prior = 0.1,
                      .alpha = 1,
                      .compare = c("dataset", "groups"),
                      .complete = FALSE,
                      .unweighted = TRUE,
                      .variance = TRUE,
                      .odds = FALSE,
                      .prob = FALSE) {

  .compare <- match.arg(.compare)
  .prior <- match.arg(.prior)

  grouping <- group_vars(tbl)
  tbl <- ungroup(tbl)

  tbl$n_wik <- pull(tbl, {{n}})

  if (.complete) {

    tbl <- tbl %>%
      tidyr::complete({{group}}, {{feature}},
                      fill = list(n_wik = 0)) %>%
      mutate({{n}} := n_wik)

  }

  tbl$.group <- pull(tbl, {{group}})
  tbl$.feature <- pull(tbl, {{feature}})
  tbl$.topic <- "none"

  if (!missing(topic)) {tbl$.topic <- pull(tbl, {{topic}})}

  if (.prior == "informed") {

    tbl <- tbl %>%
      add_count(.topic, .feature, wt = n_wik, name = "feature_cnt") %>%
      add_count(.topic, .group, wt = n_wik, name = "group_cnt") %>%
      add_count(.topic, wt = n_wik, name = "topic_cnt") %>%
      mutate(alpha_k = feature_cnt * group_cnt / topic_cnt * .k_prior,
             n_wjk = feature_cnt - n_wik) %>%
      select(-feature_cnt, -group_cnt, -topic_cnt)

  .co} else if (.prior == "empirical") {

    tbl <- tbl %>%
      add_count(.topic, .feature, wt = n_wik, name = "alpha_k") %>%
      mutate(n_wjk = alpha_k - n_wik)

  } else {

    tbl <- tbl %>%
      add_count(.topic, .feature, wt = n_wik,
                name = "feature_cnt") %>%
      mutate(alpha_k = .alpha,
             n_wjk = feature_cnt - n_wik) %>%
      select(-feature_cnt)

  }

  if (.compare == "dataset") {

    tbl <- tbl %>%
      mutate(y_wik = n_wik + alpha_k) %>%
      add_count(.topic, .feature, wt = y_wik, name = "y_wk") %>%
      add_count(.topic, .group, wt = y_wik, name = "n_ik") %>%
      add_count(.topic, wt = y_wik, name = "n_k") %>%
      mutate(omega_wik = y_wik/(n_ik - y_wik),
             omega_wk = y_wk/(n_k - y_wk),
             delta_wik = log(omega_wik) - log(omega_wk),
             sigma2_wik = 1/y_wik + 1/y_wk,
             zeta_wik = delta_wik/sqrt(sigma2_wik)) %>%
      filter(n_wik > 0) %>%
      rename(log_odds_weighted = zeta_wik,
             log_odds = delta_wik, variance = sigma2_wik) %>%
      select(-.group, -.feature, -n_wik, -.topic, -y_wik,
             -y_wk, -n_ik, -n_wjk, -n_k, -alpha_k, -omega_wik,
             -omega_wk) %>%
      mutate(odds = exp(log_odds),
             prob = odds/(1 + odds))

  } else if (.compare == "groups") {

    tbl <- tbl %>%
      mutate(y_wik = n_wik + alpha_k,
             y_wjk = n_wjk + alpha_k) %>%
      add_count(.topic, .group, wt = y_wik, name = "n_ik") %>%
      add_count(.topic, .group, wt = y_wjk, name = "n_jk") %>%
      mutate(omega_wik = y_wik/(n_ik - y_wik),
             omega_wjk = y_wjk/(n_jk - y_wjk),
             delta_wik = log(omega_wik) - log(omega_wjk),
             sigma2_wik = 1/y_wik + 1/y_wjk,
             zeta_wik = delta_wik/sqrt(sigma2_wik)) %>%
      filter(n_wik > 0) %>%
      rename(log_odds_weighted = zeta_wik,
             log_odds = delta_wik, variance = sigma2_wik) %>%
      select(-.group, -.feature, -n_wik, -.topic, -y_wik,
             -y_wjk, -n_ik, -n_jk, -n_wjk, -alpha_k, -omega_wik,
             -omega_wjk) %>%
      mutate(odds = exp(log_odds),
             prob = odds/(1 + odds))

  } else {

    stop("Comparisons can only be different from dataset or comparison to other groups")

  }

  if (!.unweighted) {tbl$log_odds <- NULL}
  if (!.variance) {tbl$variance <- NULL}
  if (!.odds) {tbl$odds <- NULL}
  if (!.prob) {tbl$prob <- NULL}

  if (!is_empty(grouping)) {tbl <- group_by(tbl, !!sym(grouping))}

  return(tbl)

}
