#' Bind the weighted log odds to a tidy dataset
#'
#' Calculate and bind the log odds ratio, weighted by an uninformative Dirichlet
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
#' @param .prior Whether prior should be based on total frequency count (empirical),
#'  g-prior from empirical Bayes (informed) or uninformed with set alpha (uninformed)
#' @param .alpha (Optional) Frequency of each feature for uninformed prior
#' @param .k_prior Penalty term for informed prior
#' @param .compare Whether to compare group-feature to entire dataset or
#' against all other groups
#' @param .center Whether to center log_odds_weighted across groups
#' @param .unweighted Whether to include point estimate log odds
#' @param .variance Whether to include variance of feature
#' @param .odds Whether to include odds of seeing feature within group
#' @param .prob Whether to include probability for feature within group
#'
#' @details The arguments \code{group}, \code{feature}, \code{n}, and \code{topic}
#' are passed by expression and support \link[rlang]{quasiquotation};
#' you can unquote strings and symbols. Grouping is preserved but ignored.
#'
#' The dataset must have exactly one row per group-feature combination for
#' this calculation to succeed. Read Monroe, Colaresi, and Quinn (2017) for
#' more on the weighted log odds ratio.
#'
#' @source <https://doi.org/10.1093/pan/mpn018>
#'
#' @importFrom rlang sym
#' @importFrom dplyr count left_join mutate rename group_by ungroup group_vars
#' @export

add_blow <- function(tbl,
                     group,
                     feature,
                     n,
                     topic = NULL,
                     .prior = c("empirical", "informed", "uninformed"),
                     .alpha = 1,
                     .k_prior = 0.1,
                     .compare = c("dataset", "groups"),
                     .center = FALSE,
                     .unweighted = TRUE,
                     .variance = TRUE,
                     .odds = TRUE,
                     .prob = TRUE) {

  .compare <- match.arg(.compare)
  .prior <- match.arg(.prior)

  # groups preserved but ignored
  grouping <- group_vars(tbl)
  tbl <- ungroup(tbl)

  tbl$n_wik <- pull(tbl, {{n}})

  tbl <- tbl %>%
    tidyr::complete({{group}}, {{feature}}, fill = list(n_wik = 0)) %>%
    mutate({{n}} := n_wik)

  tbl$.group <- pull(tbl, {{group}})
  tbl$.feature <- pull(tbl, {{feature}})
  tbl$.topic <- "none"

  if (!missing(topic)) {tbl$.topic <- pull(tbl, {{topic}})}

  if(.prior == "empirical") {

    tbl <- tbl %>%
      add_count(.topic, .feature, wt = n_wik, name = "alpha_k") %>% # count of each feature
      mutate(n_wjk = alpha_k - n_wik)                               # count of each feature in group j

  } else if (.prior == "informed") {

    tbl <- tbl %>%
      add_count(.topic, .feature, wt = n_wik, name = "feature_cnt") %>%
      add_count(.topic, .group, wt = n_wik, name = "group_cnt") %>%
      add_count(.topic, wt = n_wik, name = "topic_cnt") %>%
      mutate(alpha_k = feature_cnt * group_cnt / topic_cnt * .k_prior,
             n_wjk = feature_cnt - n_wik) %>%
      select(-feature_cnt, -group_cnt, -topic_cnt)

  } else {

    tbl <- tbl %>%
      add_count(.topic, .feature, wt = n_wik, name = "feature_cnt") %>%
      mutate(alpha_k = .alpha,
             n_wjk = n_wik - feature_cnt) %>%
      select(-feature_cnt)

  }

  if (.compare == "dataset") {

    tbl <- tbl %>%
      mutate(y_wik = n_wik + alpha_k) %>%                        # pseudo count of each feature in group i
      add_count(.topic, .feature, wt = y_wik, name = "y_wk") %>% # pseudo count of each feature
      add_count(.topic, .group, wt = y_wik, name = "n_ik") %>%   # pseudo count of all features in group i
      add_count(.topic, wt = y_wik, name = "n_k") %>%            # pseudo count of all features
      mutate(
        omega_wik = y_wik / (n_ik - y_wik),                      # odds of feature in group i
        omega_wk = y_wk / (n_k - y_wk),                          # overall odds of feature
        delta_wik = log(omega_wik) - log(omega_wk),              # equation 15
        sigma2_wik = 1 / y_wik + 1 / y_wk,                       # equation 18
        # sigma2_wik = 1 / y_wik + 1 / (n_ik - y_wik) +
        #   1 / y_wk + 1 / (n_k - y_wk),
        zeta_wik = delta_wik / sqrt(sigma2_wik)                  # equation 21
      ) %>%
      filter(n_wik > 0) %>%
      rename(log_odds_weighted = zeta_wik,
             log_odds = delta_wik,
             variance = sigma2_wik) %>%
      select(-.group, -.feature, -n_wik, -.topic,
             -y_wik, -y_wk, -n_ik, -n_wjk, -n_k,
             -alpha_k, -omega_wik, -omega_wk) %>%
      mutate(odds = exp(log_odds),
             prob = odds / (1 + odds))

  } else if (.compare == "groups") {

    tbl <- tbl %>%
      mutate(y_wik = n_wik + alpha_k,                             # pseudo count of each feature in group i
             y_wjk = n_wjk + alpha_k) %>%                         # pseudo count of each feature in group j
      add_count(.topic, .group, wt = y_wik, name = "n_ik") %>%    # pseudo count of all features in group i
      add_count(.topic, .group, wt = y_wjk, name = "n_jk") %>%    # pseudo count of all features in group j
      mutate(
        omega_wik = y_wik / (n_ik - y_wik),                       # odds of feature in group i
        omega_wjk = y_wjk / (n_jk - y_wjk),                       # odds of feature in group j
        delta_wik = log(omega_wik) - log(omega_wjk),              # equation 16
        sigma2_wik = 1 / y_wik + 1 / y_wjk,                       # equation 20
        # sigma2_wik = 1 / y_wik + 1 / (n_ik - y_wik) +
        #   1 / y_wjk + 1 / (n_jk - y_wjk),
        zeta_wik = delta_wik / sqrt(sigma2_wik)                   # equation 22
      ) %>%
      filter(n_wik > 0) %>%
      rename(log_odds_weighted = zeta_wik,
             log_odds = delta_wik,
             variance = sigma2_wik) %>%
      select(-.group, -.feature, -n_wik, -.topic,
             -y_wik, -y_wjk, -n_ik, -n_jk, -n_wjk,
             -alpha_k, -omega_wik, -omega_wjk) %>%
      mutate(odds = exp(log_odds),
             prob = odds / (1 + odds))

  } else {

    stop("Comparisons can only be different from dataset or comparison to other groups")

  }

  if(.center) {

    tbl <- tbl %>%
      group_by({{feature}}) %>%
      mutate(log_odds_weighted = log_odds_weighted - mean(log_odds_weighted)) %>%
      ungroup()
    }

  if(!.unweighted) {tbl$log_odds <- NULL}

  if(!.variance) {tbl$variance <- NULL}

  if(!.odds) {tbl$odds <- NULL}

  if(!.prob) {tbl$prob <- NULL}

  if (!is_empty(grouping)) {tbl <- group_by(tbl, !!sym(grouping))}

  return(tbl)

}
