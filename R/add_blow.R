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
#' @param .prior Whether prior should be based on g-prior from empirical Bayes,
#' uninformed with set alpha (uninformed), or total frequency count from tidylo
#' implementation
#' @param .compare Whether to compare group-feature to entire dataset or
#' against all other groups
#' @param .alpha_prior Frequency of each feature for uninformed prior
#' @param .k_prior Penalty term for informed prior
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

# more closely follow nomenclature from paper
# rename to y_kwi, etc. to match paper

add_blow <- function (tbl,
                      group,
                      feature,
                      n,
                      topic = NULL,
                      .prior = c("empirical", "uninformative", "tidylo"),
                      .compare = c("dataset", "groups"),
                      .k_prior = 0.1,
                      .alpha_prior = 1,
                      .complete = FALSE,
                      .unweighted = TRUE,
                      .variance = TRUE,
                      .odds = FALSE,
                      .prob = FALSE) {

  .compare <- match.arg(.compare)
  .prior <- match.arg(.prior)

  grouping <- group_vars(tbl)
  tbl <- ungroup(tbl)

  tbl$y_wik <- pull(tbl, {{n}})

  if (.complete) {

    tbl <- tbl %>%
      tidyr::complete({{group}}, {{feature}},
                      fill = list(y_wik = 0)) %>%
      mutate({{n}} := y_wik)

  }

  tbl$.group <- pull(tbl, {{group}})
  tbl$.feature <- pull(tbl, {{feature}})
  tbl$.topic <- "none"

  if (!missing(topic)) {tbl$.topic <- pull(tbl, {{topic}})}

  # include option for average topic-group count?
  if (.prior == "empirical") {

    tbl <- tbl %>%
      add_tally(wt = y_wik, name = "total_cnt") %>%
      add_count(.feature, wt = y_wik, name = "feature_cnt") %>%
      add_count(.topic, .group, wt = y_wik, name = "topic_group_cnt") %>%
      add_count(.topic, .feature, wt = y_wik, name = "topic_feature_cnt") %>%
      mutate(alpha_wik = feature_cnt / total_cnt * topic_group_cnt * .k_prior,
             y_wjk = topic_feature_cnt - y_wik) %>%
      select(-total_cnt, -feature_cnt, -topic_feature_cnt, -topic_group_cnt)

  } else if (.prior == "uninformative") {

    if (!is.null(.alpha_prior)) {

      .alpha <- .alpha_prior

    } else {

      .alpha <- sum(tbl$y_wik) / n_distinct(tbl$.feature)

    }

    tbl <- tbl %>%
      add_count(.topic, .feature, wt = y_wik, name = "feature_cnt") %>%
      mutate(alpha_wik = .alpha,
             y_wjk = feature_cnt - y_wik) %>%
      select(-feature_cnt)

  } else {

    tbl <- tbl %>%
      add_count(.topic, .feature, wt = y_wik, name = "feature_cnt") %>%
      mutate(alpha_wik = feature_cnt * .k_prior,
             y_wjk = alpha_wik - y_wik) %>%
      select(-feature_cnt)

  }

  if (.compare == "dataset") {

    tbl <- tbl %>%
      add_count(.topic, .feature, wt = y_wik, name = "y_wk") %>%
      add_count(.topic, .feature, wt = alpha_wik, name = "alpha_wk") %>%
      add_count(.topic, .group, wt = y_wik, name = "n_ik") %>%
      add_count(.topic, .group, wt = alpha_wik, name = "alpha_ik") %>%
      add_count(.topic, wt = y_wik, name = "n_k") %>%
      add_count(.topic, wt = alpha_wik, name = "alpha_k") %>%
      mutate(omega_wik = (y_wik + alpha_wik) / (n_ik + alpha_ik - y_wik - alpha_wik),
             omega_wk = (y_wk + alpha_wk) / (n_k + alpha_k - y_wk - alpha_wk),
             delta_wik = log(omega_wik) - log(omega_wk),
             sigma2_wik = 1 / (y_wik + alpha_wik) + 1 / (n_ik + alpha_ik - y_wik - alpha_wik) +
               1 / (y_wk + alpha_wk) + 1 / (n_k + alpha_k - y_wk - alpha_wk),
             zeta_wik = delta_wik / sqrt(sigma2_wik)) %>%
      filter(y_wik > 0) %>%
      rename(log_odds = delta_wik,
             variance = sigma2_wik,
             zeta = zeta_wik) %>%
      select(-.group, -.feature, -.topic,
             -y_wik, -y_wjk, -y_wk, -n_ik, -n_k,
             -alpha_wik, -alpha_wk, -alpha_ik, -alpha_k,
             -omega_wik, -omega_wk) %>%
      mutate(odds = exp(log_odds),
             prob = odds / (1 + odds))

  } else if (.compare == "groups") {

    tbl <- tbl %>%
      add_count(.topic, .group, wt = y_wik, name = "n_ik") %>%
      add_count(.topic, .group, wt = alpha_wik, name = "alpha_ik") %>%
      add_count(.topic, .group, wt = y_wjk, name = "n_jk") %>%
      mutate(omega_wik = (y_wik + alpha_wik) / (n_ik + alpha_ik - y_wik - alpha_wik),
             omega_wjk = (y_wjk + alpha_wik) / (n_jk + alpha_ik - y_wjk - alpha_wik),
             delta_wik = log(omega_wik) - log(omega_wjk),
             sigma2_wik = 1 / (y_wik + alpha_wik) + 1 / (n_ik + alpha_ik - y_wik - alpha_wik) +
               1 / (y_wjk + alpha_wik) + 1 / (n_jk + alpha_ik - y_wjk - alpha_wik),
             zeta_wik = delta_wik / sqrt(sigma2_wik)) %>%
      filter(y_wik > 0) %>%
      rename(log_odds = delta_wik,
             variance = sigma2_wik,
             zeta = zeta_wik) %>%
      select(-.group, -.feature, -.topic,
             -y_wik, -y_wjk, -y_wjk, -n_ik, -n_jk,
             -alpha_wik, -alpha_ik,
             -omega_wik, -omega_wjk) %>%
      mutate(odds = exp(log_odds),
             prob = odds / (1 + odds))

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
