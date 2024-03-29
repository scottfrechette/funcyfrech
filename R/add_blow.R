#' Bind the weighted log odds to a tidy dataset
#'
#' Calculate and bind the log odds ratio, weighted by a Dirichlet
#' prior, of a tidy dataset to the dataset itself. The weighted log odds ratio
#' is added as a column named \code{zeta}, with optional columns
#' \code{log_odds}, \code{variance}, \code{odds}, and \code{prob}.
#' This functions supports non-standard evaluation through the tidyeval framework.
#'
#' @param df A tidy dataset with one row per feature and set
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
#' @param .log_odds Whether to include point estimate log odds
#' @param .se Whether to include standard error of estimate
#' @param .odds Whether to include odds of seeing feature within group
#' @param .prob Whether to include probability for feature within group
#' @param .sort Whether to sort by largest zeta
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

add_blow <- function (df,
                      group,
                      feature,
                      n,
                      topic = NULL,
                      .prior = c("empirical", "uninformative", "tidylo"),
                      .compare = c("dataset", "others"),
                      .k_prior = 1,
                      .alpha_prior = 1,
                      .log_odds = FALSE,
                      .se = FALSE,
                      .odds = FALSE,
                      .prob = FALSE,
                      .sort = FALSE) {

  .compare <- match.arg(.compare)
  .prior <- match.arg(.prior)

  grouping <- group_vars(df)
  df <- ungroup(df)

  df$y_kwi <- pull(df, {{n}})

  if (.compare == "others") {

    df <- df %>%
      tidyr::complete({{topic}}, {{group}}, {{feature}},
                      fill = list(y_kwi = 0)) %>%
      mutate({{n}} := y_kwi)

  }

  df$.group <- pull(df, {{group}})
  df$.feature <- pull(df, {{feature}})
  df$.topic <- "none"

  if (!missing(topic)) {df$.topic <- pull(df, {{topic}})}

  # include option for average topic-group count?
  if (.prior == "empirical") {

    df <- df %>%
      add_tally(wt = y_kwi, name = "total_cnt") %>%
      add_count(.feature, wt = y_kwi, name = "feature_cnt") %>%
      add_count(.topic, .group, wt = y_kwi, name = "topic_group_cnt") %>%
      add_count(.topic, .feature, wt = y_kwi, name = "topic_feature_cnt") %>%
      mutate(alpha_kwi = feature_cnt/total_cnt * topic_group_cnt * .k_prior) %>%
      add_count(.topic, .feature, wt = alpha_kwi, name = "alpha_kw") %>%
      mutate(y_kwj = topic_feature_cnt - y_kwi,
             alpha_kwj = alpha_kw - alpha_kwi) %>%
      select(-total_cnt, -feature_cnt, -topic_feature_cnt, -topic_group_cnt, -alpha_kw)

  } else if (.prior == "uninformative") {

    if (!is.null(.alpha_prior)) {

      .alpha <- .alpha_prior

    } else {

      .alpha <- sum(df$y_kwi) / n_distinct(df$.feature)

    }

    df <- df %>%
      add_count(.topic, .feature, wt = y_kwi, name = "feature_cnt") %>%
      mutate(alpha_kwi = .alpha,
             alpha_kwj = .alpha,
             y_kwj = feature_cnt - y_kwi) %>%
      select(-feature_cnt)

  } else {

    df <- df %>%
      add_count(.topic, .feature, wt = y_kwi, name = "feature_cnt") %>%
      mutate(alpha_kwi = feature_cnt,
             alpha_kwj = feature_cnt,
             y_kwj = alpha_kwi - y_kwi) %>%
      select(-feature_cnt)

  }

  if (.compare == "dataset") {

    df <- df %>%
      add_count(.topic, .feature, wt = y_kwi, name = "y_kw") %>%
      add_count(.topic, .feature, wt = alpha_kwi, name = "alpha_kw") %>%
      add_count(.topic, .group, wt = y_kwi, name = "n_ki") %>%
      add_count(.topic, .group, wt = alpha_kwi, name = "alpha_k0i") %>%
      add_count(.topic, wt = y_kwi, name = "n_k") %>%
      add_count(.topic, wt = alpha_kwi, name = "alpha_k0") %>%
      mutate(omega_kwi = (y_kwi + alpha_kwi) / (n_ki + alpha_k0i - y_kwi - alpha_kwi),
             omega_kw = (y_kw + alpha_kw) / (n_k + alpha_k0 - y_kw - alpha_kw),
             delta_kwi = log(omega_kwi) - log(omega_kw),
             sigma_kwi = sqrt(1 / (y_kwi + alpha_kwi) +
                                1 / (n_ki + alpha_k0i - y_kwi - alpha_kwi) +
                                1 / (y_kw + alpha_kw) +
                                1 / (n_k + alpha_k0- y_kw - alpha_kw)),
             zeta_kwi = delta_kwi / sigma_kwi) %>%
      filter(y_kwi > 0) %>%
      rename(log_odds = delta_kwi,
             se = sigma_kwi,
             zeta = zeta_kwi) %>%
      select(-.group, -.feature, -.topic,
             -y_kwi, -y_kwj, -y_kw,
             -n_ki, -n_k,
             -alpha_kwi, -alpha_kw,
             -alpha_k0i, -alpha_k0,
             -omega_kwi, -omega_kw) %>%
      mutate(odds = exp(log_odds),
             prob = odds / (1 + odds))

  } else if (.compare == "others") {

    df <- df %>%
      add_count(.topic, .group, wt = y_kwi, name = "n_ki") %>%
      add_count(.topic, .group, wt = alpha_kwi, name = "alpha_k0i") %>%
      add_count(.topic, wt = y_kwi, name = "n_k") %>%
      add_count(.topic, wt = alpha_kwi, name = "alpha_k0") %>%
      mutate(n_kj = n_k - n_ki,
             alpha_k0j = alpha_k0 - alpha_k0i) %>%
      mutate(omega_kwi = (y_kwi + alpha_kwi)/(n_ki + alpha_k0i - y_kwi - alpha_kwi),
             omega_kwj = (y_kwj + alpha_kwj)/(n_kj + alpha_k0j - y_kwj - alpha_kwj),
             delta_kwi = log(omega_kwi) - log(omega_kwj),
             sigma_kwi = sqrt(1/(y_kwi + alpha_kwi) +
                                1/(n_ki + alpha_k0i - y_kwi - alpha_kwi) +
                                1/(y_kwj + alpha_kwj) +
                                1/(n_kj + alpha_k0j - y_kwj - alpha_kwj)),
             zeta_kwi = delta_kwi / sigma_kwi) %>%
      filter(y_kwi > 0) %>%
      rename(log_odds = delta_kwi,
             se = sigma_kwi,
             zeta = zeta_kwi) %>%
      select(-.group, -.feature, -.topic,
             -y_kwi, -y_kwj, -y_kwj,
             -n_k, -n_ki, -n_kj,
             -alpha_kwi,- alpha_kwj,
             -alpha_k0, -alpha_k0i, -alpha_k0j,
             -omega_kwi, -omega_kwj) %>%
      mutate(odds = exp(log_odds),
             prob = odds / (1 + odds))

  } else {

    stop("Comparisons can only be different from dataset or comparison to other groups")

  }

  if (!.log_odds) {df$log_odds <- NULL}
  if (!.se) {df$se <- NULL}
  if (!.odds) {df$odds <- NULL}
  if (!.prob) {df$prob <- NULL}

  if (.sort) {df <- arrange(df, -zeta)}

  if (!is_empty(grouping)) {df <- group_by(df, !!sym(grouping))}

  return(df)

}
