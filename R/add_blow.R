#' Bind the weighted log odds to a tidy dataset
#'
#' Calculate and bind the log odds ratio, weighted by a Dirichlet
#' prior, of a tidy dataset to the dataset itself. The weighted log odds ratio
#' is added as a column named \code{zeta}, with optional columns
#' \code{log_odds}, \code{variance}, \code{odds}, and \code{prob}.
#' This functions supports non-standard evaluation through the tidyeval framework.
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
#' @param .log_odds Whether to include point estimate log odds
#' @param .variance Whether to include variance of feature
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
                      .log_odds = FALSE,
                      .variance = FALSE,
                      .odds = FALSE,
                      .prob = FALSE,
                      .sort = FALSE) {

  .compare <- match.arg(.compare)
  .prior <- match.arg(.prior)

  grouping <- group_vars(tbl)
  tbl <- ungroup(tbl)

  tbl$y_kwi <- pull(tbl, {{n}})

  if (.complete) {

    tbl <- tbl %>%
      tidyr::complete({{group}}, {{feature}},
                      fill = list(y_kwi = 0)) %>%
      mutate({{n}} := y_kwi)

  }

  tbl$.group <- pull(tbl, {{group}})
  tbl$.feature <- pull(tbl, {{feature}})
  tbl$.topic <- "none"

  if (!missing(topic)) {tbl$.topic <- pull(tbl, {{topic}})}

  # include option for average topic-group count?
  if (.prior == "empirical") {

    tbl <- tbl %>%
      add_tally(wt = y_kwi, name = "total_cnt") %>%
      add_count(.feature, wt = y_kwi, name = "feature_cnt") %>%
      add_count(.topic, .group, wt = y_kwi, name = "topic_group_cnt") %>%
      add_count(.topic, .feature, wt = y_kwi, name = "topic_feature_cnt") %>%
      mutate(alpha_kwi = feature_cnt / total_cnt * topic_group_cnt * .k_prior,
             y_kwj = topic_feature_cnt - y_kwi) %>%
      select(-total_cnt, -feature_cnt, -topic_feature_cnt, -topic_group_cnt)

  } else if (.prior == "uninformative") {

    if (!is.null(.alpha_prior)) {

      .alpha <- .alpha_prior

    } else {

      .alpha <- sum(tbl$y_kwi) / n_distinct(tbl$.feature)

    }

    tbl <- tbl %>%
      add_count(.topic, .feature, wt = y_kwi, name = "feature_cnt") %>%
      mutate(alpha_kwi = .alpha,
             y_kwj = feature_cnt - y_kwi) %>%
      select(-feature_cnt)

  } else {

    tbl <- tbl %>%
      add_count(.topic, .feature, wt = y_kwi, name = "feature_cnt") %>%
      mutate(alpha_kwi = feature_cnt * .k_prior,
             y_kwj = alpha_kwi - y_kwi) %>%
      select(-feature_cnt)

  }

  if (.compare == "dataset") {

    tbl <- tbl %>%
      add_count(.topic, .feature, wt = y_kwi, name = "y_kw") %>%
      add_count(.topic, .feature, wt = alpha_kwi, name = "alpha_kw") %>%
      add_count(.topic, .group, wt = y_kwi, name = "n_ki") %>%
      add_count(.topic, .group, wt = alpha_kwi, name = "alpha_k0i") %>%
      add_count(.topic, wt = y_kwi, name = "n_k") %>%
      add_count(.topic, wt = alpha_kwi, name = "alpha_k0") %>%
      mutate(omega_kwi = (y_kwi + alpha_kwi) / (n_ki + alpha_k0i - y_kwi - alpha_kwi),
             omega_kw = (y_kw + alpha_kw) / (n_k + alpha_k0 - y_kw - alpha_kw),
             delta_kwi = log(omega_kwi) - log(omega_kw),
             sigma2_kwi = 1 / (y_kwi + alpha_kwi) + 1 / (n_ki + alpha_k0i - y_kwi - alpha_kwi) +
               1 / (y_kw + alpha_kw) + 1 / (n_k + alpha_k0- y_kw - alpha_kw),
             zeta_kwi = delta_kwi / sqrt(sigma2_kwi)) %>%
      filter(y_kwi > 0) %>%
      rename(log_odds = delta_kwi,
             variance = sigma2_kwi,
             zeta = zeta_kwi) %>%
      select(-.group, -.feature, -.topic,
             -y_kwi, -y_kwj, -y_kw, -n_ki, -n_k,
             -alpha_kwi, -alpha_kw, -alpha_k0i, -alpha_k0,
             -omega_kwi, -omega_kw) %>%
      mutate(odds = exp(log_odds),
             prob = odds / (1 + odds))

  } else if (.compare == "groups") {

    tbl <- tbl %>%
      add_count(.topic, .group, wt = y_kwi, name = "n_ki") %>%
      add_count(.topic, .group, wt = alpha_kwi, name = "alpha_k0i") %>%
      add_count(.topic, .group, wt = y_kwj, name = "n_kj") %>%
      mutate(omega_kwi = (y_kwi + alpha_kwi) / (n_ki + alpha_k0i - y_kwi - alpha_kwi),
             omega_kwj = (y_kwj + alpha_kwi) / (n_kj + alpha_k0i - y_kwj - alpha_kwi),
             delta_kwi = log(omega_kwi) - log(omega_kwj),
             sigma2_kwi = 1 / (y_kwi + alpha_kwi) + 1 / (n_ki + alpha_k0i - y_kwi - alpha_kwi) +
               1 / (y_kwj + alpha_kwi) + 1 / (n_kj + alpha_k0i - y_kwj - alpha_kwi),
             zeta_kwi = delta_kwi / sqrt(sigma2_kwi)) %>%
      filter(y_kwi > 0) %>%
      rename(log_odds = delta_kwi,
             variance = sigma2_kwi,
             zeta = zeta_kwi) %>%
      select(-.group, -.feature, -.topic,
             -y_kwi, -y_kwj, -y_kwj, -n_ki, -n_kj,
             -alpha_kwi, -alpha_k0i,
             -omega_kwi, -omega_kwj) %>%
      mutate(odds = exp(log_odds),
             prob = odds / (1 + odds))

  } else {

    stop("Comparisons can only be different from dataset or comparison to other groups")

  }

  if (!.log_odds) {tbl$log_odds <- NULL}
  if (!.variance) {tbl$variance <- NULL}
  if (!.odds) {tbl$odds <- NULL}
  if (!.prob) {tbl$prob <- NULL}

  if (.sort) {tbl <- arrange(tbl, -zeta)}

  if (!is_empty(grouping)) {tbl <- group_by(tbl, !!sym(grouping))}

  return(tbl)

}

#' @export
add_rolling_blow <- function (tbl,
                              group,
                              feature,
                              n,
                              date,
                              window = NULL,
                              ratio = NULL,
                              topic = NULL,
                              .prior = c("empirical", "uninformative", "tidylo"),
                              .compare = c("dataset", "groups"),
                              .k_prior = 0.1,
                              .alpha_prior = 1) {

  if (is.null(window) & is.null(ratio)) stop("Please select either window or ratio")

  if (!is.null(window)) ratio <- 2 / (window + 1)

  tbl$y_kwi <- pull(tbl, {{n}})
  tbl$.dt <- pull(tbl, {{date}})
  tbl$.group <- pull(tbl, {{group}})
  tbl$.feature <- pull(tbl, {{feature}})
  tbl$.topic <- "none"

  if (!missing(topic)) {tbl$.topic <- pull(tbl, {{topic}})}

  tbl %>%
    arrange(.dt) %>%
    group_by(.group, .feature) %>%
    mutate(ema = .ema(y_kwi, ratio = ratio)) %>%
    ungroup() %>%
    nest(data = -.dt) %>%
    mutate(data = map(data,
                      ~add_blow(.x, .group, .feature, ema,
                                .topic, .prior, .compare,
                                .k_prior, .alpha_prior))) %>%
    unnest(data)

}

#' @export
plot_rolling_blow <- function(tbl, date,
                              group, group_filter,
                              feature, feature_filter) {

  tbl$.dt <- pull(tbl, {{date}})
  tbl$.group <- pull(tbl, {{group}})
  tbl$.feature <- pull(tbl, {{feature}})

  tbl %>%
    filter(.group == group_filter,
           .feature == feature_filter) %>%
    ggplot(aes(.dt, zeta)) +
    geom_rect(xmin = -Inf, xmax = Inf, ymin = -1.96, ymax = 1.96, fill = 'grey75') +
    geom_line() +
    ggthemes::theme_tufte() +
    labs(x = NULL)

}

.ema <- function (x, window = NULL, ratio = NULL) {

  if (is.null(window) & is.null(ratio)) stop("Please select either window or ratio")

  if (!is.null(window)) ratio <- 2 / (window + 1)

  c(stats::filter(x = x * ratio, filter = 1 - ratio, method = "recursive", init = x[1]))

}

