#' Bind the weighted log odds to a tidy dataset
#'
#' Calculate and bind posterior log odds ratios, assuming a
#' multinomial model with a Dirichlet prior. The Dirichlet prior
#' parameters are set using an empirical Bayes approach by default,
#' but an uninformative prior is also available. Assumes that data
#' is in a tidy format, and adds the weighted log odds ratio
#' as a column. Supports non-standard evaluation through the
#' tidyeval framework.
#'
#' @param tbl A tidy dataset with one row per `feature` and `set`
#' @param set Column of sets between which to compare features, such as
#' documents for text data
#' @param feature Column of features for identifying differences, such as words or
#' bigrams with text data
#' @param n Column containing feature-set counts
#' @param .alpha If `NULL` uses empirical prior otherwise this is prior for every feature (uninformative)
#' @param .equation Whether to compare to dataset (equation 15) or other sets (equation 16)
#' @param .unweighted Whether or not to return the unweighted log odds,
#'   in addition to the weighted log odds. Defaults to `TRUE`.
#'
#' @details The arguments \code{set}, \code{feature}, and \code{n}
#' are passed by expression and support \link[rlang]{quasiquotation};
#' you can unquote strings and symbols. Grouping is preserved but ignored.
#'
#'
#' The dataset must have exactly one row per set-feature combination for
#' this calculation to succeed. Read Monroe, Colaresi, and Quinn (2017) for
#' more on the weighted log odds ratio.
#'
#' @source <https://doi.org/10.1093/pan/mpn018>
#'
#' @importFrom rlang enquo as_name is_empty sym
#' @importFrom dplyr count left_join mutate rename group_by ungroup group_vars
#' @export

bind_frech_lo <- function(tbl, set, feature, n,
                          .alpha = NULL,
                          .equation = 15,
                          .unweighted = TRUE) {
  set <- enquo(set)
  feature <- enquo(feature)
  n <- enquo(n)
  grouping <- group_vars(tbl)
  tbl <- ungroup(tbl)

  tbl <-tbl %>%
    add_count(!!feature, wt = !!n, name = "alpha") %>% # count of each word
    mutate(n_other = alpha - n)                        # count of word in group i

  if (!is.null(.alpha)) {tbl$alpha <- .alpha}

  if (.equation == 15) {

    tbl <- tbl %>%
      mutate(y_wi = !!n + alpha) %>%                 # pseudo count of word w in group i
      add_count(phrase, wt = y_wi, name = "y_w") %>% # total pseudo count of word w
      add_count(!!set, wt = y_wi, name = "n_i") %>%  # pseudo count of all words in group i
      mutate(
        omega_wi = y_wi / (n_i - y_wi),              # odds in group i
        omega_w = y_w / (sum(y_wi) - y_w),           # overall odds
        delta_wi = log(omega_wi) - log(omega_w),     # eqn 15,
        sigma2_wi = 1 / y_wi + 1 / y_w,              # eqn 18
        zeta_wi = delta_wi / sqrt(sigma2_wi)         # eqn 21
      ) %>%
      rename(log_odds_weighted = zeta_wi,
             log_odds = delta_wi) %>%
      select(!!set, !!feature, !!n,
             log_odds, log_odds_weighted)

  } else if (.equation == 16) {

    tbl <- tbl %>%
      mutate(y_wi = !!n + alpha,
             y_wj = n_other + alpha) %>%            # pseudo count of word w in group i
      add_count(!!set, wt = y_wi, name = "n_i") %>% # pseudo count of all words in group i
      add_count(!!set, wt = y_wj, name = "n_j") %>% # pseudo count of all words in group j
      mutate(
        omega_wi = y_wi / (n_i - y_wi),             # odds in group i
        omega_wj = y_wj / (n_j - y_wj),             # odds in group j
        delta_wi = log(omega_wi) - log(omega_wj),   # eqn 16,
        sigma2_wi = 1 / y_wi + 1 / y_wj,            # eqn 20
        zeta_wi = delta_wi / sqrt(sigma2_wi)        # eqn 22
      ) %>%
      rename(log_odds_weighted = zeta_wi,
             log_odds = delta_wi) %>%
      select(!!set, !!feature, !!n,
             log_odds, log_odds_weighted)

  } else {
    stop("Please choose equation 15 or 16")
  }

  if(.unweighted) {tbl$log_odds <- NULL}

  if (!is_empty(grouping)) {
    tbl <- group_by(tbl, !!sym(grouping))
  }

  return(tbl)

}
