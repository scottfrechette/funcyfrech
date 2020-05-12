#' Bind the weighted log odds to a tidy dataset
#'
#' Calculate and bind the log odds ratio, weighted by an uninformative Dirichlet
#' prior, of a tidy dataset to the dataset itself. The weighted log odds ratio
#' is added as a column. This functions supports non-standard evaluation through
#' the tidyeval framework.
#'
#' @param tbl A tidy dataset with one row per feature and set
#' @param set Column of sets between which to compare features, such as
#' documents for text data
#' @param feature Column of features for identifying differences, such as words or
#' bigrams with text data
#' @param n Column containing feature-set counts
#' @param alpha (Optional) Prior frequency of each feature. Can be single number
#' for all words or dataframe of counts for each feature
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
#' @examples
#'
#' library(dplyr)
#'
#' gear_counts <- mtcars %>%
#'   count(vs, gear)
#'
#' gear_counts
#'
#'
#' @importFrom rlang enquo as_name is_empty sym
#' @importFrom dplyr count left_join mutate rename group_by ungroup group_vars
#' @export

bind_log_odds <- function(tbl, set, feature, n, alpha = NULL) {

  set <- enquo(set)
  feature <- enquo(feature)
  y_col <- enquo(n)
  grouping <- group_vars(tbl)
  tbl <- ungroup(tbl)

  if(is.null(alpha)) {

    n_df <- count(tbl, !!set, wt = !!y_col, name = ".n")
    alpha_df <- count(tbl, !!feature, wt = !!y_col, name = ".alpha")
    df_joined <- tbl %>%
      left_join(n_df, by = rlang::as_name(set)) %>%
      left_join(alpha_df, by = rlang::as_name(feature)) %>%
      mutate(.alpha0 = sum(!!y_col),
             y_other = .alpha - !!y_col,
             n_other = .alpha0 - .n,
             l1 = (!!y_col + .alpha) / (.n + .alpha0 - !!y_col - .alpha),
             l2 = (y_other + .alpha) / (n_other + .alpha0 - y_other - .alpha),
             sigma2 = 1/(!!y_col + .alpha) + 1/(y_other + .alpha),
             log_odds = (log(l1) - log(l2))/sqrt(sigma2))
    tbl$log_odds <- df_joined$log_odds

  } else {

    if(length(alpha) == 1) {

      n_df <- count(tbl, !!set, wt = !!y_col, name = ".n")
      y_total_df <- count(tbl, !!feature, wt = !!y_col, name = "y_total")
      df_joined <- tbl %>%
        left_join(n_df, by = rlang::as_name(set)) %>%
        left_join(y_total_df, by = rlang::as_name(feature)) %>%
        mutate(.alpha = alpha,
               .alpha0 = nrow(distinct(tbl, !!feature)) * alpha,
               y_other = y_total - !!y_col,
               n_other = sum(!!y_col) - .n,
               l1 = (!!y_col + .alpha) / (.n + .alpha0 - !!y_col - .alpha),
               l2 = (y_other + .alpha) / (n_other + .alpha0 - y_other - .alpha),
               sigma2 = 1/(!!y_col + .alpha) + 1/(y_other + .alpha),
               log_odds = (log(l1) - log(l2))/sqrt(sigma2))
      tbl$log_odds <- df_joined$log_odds

    } else if (nrow(alpha) >= nrow(distinct(tbl, {{feature}}))) {

      n_df <- count(tbl, !!set, wt = !!y_col, name = ".n")
      y_total_df <- count(tbl, !!feature, wt = !!y_col, name = "y_total")
      alpha0 <- sum(alpha[,2])
      df_joined <- tbl %>%
        left_join(n_df, by = rlang::as_name(set)) %>%
        left_join(y_total_df, by = rlang::as_name(feature)) %>%
        left_join(alpha, by = rlang::as_name(feature)) %>%
        rename(.alpha = ncol(.)) %>%
        mutate(.alpha0 = alpha0,
               y_other = y_total - !!y_col,
               n_other = sum(!!y_col) - .n,
               l1 = (!!y_col + .alpha) / (.n + .alpha0 - !!y_col - .alpha),
               l2 = (y_other + .alpha) / (n_other + .alpha0),
               sigma2 = 1/(!!y_col + .alpha) + 1/(y_other + .alpha),
               log_odds = (log(l1) - log(l2))/sqrt(sigma2))
      tbl$log_odds <- df_joined$log_odds

    } else {

      message("alpha must be length 1 or feature")

    }

  }

  if (!is_empty(grouping)) {
    tbl <- group_by(tbl, !!sym(grouping))
  }

  tbl

}
