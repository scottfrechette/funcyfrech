#' Summarize draws from Stan model
#'
#' @param predicted_draws Tidy output from add_predicted_draws or predicted_draws
#' @param truth Column indicating observed values
#' @param prediction Column indicating predicted values
#' @param robust Whether to use mean or median
#' @export

summarize_predicted_draws <- function(predicted_draws,
                                      truth,
                                      prediction,
                                      robust = F) {

  predicted_draws %>%
    summarize(.truth = {{truth}}[1],
              mean = mean({{prediction}}),
              median = median({{prediction}}),
              sd = sd({{prediction}}),
              mad = mad({{prediction}}, constant = 1),
              l50 = quantile({{prediction}}, 0.25),
              u50 = quantile({{prediction}}, 0.75),
              l80 = quantile({{prediction}}, 0.1),
              u80 = quantile({{prediction}}, 0.9),
              l95 = quantile({{prediction}}, 0.025),
              u95 = quantile({{prediction}}, 0.975),
              .groups = "keep") %>%
    mutate(center = mean * (robust == FALSE) + median * (robust == TRUE),
           scale = sd * (robust == FALSE) + mad * (robust == TRUE),
           error = .truth - center,
           error_scaled = error / scale,
           within50 = between(.truth, l50, u50),
           within80 = between(.truth, l80, u80),
           within95 = between(.truth, l95, u95)) %>%
    ungroup() %>%
    summarize(mae = mean(abs(error)),
              mae_scaled = mean(abs(error_scaled)),
              within50 = mean(within50),
              within80 = mean(within80),
              within95 = mean(within95))

}
