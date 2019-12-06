convert_ratio <- function(x, top = 3) {

  crossing(num = 1:50, denom = 1:50) %>%
    filter(num < denom) %>%
    mutate(fraction = str_glue("{num}/{denom}"),
           decimal = num / denom,
           ratio = str_glue("{num}:{denom - num}"),
           delta = decimal - x) %>%
    arrange(abs(delta),
            num) %>%
    slice(1:top) %>%
    select(fraction, decimal, ratio, delta)

}
