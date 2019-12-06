#' Remove stop words from bigrams
#'
#' @name remove_bigram_stopwords
#' @param df A tibble containing bigrams
#' @param bigram Column of bigrams
#' @param char_only Remove numbers
#' @export

remove_bigram_stopwords <- function(df, bigrams, char_only = TRUE) {

  df <- df %>%
    tidyr::separate({{bigrams}}, c("word1", "word2"),
                    sep = " ", extra = "drop", fill = "right") %>%
    dplyr::filter(!word1 %in% stop_words$word,
                  !word2 %in% stop_words$word,
                  !is.na(word2)) %>%
    tidyr::unite({{bigrams}}, word1, word2, sep = " ")

  if(char_only) {

    df <- df %>%
      dplyr::filter(!stringr::str_detect({{bigrams}}, "\\d"))

  }

  df

}

