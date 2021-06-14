library(readr)
raw_speeches <- read_csv("speeches_presidents.csv")

library(dplyr)
library(stringr)
speeches <- raw_speeches %>%
    filter(president %in% c("이명박", "노무현")) %>%
    mutate(value = str_replace_all(value, "[^가-???]", " "),
           value = str_squish(value))    
speeches



library(tidytext)
library(KoNLP)
useNIADic()
speeches <- speeches %>%
    unnest_tokens(input = value,
                  output = word,
                  token = extractNoun)
frequency <- speeches %>%
    count(president, word) %>%
    filter(str_count(word) > 1)
frequency



library(tidyr)
frequency_wide <- frequency %>%
    pivot_wider(names_from = president,
                values_from = n,
                values_fill = list(n = 0))
frequency_wide


frequency_wide <- frequency_wide %>%
    mutate(log_odds_ratio = log(((이명박 + 1) / (sum(이명박 + 1))) /
                                    ((노무현 + 1) / (sum(노무현 + 1)))))


top10 <- frequency_wide %>%
    group_by(president = ifelse(log_odds_ratio > 0, "lee", "roh")) %>%
    slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

top10



theme_set(theme_gray(base_family = "AppleGothic"))


library(ggplot2)
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = president)) +
    geom_col() +
    coord_flip () +
    labs(x = NULL)



raw_address <- read_csv("inaugural_address.csv")

address <- raw_address %>%
    mutate(value = str_replace_all(value, "[^가-???]", " "),
           value = str_squish(value))    


address <- address %>%
    unnest_tokens(input = value,
                  output = word,
                  token = extractNoun)
frequency <- address %>%
    count(president, word) %>%
    filter(str_count(word) > 1)
frequency


frequency_wide <- frequency %>%
    pivot_wider(names_from = president,
                values_from = n,
                values_fill = list(n = 0))
frequency_wide


frequency_wide <- frequency_wide %>%
    mutate(ratio_roh = ((노무현)/(sum(노무현))),
           ratio_moon = ((문재인)/(sum(문재인))),
           ratio_park = ((박근혜)/(sum(박근혜))),
           ratio_lee = ((이명박)/(sum(이명박))))
frequency_wide

frequency_wide <- frequency_wide %>%
    mutate(ratio_roh  = ((노무현 + 1)/(sum(노무현 + 1))), 
           ratio_moon  = ((문재인 + 1)/(sum(문재인 + 1))),
           ratio_park  = ((박근혜 + 1)/(sum(박근혜 + 1))),
           ratio_lee  = ((이명박 + 1)/(sum(이명박 + 1)))) 
frequency_wide


frequency <- frequency %>%
    bind_tf_idf(term = word,           # 단어
                document = president,  # 텍스트 구분 기준
                n = n) %>%             # 단어 빈도
    arrange(-tf_idf)

top10 <- frequency %>%
    group_by(president) %>%
    slice_max(tf_idf, n = 10, with_ties = F)


top10$president <- factor(top10$president,
                          levels = c("문재인", "박근혜", "이명박", "노무현"))
# 막대 그래프 만들기
ggplot(top10, aes(x = reorder_within(word, tf_idf, president),
                  y = tf_idf,
                  fill = president)) +
    geom_col(show.legend = F) +
    coord_flip() +
    facet_wrap(~ president, scales = "free", ncol = 2) +
    scale_x_reordered() +
    labs(x = NULL)

