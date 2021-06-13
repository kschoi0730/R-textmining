raw_park <- readLines("speech_park.txt", encoding = "UTF-8")
head(raw_park)


library(dplyr)
library(stringr)

park <- raw_park %>%
    str_replace_all("[^가-???]", " ") %>%
    str_squish() %>%
    as_tibble()

library(tidytext)

word_park <- park %>%
    unnest_tokens(input = value,
                  output = word,
                  token = "words")



top20_park <- word_park %>%
    count(word, sort = T) %>%
    filter(str_count(word)>1) %>%
    head(20)

library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()


library(ggplot2)
ggplot(top20_park, aes(x = reorder(word, n), y = n)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = n), hjust = -0.3) +            # 막대 밖 빈도 표시
    labs(title = "박근혜 대통령 출마 연설문 단어 빈도",  # 그래프 제목
         x = NULL, y = NULL) +                           # 축 이름 삭제
    theme(title = element_text(family = "nanumgothic", size = 12))               # 제목 크기



