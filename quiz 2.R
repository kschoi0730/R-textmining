raw_park <- readLines("speech_park.txt", encoding = "UTF-8")

library(stringr)
library(textclean)
park <- raw_park %>%
    str_replace_all("[^가-???]", " ") %>%  # 한글만 남기기
    str_squish() %>%                      # 중복 공백 제거
    as_tibble()                           # tibble로 변환
park


word_noun_park <- park %>%
    unnest_tokens(input = value,
                  output = word,
                  token = extractNoun)


word_noun_park <- word_noun_park %>%
    count(word, sort = T) %>%    # 단어 빈도 구해 내림차순 정렬
    filter(str_count(word) > 1)  # 두 글자 이상만 남기기
word_noun_park


park %>%
    unnest_tokens(input = value,
                  output = word,
                  token = extractNoun) %>%
    count(word, sort = T) %>%
    filter(str_count(word) > 1)


top20_park <- word_noun_park %>%
    head(20)


library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()


library(ggplot2)
ggplot(top20_park, aes(x = reorder(word, n), y = n)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = n), hjust = -0.3) +
    labs(x = NULL) +
    theme(text = element_text(family = "nanumgothic"))


sentences_park <- raw_park %>%
    str_squish() %>%
    as_tibble() %>%
    unnest_tokens(input = value,
                  output = sentence,
                  token = "sentences")

sentences_park %>%
    filter(str_detect(sentence, "경제"))
