install.packages("multilinguer")
library(multilinguer)
install_jdk()


install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"),
                 type = "binary")


install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP",
                        upgrade = "never",
                        INSTALL_opts = c("--no-multiarch"))
library(KoNLP)

useNIADic()


library(dplyr)
text <- tibble(
    value = c("대한민국은 민주공화국이다.",
              "대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다."))
text
extractNoun(text$value)

library(tidytext)
text %>%
    unnest_tokens(input = value,        # 분석 대상
                  output = word,        # 출력 변수명
                  token = extractNoun)  # 토큰화 함수

raw_moon <- readLines("speech_moon.txt", encoding = "UTF-8")
library(stringr)
install.packages("textclean")
library(textclean)
moon <- raw_moon %>%
    str_replace_all("[^가-???]", " ") %>%  # 한글만 남기기
    str_squish() %>%                      # 중복 공백 제거
    as_tibble()                           # tibble로 변환
moon


word_noun <- moon %>%
    unnest_tokens(input = value,
                  output = word,
                  token = extractNoun)
word_noun


word_noun <- word_noun %>%
    count(word, sort = T) %>%    # 단어 빈도 구해 내림차순 정렬
    filter(str_count(word) > 1)  # 두 글자 이상만 남기기
word_noun


moon %>%
    unnest_tokens(input = value,
                  output = word,
                  token = extractNoun) %>%
    count(word, sort = T) %>%
    filter(str_count(word) > 1)


top20 <- word_noun %>%
    head(20)


library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()


library(ggplot2)
ggplot(top20, aes(x = reorder(word, n), y = n)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = n), hjust = -0.3) +
    labs(x = NULL) +
    theme(text = element_text(family = "nanumgothic"))



# 폰트 설정
library(showtext)
font_add_google(name = "Black Han Sans", family = "blackhansans")
showtext_auto()
library(ggwordcloud)
ggplot(word_noun, aes(label = word, size = n, col = n)) +
    geom_text_wordcloud(seed = 1234, family = "blackhansans") +
    scale_radius(limits = c(3, NA),
                 range = c(3, 15)) +
    scale_color_gradient(low = "#66aaf2", high = "#004EA1") +
    theme_minimal()



sentences_moon <- raw_moon %>%
    str_squish() %>%
    as_tibble() %>%
    unnest_tokens(input = value,
                  output = sentence,
                  token = "sentences")
sentences_moon



sentences_moon %>%
    filter(str_detect(sentence, "국민"))


sentences_moon %>%
    filter(str_detect(sentence, "일자리"))
