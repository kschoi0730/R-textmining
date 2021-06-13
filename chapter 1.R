raw_moon <- readLines("speech_moon.txt", encoding = "UTF-8")
head(raw_moon)


install.packages("stringr")
library(stringr)
moon <- raw_moon %>%
    str_replace_all("[^가-???]", " ")
head(moon)


moon <- moon %>%
    str_squish()
head(moon)

install.packages("dplyr")
library(dplyr)
moon <- as_tibble(moon)
moon


install.packages("tidytext")
library(tidytext)
# 문장 기준 토큰화
text %>%
    unnest_tokens(input = value,        # 토큰화할 텍스트
                  output = word,        # 토큰을 담을 변수명
                  token = "sentences")  # 문장 기준


word_space <- moon %>%
    unnest_tokens(input = value,
                  output = word,
                  token = "words")
word_space

word_space <- word_space %>%
    count(word, sort = T) %>%
    filter(str_count(word) > 1)

top20 <- word_space %>%
    head(20)
top20


install.packages("ggplot2")
library(ggplot2)
ggplot(top20, aes(x = reorder(word, n), y = n)) +  # 단어 빈도순 정렬
    geom_col() +
    coord_flip()                                      # 회전


theme_set(theme_gray(base_family = "AppleGothic"))


ggplot(top20, aes(x = reorder(word, n), y = n)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = n), hjust = -0.3) +            # 막대 밖 빈도 표시
    labs(title = "문재인 대통령 출마 연설문 단어 빈도",  # 그래프 제목
         x = NULL, y = NULL) +                           # 축 이름 삭제
    theme(title = element_text(size = 12))               # 제목 크기


install.packages("ggwordcloud")
library(ggwordcloud)
ggplot(word_space, aes(label = word, size = n)) +
    geom_text_wordcloud(seed = 1234) +
    scale_radius(limits = c(3, NA),     # 최소, 최대 단어 빈도
                 range = c(3, 30))      # 최소, 최대 글자 크기


ggplot(word_space,
       aes(label = word,
           size = n,
           col = n)) +                     # 빈도에 따라 색깔 표현
    geom_text_wordcloud(seed = 1234) +
    scale_radius(limits = c(3, NA),
                 range = c(3, 30)) +
    scale_color_gradient(low = "#66aaf2",     # 최소 빈도 색깔
                         high = "#004EA1") +  # 최고 빈도 색깔
    theme_minimal()                           # 배경 없는 테마 적용


install.packages("showtext")
library(showtext)
install.packages("jsonlite")
install.packages("curl")
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()

ggplot(word_space,
       aes(label = word,
           size = n,
           col = n)) +
    geom_text_wordcloud(seed = 1234,
                        family = "nanumgothic") +  # 폰트 적용
    scale_radius(limits = c(3, NA),
                 range = c(3, 30)) +
    scale_color_gradient(low = "#66aaf2",
                         high = "#004EA1") +
    theme_minimal()



