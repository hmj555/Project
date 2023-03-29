# TM final 201822037 통계학과 한민주 R code

# 셀레니움 환경조성(명령창)
# cd C:\selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

# 패키지
library(httr)
library(rvest)
library(tidytext)
library(RSelenium)
library(dplyr)
library(stringr)
library(textclean)
library(KoNLP)
library(scales)
library(ggplot2)
library(readr)
library(tidyr)
library(widyr)
library(tidygraph)
library(ggraph)
library(showtext)
library(topicmodels)
library(ldatuning)
library(ggwordcloud)

# 디렉토리 지정(생략)

# 웹 크롤링
driver <- rsDriver(browser=c("chrome"), chromever="108.0.5359.71")
remote_driver <- driver[["client"]]
remote_driver$open()
remote_driver$navigate("https://www.youtube.com/watch?v=Jod8j5PHBw8")

# 재생 
btn = remote_driver$findElement(using = "css selector" , 
                                value = '.html5-main-video')
btn$clickElement()

# 스크롤

scroll = function(start, end){
  remote_driver$executeScript(paste0("window.scrollTo(",start,",",end,")"))
}

scroll(2500,10000) ; scroll(10000,20000)
scroll(20000,25000) ; scroll(25000,50000)
scroll(50000,100000) ; scroll(100000,5000000)
scroll(500000,1000000) ; scroll(1000000,1500000)
scroll(1500000,2000000) ; scroll(2000000,2500000)
scroll(2500000,3000000) ; scroll(3000000,3500000) 

# 댓글 가져오기
res <- remote_driver$getPageSource() %>% `[[`(1)
html = res %>% read_html()
comment1 = html %>% html_nodes('#content-text') %>% html_text()

## (크롤링-> 댓글) 함수 만들기
crawling = function(url){
  # url로 이동
  remote_driver$navigate(url)
  # 스크롤
  scroll(2500,10000) ; scroll(15000,20000)
  scroll(20000,25000) ; scroll(25000,30000)
  scroll(30000,35000) ; scroll(30000,40000)
  scroll(40000,45000) ; scroll(45000,50000)
  scroll(50000,55000) ; scroll(55000,60000)
  scroll(60000,65000) ; scroll(65000,70000)
  # 댓글 가져오기
  res <- remote_driver$getPageSource() %>% `[[`(1)
  html = res %>% read_html()
  comment = html %>% html_nodes('#content-text') %>% html_text()
  return(comment)
}

# comment2
comment2 = crawling("https://www.youtube.com/watch?v=i6r8c45cfQU" , "comment2")
# comment3 
comment3 = crawling("https://www.youtube.com/watch?v=L9Noe94JS3I" , "comment3")
# comment4 
comment4 = crawling("https://www.youtube.com/watch?v=alGJ-iCQsX8" , "comment4")
# comment5 
comment5= crawling("https://www.youtube.com/watch?v=h08k0PqjV3w" , "comment5")
# comment6 
comment6 = crawling("https://www.youtube.com/watch?v=433HGrqXk_k" , "comment6")
# comment7 
comment7 = crawling("https://www.youtube.com/watch?v=ZBaZJJIMxIY" , "comment7")

comment_ = c(comment1,comment2,comment3,comment4,comment5,comment6,comment7)

# csv 파일로 내보내기
write.csv(comment_ , 
          file = "C:/Users/user/Downloads/comment.csv")

#########################################################################

# data 불러오기
raw_comment = read.csv("C:/Users/user/Downloads/comment.csv")

names(raw_comment) = c("id" , "reply")
glimpse(raw_comment)

# 전처리
comment = raw_comment %>% 
  filter(str_count(reply, " ") >= 1) %>%                   # 띄어쓰기 1개 이상 추출
  mutate(reply_raw = str_squish(replace_html(reply)),      # 원문 보유
         reply = str_replace_all(reply, "[^가-힣]", " "),  # 한글만 남기기
         reply = str_squish(reply))                        # 중복 공백 제거


# 명사 기준 토큰화
word_noun <- comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = extractNoun,
                drop = F)

# 단어 빈도 구하기
frequency <- word_noun %>%
  count(word, sort = T) %>%    # 단어 빈도 구해 내림차순 정렬
  filter(str_count(word) > 1)  # 두 글자 이상만 남기기

# 상위 단어 추출
frequency %>%  head(40) 

# 불용어 목록 생성
stopword_noun <- c("진짜", "해서", "이거", "하나", "해","하게")

# 유의어 처리
frequency  = frequency %>% mutate(word = ifelse(str_detect(word , "박주") , "박주비" ,word))

# 폰트
font_add_google(name = "Black Han Sans", family = "blackhansans")
showtext_auto()

# 1) 워드 클라우드
ggplot(frequency,
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

# 2) 주요 단어 목록 만들기
top20_noun <- frequency %>%
  filter(!word %in% stopword_noun) %>%
  head(20)

top20_noun 

# 글씨체
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()

ggplot(top20_noun, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "orange") +
  coord_flip() +
  geom_text(aes(label = comma(n, accuracy = 1)), hjust = -0.3) +  
  scale_y_continuous(limits = c(0, 470)) +
  
  labs(title = "장원영 영상 댓글 주요 단어",
       subtitle = "언급 빈도 Top 20",
       x = NULL) +
  
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic", size = 12),
        plot.title = element_text(size = 14, face = "bold"),      # 제목 폰트
        plot.subtitle = element_text(size = 13))                  # 부제목 폰트


# 3) 감정분석
dic <- read_csv("C:/Users/user/Downloads/knu_sentiment_lexicon.csv") # 감정사전

glimpse(comment)

# 감정 점수 부여
word_comment <- word_noun %>%
  mutate(word = ifelse(str_detect(word , "박주") , "박주비" ,word)) %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

word_comment %>%
  select(word, polarity)

# 감정 분류 
word_comment <- word_comment %>%
  mutate(sentiment = ifelse(polarity ==  2, "pos",
                            ifelse(polarity == -2, "neg", "neu")))

word_comment %>% count(sentiment)

# 긍정,부정 단어 top10
top10_sentiment <- word_comment %>%
  filter(sentiment != "neu") %>%
  filter(!word %in% stopword_noun) %>%
  filter(str_count(word) > 1) %>%
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10)

top10_sentiment

ggplot(top10_sentiment, aes(x = reorder(word, n), 
                            y = n, 
                            fill = sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +  
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))

# 중립 단어 중 긍정/부정 단어 추가 
top_neu <- word_comment %>%
  filter(sentiment == "neu") %>%
  count(sentiment, word) %>%
  filter(str_count(word) > 1) %>%
  slice_max(n, n = 40)

top_neu # 중립 단어 확인

word_comment %>%
  filter(str_detect(reply, "표정")) %>%
  select(reply)

word_comment %>%
  filter(str_detect(reply, "무대")) %>%
  select(reply)

word_comment %>%
  filter(str_detect(reply, "언니")) %>%
  select(reply)

word_comment %>%
  filter(str_detect(reply, "중국인")) %>%
  select(reply)

word_comment %>%
  filter(str_detect(reply, "쉴드")) %>%
  select(reply)

word_comment %>%
  filter(str_detect(reply, "논란")) %>%
  select(reply)

new_dic <- dic %>%
  mutate(polarity = ifelse(word %in% "쉴드", -2, polarity))

new_dic= new_dic %>% add_row(word = c("중국인", "나락", "논란","표정", "무대", "언니", "응원")
                         , polarity = c(-2,-2,-2,2,2,2,2))

new_dic= new_dic %>% add_row(word = c("노력", "대단","박주비","중국","재능")
                             , polarity = c(2,2,-2,-2,2))

new_dic %>% filter(word %in% c("중국인", "나락", "쉴드", "논란"))
new_dic %>% filter(word %in% c("표정", "무대", "언니", "응원"))
new_dic %>% filter(word %in% c("노력", "대단","박주비","중국","재능"))

# 감정 점수 부여
word_comment <- word_noun %>%
  mutate(word = ifelse(str_detect(word , "박주") , "박주비" ,word)) %>%
  mutate(word = ifelse(str_detect(word , "중국") , "중국인" ,word)) %>%
  left_join(new_dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

# 감정 분류 
word_comment <- word_comment %>%
  mutate(sentiment = ifelse(polarity ==  2, "pos",
                            ifelse(polarity == -2, "neg", "neu")))

word_comment %>% count(sentiment)

# 긍정,부정 단어 top10
top10_sentiment <- word_comment %>%
  filter(sentiment != "neu") %>%
  filter(!word %in% stopword_noun) %>%
  filter(str_count(word) > 1) %>%
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10)

top10_sentiment

ggplot(top10_sentiment, aes(x = reorder(word, n), 
                            y = n, 
                            fill = sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +  
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))


# 4) 댓글(문장)별 감정 점수
score_comment <- word_comment %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

score_comment %>% 
  select(score, reply)

# 긍정 댓글
score_comment %>%
  select(score, reply) %>% 
  arrange(-score) 

# 부정 댓글
score_comment %>%
  select(score, reply) %>% 
  arrange(score) %>% pull() %>% head()


# 감정 댓글 비율
frequency_score <- score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)

frequency_score

# 막대 그래프 만들기
ggplot(frequency_score, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) + 
  scale_x_discrete(limits = c("pos", "neu", "neg"))


# 누적막대 비율
frequency_score$dummy <- 0
frequency_score

ggplot(frequency_score, aes(x = dummy, y = ratio, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = paste0(round(ratio, 1), "%")),      
            position = position_stack(vjust = 0.5)) + 
  theme(axis.title.x = element_blank(),  # x축 이름 삭제
        axis.text.x  = element_blank(),  # x축 값 삭제
        axis.ticks.x = element_blank())  # x축 눈금 삭제


comment <- score_comment %>%
  unnest_tokens(input = reply,          # 단어 기준 토큰화
                output = word,
                token = "words",
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") &  # 한글 추출
           str_count(word) >= 2)          # 두 글자 이상 추출

# 감정 및 단어별 빈도 구하기
frequency_word <- comment %>%
  filter(str_count(word) >= 2) %>%
  mutate(word = ifelse(str_detect(word , "중국") , "중국인" ,word)) %>%
  mutate(word = ifelse(str_detect(word , "논란") , "논란" ,word)) %>%
  mutate(word = ifelse(str_detect(word , "언니") , "언니" ,word)) %>%
  count(sentiment, word, sort = T)

# 긍정 댓글 고빈도 단어
frequency_word %>%
  filter(sentiment == "pos")

# 부정 댓글 고빈도 단어
frequency_word %>%
  filter(sentiment == "neg")

comment_wide <- frequency_word %>%
  filter(sentiment != "neu") %>%  # 중립 제외
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))

comment_wide

# 5) 로그 오즈비 구하기
comment_wide <- comment_wide %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                ((neg + 1) / (sum(neg + 1)))))

comment_wide

top10 <- comment_wide %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

top10 

# 로그 오즈비 막대 그래프 만들기
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))


# 댓글 원문
score_comment %>%
  filter(score > 0 & str_detect(reply, "무대")) %>%
  select(reply) %>% pull() %>% head()

score_comment %>%
  filter(score<0 & str_detect(reply, "겁나")) %>%
  select(reply) %>% pull() %>% head()

### 6) 의미망 분석

# 전처리
comment <- raw_comment %>%
  select(reply) %>%
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply),
         id = row_number())

# 형태소 기준 토큰화
comment_pos <- comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = SimplePos22, 
                drop = F)

# 품사별 행 분리
comment_pos <- comment_pos %>%
  separate_rows(word, sep = "[+]")

# 명사 추출
noun <- comment_pos %>%
  filter(str_detect(word, "/n")) %>%
  mutate(word = str_remove(word, "/.*$"))

# 동사, 형용사 추출
pvpa <- comment_pos %>%
  filter(str_detect(word, "/pv|/pa")) %>%         # "/pv", "/pa" 추출
  mutate(word = str_replace(word, "/.*$", "다"))  # "/"로 시작 문자를 "다"로 바꾸기

# 품사 결합
comment <- bind_rows(noun, pvpa) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)

# 최종 품사별 토큰화
comment_new <- comment_pos %>%
  separate_rows(word, sep = "[+]") %>%
  filter(str_detect(word, "/n|/pv|/pa")) %>%
  mutate(word = ifelse(str_detect(word, "/pv|/pa"),
                       str_replace(word, "/.*$", "다"),
                       str_remove(word, "/.*$"))) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)

# 동시 출현 빈도 구하기
pair <- comment %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)
pair

pair %>% filter(item1 == "장원영")
pair %>% filter(item1 == "예쁘다")

graph_comment <- pair %>%
  filter(n >= 15) %>%
  as_tbl_graph()

graph_comment # 12개 노드 32개 엣지

# 네트워크 그래프
# 함수화
word_network <- function(x) {
  ggraph(x, layout = "fr") +
    geom_edge_link(color = "gray50",
                   alpha = 0.5) +
    geom_node_point(color = "lightcoral",
                    size = 5) +
    geom_node_text(aes(label = name),
                   repel = T,
                   size = 5,
                   family = "nanumgothic") +
    theme_graph()
}

set.seed(1234)
word_network(graph_comment)

# 유의어 처리하기
comment <- comment %>%
  mutate(word = ifelse(str_detect(word, "장원영") ,"원영", word))

# 단어 동시 출현 빈도 구하기
pair <- comment %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)

# 연결중심성/커뮤니티
set.seed(1234)
graph_comment <- pair %>%
  filter(n >= 18) %>%
  as_tbl_graph(directed = F) %>%        # 방향성 제거
  mutate(centrality = centrality_degree(),        # 연결 중심성
         group = as.factor(group_infomap()))      # 커뮤니티

graph_comment

set.seed(1234)
ggraph(graph_comment, layout = "fr") +      # 레이아웃
  
  geom_edge_link(color = "gray50",          # 엣지 색깔
                 alpha = 0.5) +             # 엣지 명암
  
  geom_node_point(aes(size = centrality,    # 노드 크기
                      color = group),       # 노드 색깔
                  show.legend = F) +        # 범례 삭제
  scale_size(range = c(5, 15)) +            # 노드 크기 범위
  
  geom_node_text(aes(label = name),         # 텍스트 표시
                 repel = T,                 # 노드밖 표시
                 size = 5,                  # 텍스트 크기
                 family = "nanumgothic") +  # 폰트
  
  theme_graph()                             # 배경 삭제


graph_comment %>%
  filter(name == "원영") # 1번 커뮤니티로 분류

# 1번 커뮤니티 - 원영이 예쁘다
graph_comment %>%
  filter(group == 1) %>%
  arrange(-centrality) %>%
  data.frame()

graph_comment %>%
  arrange(-centrality)

# 2번 커뮤니티 - 진짜 예쁘다
graph_comment %>%
  filter(group == 2) %>%
  arrange(-centrality) %>%
  data.frame()

# 단어쌍이 사용된 댓글 원문 추출
comment %>%
  filter(str_detect(reply, "원영") & str_detect(reply, "예쁘다")) %>%
  select(reply)

comment %>%
  filter(str_detect(reply, "표정") & str_detect(reply, "좋다")) %>%
  select(reply)

# 파이계수
word_cors <- comment %>%
  add_count(word) %>%
  filter(n >= 20) %>%
  pairwise_cor(item = word,
               feature = id,
               sort = T)

word_cors

# 상대적으로 자주 함께 사용된 단어
word_cors %>% 
  filter(item1 == "아이브")

word_cors %>% 
  filter(item1 == "쉴드")

# 관심 단어 목록 생성
target <- c("공부", "아이브", "사랑", "중국인", "언니")

top_cors <- word_cors %>%
  filter(item1 %in% target) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 8)

# 그래프 순서 정하기
top_cors$item1 <- factor(top_cors$item1, levels = target)

# 파이계수로 막대그래프
ggplot(top_cors, aes(x = reorder_within(item2, correlation, item1),
                     y = correlation,
                     fill = item1)) +
  geom_col(show.legend = F) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))

set.seed(1234)
graph_cors <- word_cors %>%
  filter(correlation >= 0.15) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(), #연결중심성
         group = as.factor(group_infomap())) # 커뮤니티

# 네트워크 그래프
set.seed(1234)
ggraph(graph_cors, layout = "fr") +
  
  geom_edge_link(color = "gray50",
                 aes(edge_alpha = correlation,   # 엣지 명암
                     edge_width = correlation),  # 엣지 두께
                 show.legend = F) +              # 범례 삭제
  scale_edge_width(range = c(1, 4)) +            # 엣지 두께 범위
  
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = F) +
  scale_size(range = c(5, 10)) +
  
  geom_node_text(aes(label = name),
                 repel = T,
                 size = 5,
                 family = "nanumgothic") +
  
  theme_graph()

## 토픽 모델링
#  전처리
comment_ <- raw_comment %>%
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply)) %>%
  # 중복 댓글 제거
  distinct(reply, .keep_all = T) %>%
  # 짧은 문서 제거 - 3 단어 이상 추출
  filter(str_count(reply, boundary("word")) >= 3)

# 명사 추출
comment <- comment_ %>%
  unnest_tokens(input = reply,
                output = word,
                token = extractNoun,
                drop = F) %>%
  filter(str_count(word) > 1) %>%
  
  # 댓글 내 중복 단어 제거
  group_by(id) %>%
  distinct(word, .keep_all = T) %>%
  ungroup() %>%
  select(id, word)

# 빈도 높은 단어 제거
count_word <- comment %>%
  add_count(word) %>%
  filter(n <= 200) %>%
  select(-n)

# 불용어, 유의어 확인하기
count_word %>%
  count(word, sort = T) %>%
  print(n = 200)

# 불용어 목록 만들기
stopword <- c("들이", "하다", "하게", "하면", "해서", "이번", "하네",
              "해요", "이것", "니들", "하기", "하지", "한거", "해주",
              "그것", "어디", "여기", "까지", "이거", "하신", "만큼",
              "정도", "하나","사실","아이", "누구","때문","뭔가",
              "한거","해보", "가지","하지","한게","언제",
              "한데","해주","돌이","해도","하는거")

# 불용어, 유의어 처리
count_word <- count_word %>%
  filter(!word %in% stopword) %>%
  mutate(word = recode(word,
                       "박주" = "박주비"))

# LDA 모델 만들기
# 문서별 단어 빈도 구하기
count_word_doc <- count_word %>%
  count(id, word, sort = T)

count_word_doc

# DTM 만들기
dtm_comment <- count_word_doc %>%
  cast_dtm(document = id, term = word, value = n)

dtm_comment  # 8285문서, 6347330단어

# 하이퍼 파라미터 튜닝
models <- FindTopicsNumber(dtm = dtm_comment,
                           topics = 2:20,
                           return_models = T,
                           control = list(seed = 1234))

models %>%
  select(topics, Griffiths2004)

FindTopicsNumber_plot(models)

# 토픽 모델 만들기
lda_model <- LDA(dtm_comment,
                 k = 8,
                 method = "Gibbs",
                 control = list(seed = 1234))
lda_model # 8개 토픽

# 모델 내용 확인(단어가 각 토픽에 등장할 확률/문서가 각 토픽에 등장할 확률)
glimpse(lda_model)

term_topic <- tidy(lda_model, matrix = "beta")
term_topic

# 원탑은 토픽1에 등장할 확률이 0.00528로 가장 높다.
term_topic %>%
  filter(term == "원탑" ) 

# 모든 토픽의 주요 단어
terms(lda_model, 15) %>%
  data.frame()

# 토픽별 beta 상위 10개 단어 추출
top_term_topic <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)

# 토픽별 beta기준 막대 그래프
ggplot(top_term_topic,
       aes(x = reorder_within(term, beta, topic),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free", ncol = 4) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(n.breaks = 4,
                     labels = number_format(accuracy = .01)) + 
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))

# 문서별 토픽 확률 gamma 추출
doc_topic <- tidy(lda_model, matrix = "gamma")
doc_topic

# 문서별로 확률이 가장 높은 토픽 추출
doc_class <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

doc_class

# integer로 변환
doc_class$document <- as.integer(doc_class$document)

# 원문에 토픽 번호 부여
comment_topic <- raw_comment %>%
  left_join(doc_class, by = c("id" = "document"))

comment_topic <- comment_topic %>% na.omit()

#토픽별 분류된 문서 개수
comment_topic %>%count(topic)

# 문서를 한 토픽으로만 분류
doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  count(document) %>%
  filter(n >= 2)

set.seed(1234)
doc_class_unique <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  slice_sample(n = 1)

doc_class_unique

# 토픽별 주요 단어 목록
top_terms <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 6, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))

top_terms

# 토픽별 문서 빈도
count_topic <- news_comment_topic %>%
  count(topic) %>% na.omit()

count_topic

# 문서 빈도에 주요 단어 결합
count_topic_word <- count_topic %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic_name = paste("Topic", topic))

count_topic_word

# 막대그래프
ggplot(count_topic_word,
       aes(x = reorder(topic_name, n),
           y = n,
           fill = topic_name)) +
  geom_col(show.legend = F) +
  coord_flip() +
  
  geom_text(aes(label = n) ,                # 문서 빈도 표시
            hjust = -0.2) +                 # 막대 밖에 표시
  
  geom_text(aes(label = term),              # 주요 단어 표시
            hjust = 1.03,                   # 막대 안에 표시
            col = "white",                  # 색깔
            fontface = "bold",              # 두껍게
            family = "nanumgothic") +       # 폰트
  
  scale_y_continuous(expand = c(0, 0),      # y축-막대 간격 줄이기
                     limits = c(0, 820)) +  # y축 범위
  labs(x = NULL)

# 토픽 이름 짓기
comment_topic <-comment_topic %>%
  mutate(reply = str_squish(replace_html(reply))) %>%
  arrange(-gamma)

#토픽 이름 짓기

# 토픽 1~8내용 살펴보기
comment_topic %>%
  filter(topic == 1 & str_detect(reply, "행복")) %>%
  head(50) %>%
  pull(reply)

comment_topic %>%
  filter(topic == 1 & str_detect(reply, "완벽")) %>%
  head(50) %>%
  pull(reply)

comment_topic %>%
  filter(topic == 1 & str_detect(reply, "워녕이")) %>%
  head(50) %>%
  pull(reply)

# 토픽 이름 목록 만들기
name_topic <- tibble(topic = 1:8,
                     name = c("1. 한국인 코스프레, 언론 통제 비판",
                              "2. 장원영 응원, 인성 논란",
                              "3. 박주비 관련 악플, 건강 상태 악플",
                              "4. 행복 기원, 중국인 악플",
                              "5. 외모 칭찬, 노력 칭찬",
                              "6. 잔머리 불호, 무대 칭찬",
                              "7. 아이돌 원탑, 표정 칭찬",
                              "8. 완벽한 아이돌, 인성 논란"))

# 토픽 이름 결합하기
top_term_topic_name <- top_term_topic %>%
  left_join(name_topic, name_topic, by = "topic")

top_term_topic_name

# 막대 그래프 만들기
ggplot(top_term_topic_name,
       aes(x = reorder_within(term, beta, name),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ name, scales = "free", ncol = 2) +
  coord_flip() +
  scale_x_reordered() +
  
  labs(title = "장원영 유튜브 영상 댓글 토픽",
       subtitle = "토픽별 주요 단어 Top 10",
       x = NULL, y = NULL) +
  
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        title = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


