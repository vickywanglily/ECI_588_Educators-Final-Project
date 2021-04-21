install.packages("LDAvis")
install.packages("ldatuning")
install.packages("rtweet")
install.packages("readr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("tweenr")
install.packages("ggplot2")
install.packages("snowballC")
install.packages("topicmodels")
install.packages("stm")
install.packages("knitr")
install.packages("writexl")
library(writexl)
library(rtweet)
library(writecsv)
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(tweenr)
library(dataedu)
library(here)
library(SnowballC)
library(topicmodels)
library(stm)
library(ldatuning)
library(knitr)
library(LDAvis)

app_name <- "Everything about PBL"
api_key <- "GqKgXfxbFohbyw9fIXtWytYl9"
api_secret_key <- "0wguZQ8KHAhD3wygcPf6MQv1V4V7M1aGdyUeX9qOxIjOEYxSpm"
access_token <- "998560800833974272-ETD2pS4TZSvDoiQ86lCwqDit2OaunYv"
access_token_secret <- "8t368pvMd2jdpjX1Ad5Uf3jEb26zI5wLFX7qN0NUgSSEN"

token<-create_token(
  app=app_name,
  consumer_key = api_key,
  consumer_secret=api_secret_key,
  access_token=access_token,
  access_secret=access_token_secret)

get_token()

pbl_tweets<-search_tweets(q="#PBL",n=200)

pbl_non_retweets<-search_tweets("#PBL",n=200,include_rts=FALSE)
glimpse(pbl_non_retweets)

pbl_non_retweets<-search_tweets2(c(q="#PBL or pbl",'"project_based learning"',
                                   '"project based learning"','"PBLchat"',
                                   '"project-based inquiry"',
                                   '"PBI"','"pbi"'),
                                 n=200, include_rts=FALSE)

pbl_dictionary<-c("#PBL or pbl",'"project_based learning"',
                  '"project based learning"','"PBLchat"',
                  '"project-based inquiry"',
                  '"PBI"','"pbi"')
glimpse(pbl_dictionary)
pbl_tweets<-search_tweets2(pbl_dictionary,n=200,include_rts=FALSE)

write_xlsx(pbl_tweets,"data/pbl_tweets_1.xlsx")
pbl_tweets<-read_csv("data/pbl_tweets_1.csv", 
                     col_types = cols(status_id=col_character(),
                                      user_id=col_character())
                     )

pbl_tweets<-filter(pbl_tweets,lang=="en")
pbl_text<-select(pbl_tweets,status_id,screen_name,user_id, source, created_at,text)
pbl_data<-pbl_text

tweets_tidy<-pbl_data %>%
  unnest_tokens(output = word,input= text) %>%
  anti_join(stop_words, by= "word")

tweets_tidy %>%
  count(word, sort = TRUE)
tweets_quotes <- pbl_data %>%
  filter(grepl("power|engage", text)) %>%
  sample_n(10)

tweets_dtm <- tweets_tidy %>%
  count(user_id, word) %>%
  cast_dtm(user_id, word, n)

temp <- textProcessor(pbl_data$text,
                      metadata = pbl_data,
                      lowercase=TRUE,
                      removestopwords=TRUE,
                      removenumbers=TRUE,
                      removepunctuation=TRUE,
                      wordLengths=c(3,Inf),
                      stem=TRUE,
                      onlycharacter= FALSE,
                      striphtml=TRUE,
                      customstopwords=NULL)

meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents

stemmed_tweets <- pbl_data %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word))

n_distinct(pbl_data$source)

k_metrics <- FindTopicsNumber(
  tweets_dtm,
  topics = seq(10, 75, by = 5),
  metrics = "Griffiths2004",
  method = "Gibbs",
  control = list(),
  mc.cores = NA,
  return_models = FALSE,
  verbose = FALSE,
  libpath = NULL)

FindTopicsNumber_plot(k_metrics)

tweets_lda <- LDA(tweets_dtm,
                  k = 14,
                  control = list(seed = 588))


tweets_stm <- stm(documents=docs,
                  data=meta,
                  vocab=vocab, 
                  prevalence =~ user_id + status_id,
                  K=14,
                  max.em.its=5,
                  verbose = FALSE)

plot.STM(tweets_stm,n=5)

toLDAvis(mod = tweets_stm, docs = docs)
terms(tweets_lda, 5)
tidy_lda <- tidy(tweets_lda)
top_terms <- tidy_lda %>%
  group_by(topic) %>%
  slice_max(beta, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 5 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")

td_beta <- tidy(tweets_lda)
td_gamma <- tidy(tweets_lda, matrix = "gamma")

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>%
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3,
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))

plot(tweets_stm, n = 7)
pbl_data_reduced <-pbl_data$text[-temp$docs.removed]

findThoughts(tweets_stm,
             texts = pbl_data_reduced,
             topics = 13,
             n = 10,
             thresh = 0.5)
