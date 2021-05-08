library(writexl)
library(rtweet)
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(tweenr)
library(wordcloud2)
library(dataedu)
library(here)
library(SnowballC)
library(topicmodels)
library(widyr)
library(stringr)
library(igraph)
library(ggraph)
library(forcats)
library(stm)
library(ldatuning)
library(knitr)
library(LDAvis)
library(widyr)
library(formattable)
library(data.table)
AFINN <- get_sentiments("afinn")
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

pbl_tweets<-read_csv("c:/Users/Vicky/Documents/data/pbl_tweets_1.csv", 
                     col_types = 
                       cols(status_id=col_character(),user_id=               col_character())
)

pbl_rq<-read_xlsx("c:/Users/Vicky/Documents/data/pbl_rq.xlsx")
formattable(pbl_rq, align =c("l","l","l"))

pbl_tweets<-filter(pbl_tweets,lang=="en")
pbl_text<-select(pbl_tweets,status_id,screen_name,user_id, source, created_at,text)
pbl_data<-pbl_text

tweets_tidy<-pbl_data %>%
  unnest_tokens(output = word,input= text) %>%
  anti_join(stop_words, by= "word") %>%
  filter(!str_detect(word, "[:punct:]|[:digit:]"))%>%
  filter(!grepl("pbl",word)) %>%
  filter(!grepl("project", word)) %>%
  filter(!grepl("amp", word)) %>%
  filter(!grepl("project",word)) %>%
  filter(!grepl("learning",word)) %>%
  filter(!grepl("https",word)) %>%
  filter(!grepl("t.co",word)) %>%
  filter(!grepl("based",word))

wordcloud_count<-tweets_tidy %>%
  count(word,sort=T) %>%
  filter(n>=15) 
wordcloud2(wordcloud_count)

count_tf_idf<-tweets_tidy %>%
  select(status_id,word) %>%
  group_by(status_id) %>%
  count (word, sort=TRUE) %>%
  mutate(total=sum(n),frequency=n/sum(n)) %>%
  bind_tf_idf(word,status_id, n) %>%
  arrange(desc(tf_idf))

count_tf_idf %>% 
  filter(tf_idf>1.0) %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf,  fct_reorder(word, tf_idf), fill = status_id)) +
  geom_col(show.legend = FALSE) +
  labs(title="Top Words Twitter Users Mentioned Related to PBL",
       x = "tf-idf", y = NULL)

tweets_quotes <- pbl_data %>%
  filter(grepl("check|post|connecting", text)) %>%
  sample_n(10)
tweets_quotes

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
  dplyr::mutate(topic = paste0("Topic ", topic),
                topic = reorder(topic, gamma))

gamma_terms %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3,
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))
plot(tweets_stm, n = 7)

sentiment_afinn<-inner_join(tweets_tidy,AFINN,by="word")
senti_count<-sentiment_afinn %>%
  group_by(word) %>%
  count(word,sort=T)%>%
  inner_join(AFINN,by="word")%>%
  mutate(contribution=n*value) 
senti_count %>%
  filter(abs(contribution)>4) %>%
  slice_max(abs(contribution), n = 15) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(contribution, fct_reorder(word, contribution), fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL)

senti_quotes<-pbl_data %>%
  select(text) %>%
  filter(grepl('fun|love|rigorous',text))
sample_n(senti_quotes,10)
neg_quotes<-pbl_data %>%
  select(text) %>%
  filter(grepl('sucks|fake|catastrophe',text))
sample_n(senti_quotes,10)

pbl_bigrams <- pbl_data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

negation_words <- c("not", "no", "never", "without","hardly", "rarely")

bigrams_separated <- pbl_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!str_detect(word1, "[:punct:]|[:digit:]"))%>%
  filter(!grepl("pbl",word1)) %>%
  filter(!grepl("project", word1)) %>%
  filter(!grepl("amp", word1)) %>%
  filter(!grepl("project",word1)) %>%
  filter(!grepl("learning",word1)) %>%
  filter(!grepl("https",word1)) %>%
  filter(!grepl("t.co",word1)) %>%
  filter(!grepl("based",word1)) %>%
  filter(!str_detect(word2, "[:punct:]|[:digit:]"))%>%
  filter(!grepl("pbl",word2)) %>%
  filter(!grepl("project", word2)) %>%
  filter(!grepl("amp", word2)) %>%
  filter(!grepl("project",word2)) %>%
  filter(!grepl("learning",word2)) %>%
  filter(!grepl("https",word2)) %>%
  filter(!grepl("t.co",word2)) %>%
  filter(!grepl("based",word2))

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE) 

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
  count(source, bigram) %>%
  bind_tf_idf(bigram, source, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf %>% 
  filter(tf_idf>.8) %>%
  mutate(word = reorder(bigram, tf_idf)) %>%
  ggplot(aes(tf_idf,fct_reorder(bigram, tf_idf), fill = source)) +
  geom_col(show.legend = FALSE) +
  labs(title="Top Bigram Words Twitter Users Mentioned Related to PBL",
       x = "tf-idf", y = NULL)

bigram_tf_idf %>%
  filter(tf_idf>.8) %>%
  group_by(source) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = source)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, ncol = 3, scales = "free") +
  labs(x = "tf-idf", y = NULL)

pbl_trigrams <- pbl_data %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE) 

negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"negated words\"")

formattable(negated_words)
#1)  First Data Table

formattable(negated_words, 
            align =c("l","l","c","l"), 
            list(`word1` = formatter(
              "span", style = ~ style(color = "#FF9999",
                                      font.weight = "bold")
            ) 

bigram_graph <- bigram_counts %>%
  filter(n > 5) %>%
  graph_from_data_frame()

set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2020)
a <- grid::arrow(type = "closed", length = unit(.12, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

pbl_section_words <- pbl_data %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!str_detect(word, "[:punct:]|[:digit:]"))%>%
  filter(!grepl("pbl",word)) %>%
  filter(!grepl("project", word)) %>%
  filter(!grepl("amp", word)) %>%
  filter(!grepl("project",word)) %>%
  filter(!grepl("learning",word)) %>%
  filter(!grepl("https",word)) %>%
  filter(!grepl("t.co",word)) %>%
  filter(!grepl("based",word))

word_pairs <- pbl_section_words %>%
  pairwise_count(word, section, sort = TRUE)
word_pairs %>%
  filter(item1 == "education")

word_cors <- pbl_section_words %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors %>%
  filter(item1 %in% c("environmental", "exploring", "math", "question","science","resources")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity",fill="#00AEBE") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

set.seed(2016)
word_cors %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
                 
  ![The Need for a High Quality PBL Framework](c:/Users/Vicky/Documents/data/pbl.png){width=80%}
