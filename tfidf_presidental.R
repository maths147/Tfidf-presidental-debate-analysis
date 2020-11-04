# libraries ---------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(textstem) #database
library(ggwordcloud)
library(hunspell) #dictionary

# functions ---------------------------------------------------------------

theme_text <- function(){
  
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.ticks = element_blank(), #Set axis ticks to on or off
        panel.grid.minor = element_blank(), #Turn the minor grid lines on or off
        legend.title = element_blank(), #Turn the legend title on or off
        legend.position = "none") #Turn the legend on or off
}

to_ngrams = function(df, n, min_freq = 5){
  
  df = df %>%
    unnest_tokens(word, tokenized_data, token = "ngrams", n = n) %>%
    count(doc_id, word, sort = TRUE) %>%
    ungroup() %>%
    bind_tf_idf(word, doc_id, n)
  
  return(df)
}

prepare = function(df_nested){
  
  tokens = matrix(0, nrow(df_nested), 2)
  colnames(tokens) = c("doc_id", "tokenized_data")
  
  for(i in 1:nrow(df_nested)){
    tokens[i, 1] = as.character(df_nested$doc_id[[i]])
    tokens[i, 2] = paste(as.character(unlist(df_nested$data[[i]])), 
                         collapse = " ")
  }
  
  tokens = data.frame(tokens)
  
  return(tokens)
}

top_tokens_per_factor_plot = function(data_tfidf){
  
  n = nrow(data_tfidf)
  x = c()
  
  if(!(n %% 3))
    x = c(n/3, 3)
  else if(!(n %% 2))
    x = c(n/2, 2)
  else
    x = c(1, n)
  
  time_words_toplot = data_tfidf %>%
    arrange(desc(tf_idf)) %>%
    group_by(doc_id) %>%
    slice(seq_len(8)) %>%
    ungroup() %>%
    mutate(row = row_number())
  
  time_words_toplot %>%
    ggplot(aes(row, tf_idf, fill = doc_id)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~doc_id, x[1], x[2], scales = "free_y") +
    xlab(NULL) + ylab(NULL) +
    scale_x_continuous(
      breaks = time_words_toplot$row,
      labels = time_words_toplot$word
    ) + 
    theme_text() +
    theme(panel.grid.major.x = element_blank()) +
    ggtitle("Top tokens per time") +
    coord_flip()
}

# preparing data ----------------------------------------------------------

df = presidential_debates_2012

tokens_disp = 7

df = df %>%
  separate(time, into = c("time", "doc_id"), sep = " ") %>%
  select(-time, -tot)

df %>%
  count(doc_id) %>%
  mutate(doc_id = as.factor(doc_id)) %>%
  ggplot(aes(doc_id, n, fill = doc_id)) +
  geom_col() +
  geom_text(aes(doc_id, n, label = n), vjust = -0.3) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect("black", fill = NA)) +
  labs(y = "Number of dialogues", x = "Time") +
  ggtitle("Number of dialogues per time")

df %>%
  count(person) %>%
  ggplot(aes(person, n, fill = person)) +
  geom_col() +
  geom_text(aes(person, n, label = n), vjust = -0.3) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect("black", fill = NA)) +
  labs(y = "Number of dialogues", x = "Person") +
  ggtitle("Number of dialogues per person")
  
df_tk = df %>%
  mutate(dialogue = tolower(dialogue),
         dialogue = str_replace_all(dialogue, "\\\\n", " "),
         dialogue = str_replace_all(dialogue, "\\n", " "),
         dialogue = gsub("[^\x01-\x7F]", "", dialogue),
         dialogue = str_remove_all(dialogue, "\\d"),
         dialogue = str_replace_all(dialogue, "[[:punct:]]", " "),
         dialogue = gsub("[ |\t|\r]+", " ", dialogue)) %>%
  unnest_tokens(token, dialogue)

df_tk = df_tk %>% 
  anti_join(stop_words, by = c("token" = "word")) %>%
  filter(nchar(token) > 2,
         !token %in% c("don", "bob", "frank", "barack"),
         hunspell_check(token))

df_tk_nested = df_tk %>%
  select(doc_id, token) %>%
  nest(data = token)

df_tk_person_nested = df_tk %>%
  select(doc_id = person, token) %>%
  nest(data = token)

# 1-grams  ----------------------------------------------------------------

time_words = prepare(df_tk_nested)

time_words_tk_tfidf = to_ngrams(time_words, 1, min_freq = 1)

time_words_tk_tfidf %>%
  group_by(doc_id) %>%
  arrange(desc(n)) %>%
  slice(seq_len(tokens_disp)) %>%
  ggplot(aes(label = word, size = n, color = word)) +
  geom_text_wordcloud() + 
  scale_size_area(max_size = 10) +
  facet_wrap(~doc_id) +
  theme_minimal()

time_words_tk_tfidf %>%
  group_by(doc_id) %>%
  arrange(desc(tf_idf)) %>%
  slice(seq_len(tokens_disp0)) %>%
  ggplot(aes(label = word, size = tf_idf, color = word)) +
  geom_text_wordcloud() + 
  scale_size_area(max_size = 10) +
  facet_wrap(~doc_id) +
  theme_minimal()

top_tokens_per_factor_plot(time_words_tk_tfidf)

person_words = prepare(df_tk_person_nested)

person_words_tk_tfidf = to_ngrams(person_words, 1, min_freq = 1)

person_words_tk_tfidf %>%
  group_by(doc_id) %>%
  arrange(desc(n)) %>%
  slice(seq_len(tokens_disp)) %>%
  ggplot(aes(label = word, size = n, color = word)) +
  geom_text_wordcloud() + 
  scale_size_area(max_size = 10) +
  facet_wrap(~doc_id) +
  theme_minimal()

person_words_tk_tfidf %>%
  group_by(doc_id) %>%
  arrange(desc(tf_idf)) %>%
  slice(seq_len(tokens_disp)) %>%
  ggplot(aes(label = word, size = tf_idf, color = word)) +
  geom_text_wordcloud() + 
  scale_size_area(max_size = 10) +
  facet_wrap(~doc_id) +
  theme_minimal()

top_tokens_per_factor_plot(person_words_tk_tfidf)

# 2-grams -----------------------------------------------------------------

time_2grams = prepare(df_tk_nested)

time_2grams_tk_tfidf = to_ngrams(time_words, 2, min_freq = 1)

time_2grams_tk_tfidf %>%
  group_by(doc_id) %>%
  arrange(desc(n)) %>%
  slice(seq_len(tokens_disp)) %>%
  ggplot(aes(label = word, size = n, color = word)) +
  geom_text_wordcloud() + 
  scale_size_area(max_size = 10) +
  facet_wrap(~doc_id) +
  theme_minimal()

time_2grams_tk_tfidf %>%
  group_by(doc_id) %>%
  arrange(desc(tf_idf)) %>%
  slice(seq_len(tokens_disp)) %>%
  ggplot(aes(label = word, size = tf_idf, color = word)) +
  geom_text_wordcloud() + 
  scale_size_area(max_size = 10) +
  facet_wrap(~doc_id) +
  theme_minimal()

top_tokens_per_factor_plot(time_2grams_tk_tfidf)

person_2grams = prepare(df_tk_person_nested)

person_2grams_tk_tfidf = to_ngrams(person_2grams, 2, min_freq = 1)

person_2grams_tk_tfidf %>%
  group_by(doc_id) %>%
  arrange(desc(n)) %>%
  slice(seq_len(tokens_disp)) %>%
  ggplot(aes(label = word, size = n, color = word)) +
  geom_text_wordcloud() + 
  scale_size_area(max_size = 10) +
  facet_wrap(~doc_id) +
  theme_minimal()

person_2grams_tk_tfidf %>%
  group_by(doc_id) %>%
  arrange(desc(tf_idf)) %>%
  slice(seq_len(tokens_disp)) %>%
  ggplot(aes(label = word, size = tf_idf, color = word)) +
  geom_text_wordcloud() + 
  scale_size_area(max_size = 10) +
  facet_wrap(~doc_id) +
  theme_minimal()

top_tokens_per_factor_plot(person_2grams_tk_tfidf)

