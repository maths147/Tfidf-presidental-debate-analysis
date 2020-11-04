# libraries ---------------------------------------------------------------

library(tidyverse) # data wrangling
library(tidytext) # tfidf analysis and tokens managment
library(textstem) # presidental database
library(ggwordcloud) # wordclouds
library(hunspell) # dictionary

# functions ---------------------------------------------------------------

theme_text <- function(){
  
  # fixed theme for plotting
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.ticks = element_blank(), #Set axis ticks to on or off
        panel.grid.minor = element_blank(), #Turn the minor grid lines on or off
        legend.title = element_blank(), #Turn the legend title on or off
        legend.position = "none") #Turn the legend on or off
}

# take the data frame with columns doc_id and tokenized_data and count
# tfidf statistic for each doc_id
to_ngrams = function(df, n, min_freq = 5){
  
  df = df %>%
    unnest_tokens(word, tokenized_data, token = "ngrams", n = n) %>%
    count(doc_id, word, sort = TRUE) %>%
    ungroup() %>%
    bind_tf_idf(word, doc_id, n) %>%
    filter(n > min_freq)
  
  return(df)
}

# takes nested data frame and convert it to list of words separated
# with spaces
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

# plotting tfidf score with top words
top_tokens_per_factor_plot = function(data_tfidf, tokens_disp){
  
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
    slice(seq_len(tokens_disp)) %>% # takes top n from every group
    ungroup() %>%
    mutate(row = row_number())
  
  time_words_toplot %>%
    ggplot(aes(row, tf_idf, fill = doc_id)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~doc_id, x[1], x[2], scales = "free_y") +
    xlab(NULL) + ylab(NULL) +
    scale_x_continuous( # managing labels, row helps to keep it in order
      breaks = time_words_toplot$row, 
      labels = time_words_toplot$word
    ) + 
    theme_text() +
    theme(panel.grid.major.x = element_blank()) +
    ggtitle("Top tokens per time") +
    coord_flip()
}

# preparing data ----------------------------------------------------------

# data from textstem package
df = presidential_debates_2012

# how many top tokens to display
tokens_disp = 7

# extracting from column "time" their id number
df = df %>%
  separate(time, into = c("time", "doc_id"), sep = " ") %>%
  select(-time, -tot)

# plotting number of dialogues per time
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

# plotting number of dialogues per person
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

# clearing text
df_tk = df %>%
  mutate(dialogue = tolower(dialogue),
         dialogue = str_replace_all(dialogue, "\\\\n", " "), # removing new line sings
         dialogue = str_replace_all(dialogue, "\\n", " "),
         dialogue = gsub("[^\x01-\x7F]", "", dialogue), # removing special characters
         dialogue = str_remove_all(dialogue, "\\d"), # removing digits
         dialogue = str_replace_all(dialogue, "[[:punct:]]", " "), # removing punctuation
         dialogue = gsub("[ |\t|\r]+", " ", dialogue)) %>% # removing tabulators
  unnest_tokens(token, dialogue) 

df_tk = df_tk %>% 
  anti_join(stop_words, by = c("token" = "word")) %>% # removing stop words
  filter(nchar(token) > 2, # taking words with more than 2 characters
         !token %in% c("don", "bob", "frank", "barack"), # removing undiserable words
         hunspell_check(token)) # dictionary checking

# nesting tokens by doc_id (time)
df_tk_nested = df_tk %>%
  select(doc_id, token) %>%
  nest(data = token)

# nesting tokens by person speaking
df_tk_person_nested = df_tk %>%
  select(doc_id = person, token) %>%
  nest(data = token)

# 1-grams  ----------------------------------------------------------------

# prepare basic table to analysis
words = prepare(df_tk_nested)
persons = prepare(df_tk_person_nested)

# split to tokens
time_words_tk_tfidf = to_ngrams(words, 1, min_freq = 1)

# plot wordcloud with top tokens per doc_id
time_words_tk_tfidf %>%
  group_by(doc_id) %>%
  arrange(desc(n)) %>%
  slice(seq_len(tokens_disp)) %>%
  ggplot(aes(label = word, size = n, color = word)) +
  geom_text_wordcloud() + 
  scale_size_area(max_size = 10) +
  facet_wrap(~doc_id) +
  theme_minimal()

# plot wordcloud with top tfidf tokens per doc_id
time_words_tk_tfidf %>%
  group_by(doc_id) %>%
  arrange(desc(tf_idf)) %>%
  slice(seq_len(tokens_disp0)) %>%
  ggplot(aes(label = word, size = tf_idf, color = word)) +
  geom_text_wordcloud() + 
  scale_size_area(max_size = 10) +
  facet_wrap(~doc_id) +
  theme_minimal()

# plot with top tfidf tokens per doc_id
top_tokens_per_factor_plot(time_words_tk_tfidf, tokens_disp)

# split to tokens
person_words_tk_tfidf = to_ngrams(persons, 1, min_freq = 1)

# plot wordcloud with top tokens per person
person_words_tk_tfidf %>%
  group_by(doc_id) %>%
  arrange(desc(n)) %>%
  slice(seq_len(tokens_disp)) %>%
  ggplot(aes(label = word, size = n, color = word)) +
  geom_text_wordcloud() + 
  scale_size_area(max_size = 10) +
  facet_wrap(~doc_id) +
  theme_minimal()

# plot wordcloud with top tfidf tokens per person
person_words_tk_tfidf %>%
  group_by(doc_id) %>%
  arrange(desc(tf_idf)) %>%
  slice(seq_len(tokens_disp)) %>%
  ggplot(aes(label = word, size = tf_idf, color = word)) +
  geom_text_wordcloud() + 
  scale_size_area(max_size = 10) +
  facet_wrap(~doc_id) +
  theme_minimal()

# plot with top tfidf tokens per person
top_tokens_per_factor_plot(person_words_tk_tfidf, tokens_disp)

# 2-grams -----------------------------------------------------------------

time_2grams_tk_tfidf = to_ngrams(words, 2, min_freq = 1)

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

top_tokens_per_factor_plot(time_2grams_tk_tfidf, tokens_disp)

person_2grams_tk_tfidf = to_ngrams(persons, 2, min_freq = 1)

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

top_tokens_per_factor_plot(person_2grams_tk_tfidf, tokens_disp)

