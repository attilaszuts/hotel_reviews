library(readr)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(textdata)
library(igraph)
library(ggraph)
library(widyr)
df <- read_csv('hotel_reviews.csv') %>% clean_names()

skimr::skim(df)
str(df)

# main features that can be used for text analysis
##  negative_review
##  positive_review
##  review_word_count
##  reviewer_score

# interesting features about the data
##  days_since_review
##  reviewer_nationality
##  total_number_of_reviews
##  total_number_of_reviews_reviewer_has_given


# TODO: subset for only UK, Australia, Ireland, Canada and US!!! 
df %>% 
  group_by(hotel_name) %>% 
  count() %>% 
  arrange(desc(n))

df %>% 
  group_by(reviewer_nationality) %>% 
  count() %>% 
  arrange(desc(n))

nationalities <- c("United Kingdom", "United States of America", "Australia", "Ireland", "Canada")
df <- df %>% 
  filter(reviewer_nationality %in% nationalities)

# Clean data
df_cleaned <- df %>% 
  select(hotel_name, total_number_of_reviews, 
         reviewer_nationality, total_number_of_reviews_reviewer_has_given, reviewer_score,
         negative_review, positive_review) %>% 
  mutate(review_id = seq(1, length(df$hotel_address), 1)) %>% 
  pivot_longer(c(negative_review, positive_review), names_to = "review_type", values_to = "review") %>% 
  # drop reviews that are missing ("No Positive" or "No Negative")
  mutate(review_type = gsub("_review$", "", review_type),
         review = ifelse(review == "No Positive" | review == "No Negative", "<NULL>", review)) %>% 
  filter(review != "<NULL>") %>% 
  drop_na()

df_cleaned <- df_cleaned[1:1000,]

hotel_words <- df_cleaned %>% 
  unnest_tokens(word, review) %>% 
  count(hotel_name, word, sort = TRUE) %>% 
  ungroup() 

total_words <- hotel_words %>% 
  group_by(hotel_name) %>% 
  summarize(total = sum(n))

hotel_words <- left_join(hotel_words, total_words,)            

ggplot(hotel_words, aes(n/total, fill = hotel_name)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~hotel_name, ncol = 2, scales = "free_y")

freq_by_rank <- hotel_words %>%
  group_by(hotel_name) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = hotel_name)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()


rank_subset <- freq_by_rank %>%
  filter(rank < 500,
         rank > 10)
lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

hotel_words <- hotel_words %>% 
  bind_tf_idf(word, hotel_name, n)

hotel_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))


hotel_words %>%
  # anti_join(stop_words, by = "word") %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(hotel_name) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = hotel_name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~hotel_name, ncol = 2, scales = "free") +
  coord_flip()


# bigrams -----------------------------------------------------------------

bigrams_separated <- df_cleaned %>% 
  unnest_tokens(bigram, review, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") 

bigrams_filtered <- bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)


bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
  count(hotel_name, bigram) %>%
  bind_tf_idf(bigram, hotel_name, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf %>% 
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
  group_by(hotel_name) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill = hotel_name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~hotel_name, ncol = 2, scales = "free") +
  coord_flip()

AFINN <- get_sentiments("afinn") %>% rename(score = "value")
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

negation_words <- c("not", "no", "never", "without")
negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()

negated_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip() + 
  facet_wrap(~word1, ncol = 2, scales = "free_y")


# trigrams ----------------------------------------------------------------

# maybe even three?
trigrams_separated <- df_cleaned %>% 
  unnest_tokens(bigram, review, token = "ngrams", n = 4) %>% 
  separate(bigram, c("word1", "word2", "word3", "word4"), sep = " ") 

trigrams_separated %>% 
  filter(word1 == "not") %>% 
  count(word1, word2, word3, word4, sort = TRUE)

trigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


# graph visualization -----------------------------------------------------

bigram_graph <- bigram_counts %>%
  drop_na() %>% 
  filter(n > 5) %>%
  graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# functions ---------------------------------------------------------------

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}
visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}


# pairwise correlation ----------------------------------------------------

hotel_section_words <- df_cleaned %>%
  filter(hotel_name == "Hotel Arena") %>%
  rename(section = "review_id") %>%
  filter(section > 0) %>%
  unnest_tokens(review, text) %>%
  filter(!review %in% stop_words$word)

word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)
word_pairs
