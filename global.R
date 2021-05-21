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

# Read in data
read_data <- function() {
  df <- read_csv('hotel_reviews.csv') %>% clean_names()
  return(df)
}

# Clean data, and return cleaned dataset
clean_data <- function(df, pos_neg = F) {
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
  
  # TODO: posneg only or unify cols 
  if (pos_neg) {
    df_cleaned
  } else {
    df_cleaned
  }
  return(df_cleaned)
}

# Tokenize cleaned data and add total word count per hotel and per word
tokenize_count <- function(df_cleaned, n = 1) {
  hotel_words <- df_cleaned %>% 
    unnest_tokens(word, review) %>% 
    count(hotel_name, word, sort = TRUE) %>% 
    ungroup() 
  total_words <- hotel_words %>% 
    group_by(hotel_name) %>% 
    summarize(total = sum(n))
  hotel_words <- left_join(hotel_words, total_words)
  return(hotel_words)
}

# Bind term frequency - inverse document frequency to data
tfidf <- function(hotel_words) {
  hotel_words <- hotel_words %>% 
    bind_tf_idf(word, hotel_name, n) %>% 
    select(-total) %>% 
    arrange(desc(tf_idf))
}

