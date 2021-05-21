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
library(wordcloud)
library(RColorBrewer)


# Read in data
nationalities <- c("United Kingdom", "United States of America", "Australia", "Ireland", "Canada")
read_data <- function(sample = T) {
  if (sample) {
    df_raw<- read_csv('hotel_reviews_sample.csv') %>% clean_names() %>% 
      filter(reviewer_nationality %in% nationalities) 
  } else {
    df_raw<- read_csv('hotel_reviews.csv') %>% clean_names() %>% 
      filter(reviewer_nationality %in% nationalities) 
  }
  return(df_raw)
}

# Clean data, and return cleaned dataset
clean_data <- function(df_raw, pos_neg = F) {
  df_cleaned <- df_raw %>%
    select(hotel_name, total_number_of_reviews, 
           reviewer_nationality, total_number_of_reviews_reviewer_has_given, reviewer_score,
           negative_review, positive_review) %>% 
    mutate(review_id = seq(1, length(df_raw$hotel_name), 1)) %>% 
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
  return(hotel_words)
}

plot_wordcloud <- function(hotel_words, input) {
  hotel_words <- filter(hotel_words, hotel_name == input$wordcloud_filter)
  wc <- wordcloud(
    hotel_words$word, 
    hotel_words$tf_idf, 
    min.freq = 1,
    max.words = 50, 
    random.order=FALSE, 
    rot.per=0.35,            
    colors=brewer.pal(8, "Dark2"))
  return(wc)
}


df_cleaned <- read_data(sample = F) %>% clean_data()
hotel_names <- sort(unique(df_cleaned$hotel_name))

# # Execute functions for filter page
# filter_page <- function(params = NULL) {
# 
#   if (!exists('df_raw')) {
#     df_raw <- read_data()
#   }
#   df_cleaned <- clean_data(df_raw)
#   hotel_names <- sort(unique(df_cleaned$hotel_name))
#   return(hotel_names)
# }
# 
# # Execute functions for wordcloud page
# wordcloud_page <- function(params = NULL) {
#   
#   if (!exists('hotel_words')) {
#     df_raw <- read_data()
#     df_cleaned <- clean_data(df_raw)
#     hotel_words <- tokenize_count(df_cleaned)
#     hotel_words <- tfidf(hotel_words)
#   }
#   
#   return()
#   
# }