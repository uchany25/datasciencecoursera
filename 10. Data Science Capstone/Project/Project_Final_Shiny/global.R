library(shiny)
library(tibble)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(quanteda)
library(data.table)
library(shinyjs)

#### For rds file

#en_US_blogs <- readLines("en_US.blogs.txt", encoding = "UTF-8") 
#en_US_twitter <- readLines("en_US.twitter.txt", encoding = "UTF-8")
#en_US_news <- readLines("en_US.news.txt", encoding = "UTF-8")
#bad_words <- readLines("en.txt", encoding = "UTF-8")


#set.seed(42)
#sample_size_twitter <- floor(0.05 * length(en_US_twitter))
#en_US_twitter_sampled <- sample(en_US_twitter, sample_size_twitter)

#sample_size_blogs <- floor(0.05 * length(en_US_blogs))
#en_US_blogs_sampled <- sample(en_US_blogs, sample_size_blogs)

#sample_size_news <- floor(0.05 * length(en_US_news))
#en_US_news_sampled <- sample(en_US_news, sample_size_news)


#data_list_for_deploy <- list(
 # blogs = en_US_blogs_sampled,
  #twitter = en_US_twitter_sampled,
  #news = en_US_news_sampled,
  #bad_words = bad_words)

#saveRDS(data_list_for_deploy, "corpus_data_small.rds")


if (file.exists("corpus_data_small.rds")) {
  data_list <- readRDS("corpus_data_small.rds")
  
  en_US_blogs <- data_list$blogs
  en_US_twitter <- data_list$twitter
  en_US_news <- data_list$news
  bad_words <- data_list$bad_words
  
  rm(data_list)
  gc() 
  
} else {
  stop("ERROR: 'corpus_data_small.rds' The file cannot be found. Do the first step task.")
}

####################################################
################ Back-Off Method Functions ###########

# N-gram Create Function (Back-off)
create_ngram_dt <- function(corpus_data, n, words_to_remove) {
  tokens_data <- tokens(corpus_data,
                        what = "word",
                        remove_punct = TRUE,
                        remove_numbers = TRUE,
                        remove_url = TRUE,
                        remove_symbols = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(pattern = words_to_remove)
  
  if (n == 1) {
    dfm_n <- dfm(tokens_data)
    result_dt <- data.table(word1 = featnames(dfm_n), count = as.vector(colSums(dfm_n)))
    setkey(result_dt, word1)
  } else {
    dfm_n <- tokens_ngrams(tokens_data, n = n, concatenator = " ") %>% dfm()
    result_dt <- data.table(ngram = featnames(dfm_n), count = as.vector(colSums(dfm_n)))
    result_dt[, c(paste0("word", 1:n)) := tstrsplit(ngram, " ", fixed = TRUE)]
    result_dt[, ngram := NULL]
    setkeyv(result_dt, head(paste0("word", 1:n), n - 1))
  }
  return(result_dt[order(-count)])
}

# Prediction Function (Back-off Data Table)
predict_word_dt <- function(fragment, candidates, df_list, words_to_remove_pred) {
  
# transform candidates -> word vector
  if (is.character(candidates)) {
    candidates_vec <- unlist(strsplit(candidates, split = ",\\s*"))
    candidates_vec <- trimws(candidates_vec)
    candidates <- candidates_vec[candidates_vec != ""]
  }
  
  if (length(candidates) == 0) {
    return(list(result = tibble(word = "No Candidates", count = 0, level = "N/A", context = ""), level = "No Match"))
  }
  
  clean_fragment <- tolower(fragment)
  clean_fragment <- gsub("[[:punct:]]", " ", clean_fragment)
  words <- unlist(strsplit(clean_fragment, "[[:space:]]+"))
  words <- words[words != ""]
  words <- words[!(words %in% words_to_remove_pred)]
  n_words <- length(words)
  
# Back-off N-gram (6-gram to 2-gram)
  for (n in 6:2) {
    if (n_words >= (n - 1)) {
      context_words <- words[(n_words - (n - 2)):n_words]
      df_n <- df_list[[paste0(n, "g")]]
      context_cols <- paste0("word", 1:(n - 1))
      
      named_context <- as.list(context_words)
      names(named_context) <- context_cols
      
      prediction_n <- df_n[
        named_context,
        on = context_cols,
        nomatch = 0
      ]
      
      if (nrow(prediction_n) > 0) {
        word_n_col <- paste0("word", n)
        
# filtering candidates
        filtered_predictions <- prediction_n[get(word_n_col) %in% candidates]
        
        if (nrow(filtered_predictions) > 0) {
          top_prediction <- filtered_predictions[
            order(-count)
          ][1]
          
          result_df <- tibble(
            word = top_prediction[[word_n_col]],
            count = top_prediction$count,
            level = paste0(n, "-gram"),
            context = paste(context_words, collapse = " ")
          )
          return(list(result = result_df, level = "N-gram Match"))
        }
      }
    }
  }
  
# Unigram (Back-off)
  if (length(df_list[["1g"]]) > 0) {
    prediction_1g <- df_list[["1g"]][word1 %in% candidates]
    if (nrow(prediction_1g) > 0) {
      
      top_prediction <- prediction_1g[
        order(-count)
      ][1]
      
      result_df <- tibble(
        word = top_prediction$word1,
        count = top_prediction$count,
        level = "1-gram",
        context = "None (Most frequent)"
      )
      return(list(result = result_df, level = "1-gram Match"))
    }
  }
  
# Return No Prediction
  return(list(result = tibble(word = "No Prediction", count = 0, level = "N/A", context = ""), level = "No Match"))
}

####################################################
################ Kneser_Ney Method Functions #########

# N-gram Create Function (Kneser_Ney)
create_ngram_kn_dt <- function(corpus_data, n, words_to_remove) {
  tokens_data <- tokens(corpus_data,
                        what = "word",
                        remove_punct = TRUE,
                        remove_numbers = TRUE,
                        remove_url = TRUE,
                        remove_symbols = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(pattern = words_to_remove)
  
  if (n == 1) {
    dfm_n <- dfm(tokens_data)
    result_dt <- data.table(word1 = featnames(dfm_n), count = as.vector(colSums(dfm_n)))
    result_dt[, c("kn_term") := count]
    setkey(result_dt, word1)
    
  } else {
    tokens_n <- tokens_ngrams(tokens_data, n = n, concatenator = " ")
    dt <- data.table(ngram = unlist(tokens_n))
    dt[, c(paste0("word", 1:n)) := tstrsplit(ngram, " ", fixed = TRUE)]
    
    group_cols <- paste0("word", 1:n)
    counts <- dt[, .N, by = group_cols]
    
    setnames(counts, "N", "count")
    counts[, kn_term := count]
    result_dt <- counts
    
    setkeyv(result_dt, head(paste0("word", 1:n), n - 1))
  }
  return(result_dt[order(-count)])
}

# Prediction Function (Kneser-Ney)
predict_word_kn <- function(fragment, candidates, df_list, words_to_remove_pred) {
  
# transform candidates -> word vector
  if (is.character(candidates)) {
    candidates_vec <- unlist(strsplit(candidates, split = ",\\s*"))
    candidates_vec <- trimws(candidates_vec)
    candidates <- candidates_vec[candidates_vec != ""]
  }
  
  if (length(candidates) == 0) {
    return(list(result = tibble(word = "No Candidates", count = 0, level = "N/A", context = ""), level = "No Match"))
  }
  
  clean_fragment <- tolower(fragment)
  clean_fragment <- gsub("[[:punct:]]", " ", clean_fragment)
  words <- unlist(strsplit(clean_fragment, "[[:space:]]+"))
  words <- words[words != ""]
  words <- words[!(words %in% words_to_remove_pred)]
  n_words <- length(words)
  
# Kneser-Ney N-gram (6-gram to 2-gram)
  for (n in 6:2) {
    if (n_words >= (n - 1)) {
      context_words <- words[(n_words - (n - 2)):n_words]
      df_n <- df_list[[paste0(n, "g")]]
      context_cols <- paste0("word", 1:(n - 1))
      
      named_context <- as.list(context_words)
      names(named_context) <- context_cols
      
      prediction_n <- df_n[
        named_context,
        on = context_cols,
        nomatch = 0
      ]
      
      if (nrow(prediction_n) > 0) {
        word_n_col <- paste0("word", n)
        filtered_predictions <- prediction_n[get(word_n_col) %in% candidates] 
        
        if (nrow(filtered_predictions) > 0) {
          top_prediction <- filtered_predictions[
            order(-kn_term)
          ][1]
          
          result_df <- tibble(
            word = top_prediction[[word_n_col]], 
            count = top_prediction$kn_term,
            level = paste0("KN ", n, "-gram"),
            context = paste(context_words, collapse = " ")
          )
          
          return(list(result = result_df, level = "N-gram Match"))
        }
      }
    }
  }
  
# KN Unigram
  if (length(df_list[["1g"]]) > 0) {
    prediction_1g <- df_list[["1g"]][word1 %in% candidates]
    
    if (nrow(prediction_1g) > 0) {
      
      top_prediction <- prediction_1g[
        order(-kn_term)
      ][1]
      
      result_df <- tibble(
        word = top_prediction$word1, 
        count = top_prediction$kn_term,
        level = "KN 1-gram (P_cont)",
        context = "None (Smoothed Unigram)"
      )
      
      return(list(result = result_df, level = "1-gram Match"))
    }
  }
  
  # No prediction (Kneser-Ney)
  return(list(result = tibble(word = "No Prediction", count = 0, level = "N/A", context = ""), level = "No Match"))
}

onStop(function() {
  gc()
})