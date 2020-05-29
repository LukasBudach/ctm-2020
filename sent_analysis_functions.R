# Title     : TODO
# Objective : TODO
# Created by: lukas
# Created on: 29/05/2020

source('preprocessing_functions.R')

get_sentiment_dictionary <- function() {
  library(readr)
  library(stringr)
  library(dplyr)
  library(SnowballC)
  library(SentimentAnalysis)

  # read the SentiWS dictionary
  col_names <- c('Word', 'Score', 'Alt')
  cols <- cols(Word=col_character(),
               Score=col_double(),
               Alt=col_character()
               )
  dict_pos <- read_delim('data/SentiWS_v1.8c_Positive.txt', '	', skip=0, col_names=col_names, col_types=cols)
  dict_neg <- read_delim('data/SentiWS_v1.8c_Negative.txt', '	', skip=0, col_names=col_names, col_types=cols)

  dict_pos$Word <- gsub(dict_pos$Word, pattern="\\|.*", replacement='')
  dict_neg$Word <- gsub(dict_neg$Word, pattern="\\|.*", replacement='')

  dict_new <- dict_pos[, !(names(dict_pos) %in% c('Alt'))]
  dict_new <- rbind(dict_new, dict_neg[, !(names(dict_neg) %in% c('Alt'))])

  alternative_words <- str_split(dict_pos$Alt, ',')
  alternative_words <- append(alternative_words, str_split(dict_neg$Alt, ','))

  it_range <- seq(1, nrow(dict_new))
  for (i in it_range) {
    alt_word_vec <- alternative_words[[i]]
    if (anyNA(alt_word_vec)) {
      next
    }
    for (j in alt_word_vec) {
      dict_new <- rbind(dict_new, list(j, dict_new$Score[i]))
    }
  }
  dict_new <- dict_new %>% arrange(Word)

  # stem the dictionary words and remove duplicates
  dict_new$Word <- wordStem(dict_new$Word, language='german')
  dict_new <- dict_new[!duplicated(dict_new$Word),]

  # return the sentiment dictionary
  return(SentimentDictionaryWeighted(dict_new$Word, dict_new$Score))
}

get_stemmed_speeches <- function(dataset) {
  speeches <- dataset$Speech

  for (i in seq(1, nrow(dataset))) {
    speeches[i] <- stem_speech(dataset$Speech[i])
  }
  return(speeches)
}