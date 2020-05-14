# Title     : Preprocessing Functions
# Objective : One-Stop-Shop, execute to have all functions you might need for preprocessing
# Created by: lukas
# Created on: 14.05.2020

read_speeches <- function(filepath) {
  library(readr)
  col_names <- c('Index', 'SpeechDbId', 'Date', 'Period', 'Sitting', 'DocDbId', 'Speaker', 'Party', 'InterjectionCount', 'InterjectionContent', 'ParagraphCount', 'Speech')
  cols <- cols(Index=col_integer(),
               SpeechDbId=col_integer(),
               Date=col_date(),
               Period=col_integer(),
               Sitting=col_integer(),
               DocDbId=col_integer(),
               Speaker=col_character(),
               Party=col_character(),
               InterjectionCount=col_integer(),
               InterjectionContent=col_character(),
               ParagraphCount=col_integer(),
               Speech=col_character())
    return(read_csv(filepath, skip=1, col_names=col_names, col_types=cols))
}

filter_period <- function(dataset, period) {
  return(dataset[dataset$Period == period,])
}

get_frequency_matrix <- function(dataset, sparse=0.999) {
  library(tm)
  corpus <- VCorpus(VectorSource(dataset$Speech))
  corpus <- tm_map(corpus, content_transformer(tolower)) # MAKES EVERYTHING LOWERCASE
  corpus <- tm_map(corpus, removeNumbers) # REMOVE NUMBERS
  corpus <- tm_map(corpus, stripWhitespace) # REMOVE EXTRA WHITE SPACE
  freq_mat <- TermDocumentMatrix(corpus)
  sparce_mat <- removeSparseTerms(freq_mat, sparse)
  freq <- as.matrix(sparce_mat)
  colnames(freq) <- dataset$SpeechDbId
  return(freq)
}

serialize_results_text <- function(filepath, obj) {
  f <- file(filepath, 'w+')
  serialize(connection=f, object=obj, ascii=TRUE)
  close(f)
}

unserialize_results_text <- function(filepath) {
  f <- file(filepath, 'r')
  ret <- unserialize(f)
  close(f)
  return(ret)
}
