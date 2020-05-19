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

filter_period_range <- function(dataset, min, max) {
  return(dataset[dataset$Period %in% seq(min, max), ])
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
  colnames(freq) <- dataset$ID
  return(freq)
}

concat_by_speaker <- function(dataset, multiple_periods=FALSE) {
  if (multiple_periods) {
    dataset$Speaker <- paste0(dataset$Speaker,'_',dataset$Period)
  }
  u_speakers <- unique(dataset$Speaker)
  r_speeches <- data.frame()
  for(speaker in u_speakers) {
    speeches <- dataset$Speech[dataset$Speaker == speaker]
    concat <- ''
    for(speech in speeches) {
      concat <- paste(concat, speech)
    }
    r_speeches <- rbind(r_speeches, list(speaker, concat))
  }
  colnames(r_speeches) <- c('ID', 'Speech')
  return(r_speeches)
}

concat_by_party <- function(dataset, multiple_periods=FALSE) {
  if (multiple_periods) {
    dataset$Party <- paste0(dataset$Party, '_', dataset$Period)
  }
  u_parties <- unique(dataset$Party)
  r_speeches <- data.frame()
  for(party in u_parties) {
    speeches <- dataset$Speech[dataset$Party == party]
    concat <- ''
    for(speech in speeches) {
      concat <- paste(concat, speech)
    }
    r_speeches <- rbind(r_speeches, list(party, concat))
  }
  colnames(r_speeches) <- c('ID', 'Speech')
  return(r_speeches)
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

filter_coal_percentile <- function(dataset, min_percentage) {
  library(stringr)

  cts <- data.frame()
  for(i in seq(1, nrow(dataset))) {
    cts <- rbind(cts, list(dataset$SpeechDbId[i], sapply(strsplit(dataset$Speech[i], " "), length),
                           str_count(dataset$Speech[i], 'K*k*ohle')))
  }
  colnames(cts) <- c('SpeechDbId', 'WordCount', 'CoalCount')

  return(cts[cts$CoalCount / cts$WordCount >= min_percentage,])
}

filter_coal_count <- function(dataset, min_count) {
  library(stringr)

  cts <- data.frame()
  for(i in seq(1, nrow(dataset))) {
    cts <- rbind(cts, list(dataset$SpeechDbId[i], sapply(strsplit(dataset$Speech[i], " "), length),
                           str_count(dataset$Speech[i], 'K*k*ohle')))
  }
  colnames(cts) <- c('SpeechDbId', 'WordCount', 'CoalCount')

  return(cts[cts$CoalCount >= min_count,])
}
