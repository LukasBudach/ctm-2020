# Title     : Preprocessing functions
# Objective : Provide functions that are used to preprocess documents and create a TDM

library(tm)


stem_speech <- function(x) {
  library(SnowballC)
  words <- strsplit(x, ' ')[[1]]
  words <- wordStem(words, language='german')
  return(paste(words, collapse=' '))
}


get_preprocessed_corpus <- function(dataset, stem_speeches=FALSE, remove_numbers=TRUE) {
  corpus <- VCorpus(VectorSource(dataset$Speech), readerControl=list(language='ger'))
  if (remove_numbers) {
    corpus <- tm_map(corpus, removeNumbers) # REMOVE NUMBERS
  }
  corpus <- tm_map(corpus, stripWhitespace) # REMOVE EXTRA WHITE SPACE
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern="[^a-zA-Z0-9äöüÄÖÜß ]", replacement='')
  if (stem_speeches) {
    corpus <- tm_map(corpus, content_transformer(stem_speech))
  } else {
    corpus <- tm_map(corpus, content_transformer(tolower)) # MAKES EVERYTHING LOWERCASE
  }
  return(corpus)
}


get_frequency_matrix <- function(dataset, stem_speeches=FALSE, sparse=0.999) {
  corpus <- get_preprocessed_corpus(dataset, stem_speeches=stem_speeches)
  freq_mat <- TermDocumentMatrix(corpus, control=list(tolower=FALSE))
  sparce_mat <- removeSparseTerms(freq_mat, sparse)
  freq <- as.matrix(sparce_mat)
  colnames(freq) <- dataset$ID
  return(freq)
}
