# Title     : Wordscores I/O
# Objective : Implement all data input/output functions required for the Wordscores model

source('src/utils/io.R')


read_reference_speeches <- function(use_rounded_scores=FALSE) {
  library(readr)
  col_names <- c('SpeechDbId', 'CoalScore')
  cols <- cols(SpeechDbId=col_integer(), CoalScore=col_double())
  if (use_rounded_scores) {
    filepath <- 'data/scored_extremes_rounded.csv'
  } else {
    filepath <- 'data/scored_extremes.csv'
  }

  return(read_csv(filepath, skip=1, col_names=col_names, col_types=cols))
}


