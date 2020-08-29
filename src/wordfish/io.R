# Title     : Wordfish I/O
# Objective : Implement all data input/output functions required for the Wordfish model

source('src/utils/io.R')


read_scored_extremes <- function(filepath) {
  library(readr)
  col_names <- c('SpeechDbId', 'CoalScore')
  cols <- cols(SpeechDbId=col_integer(), CoalScore=col_double())
  return(read_csv(filepath, skip=1, col_names=col_names, col_types=cols))
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
