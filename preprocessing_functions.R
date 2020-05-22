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

get_only_coal_segments <- function(dataset, words_around) {
  library(stringr)
  cut_speeches <- dataset
  for (j in seq(1, nrow(dataset))) {
    positions <- str_locate_all(dataset$Speech[j], '(K|k)ohle')[[1]]
    positions[,'start'] <- positions[,'start'] - words_around
    positions[,'end'] <- positions[,'end'] + words_around

    for (i in seq(1, nrow(positions))) {
      positions[i, 'start'] <- max(positions[i, 'start'], 0)
      positions[i, 'end'] <- min(positions[i, 'end'], nchar(dataset$Speech[j]))

      while (substring(dataset$Speech[j], positions[i, 'start'], positions[i, 'start']) != ' ') {
        if (positions[i, 'start'] == 0) {
          break
        }
        positions[i, 'start'] <- positions[i, 'start'] - 1
      }
      while (substring(dataset$Speech[j], positions[i, 'end'], positions[i, 'end']) != ' ') {
        if (positions[i, 'end'] == nchar(dataset$Speech[j])) {
          break
        }
        positions[i, 'end'] <- positions[i, 'end'] + 1
      }
      if ((positions[i, 'start'] == 0) || (positions[i, 'end'] == nchar(dataset$Speech[j]))) {
        next
      }
      positions[i, 'start'] <- positions[i, 'start'] + 1
      positions[i, 'end'] <- positions[i, 'end'] - 1
    }

    found_matches <- TRUE
    while(found_matches){
      valids <- NULL
      found_matches <- FALSE
      if (nrow(positions) == 1) {
        break
      }
      for (i in seq(1, nrow(positions) - 1)) {
        if (positions[i, 'end'] >= positions[i+1, 'start']) {
          found_matches <- TRUE
          positions[i, 'end'] <- positions[i+1, 'end']
          positions[i+1, 'start'] <- 0
          positions[i+1, 'end'] <- 0
        } else if (sum(positions[i]) == 0) {
          valids <- append(valids, FALSE)
          next
        }
        valids <- append(valids, TRUE)
      }
      valids <- append(valids, sum(positions[nrow(positions)]) != 0)
      positions <- matrix(positions[valids,], ncol=2)
      colnames(positions) <- c('start', 'end')
    }

    new_speech <- ''
    for (i in seq(1, nrow(positions))) {
      if (new_speech == '') {
        new_speech <- paste0(new_speech, substr(dataset$Speech[j], positions[i, 'start'], positions[i, 'end']))
      } else {
        new_speech <- paste(new_speech, substr(dataset$Speech[j], positions[i, 'start'], positions[i, 'end']))
      }
    }
    cut_speeches$Speech[j] <- new_speech
  }
  return(cut_speeches)
}

remove_parliament_president<- function(dataset) {
  library(hash)
  # define all the presidents of the Bundestag
  presidents <- hash('Dr. Erich Köhler'=c(1), 'Dr. Hermann Ehlers'=c(1, 2), 'Dr. Eugen Gerstenmaier'=c(2, 3, 4, 5),
                     'Kai-Uwe Hassel'=c(5, 6), 'Dr. Annemarie Renger'=c(7), 'Dr. Karl Carstens (Fehmarn)'=c(8),
                     'Richard Stücklen'=c(8, 9), 'Dr. Rainer Barzel'=c(10), 'Dr. Philipp Jenninger'=c(10, 11),
                     'Dr. Rita Süssmuth'=c(11, 12, 13), 'Dr. h.c. Wolfgang Thierse'=c(14, 15),
                     'Dr. Norbert Lammert'=c(16, 17, 18), 'Dr. Wolfgang Schäuble'=c(19))
  # get only those speeches made by someone that was president at some point
  pres_only_speeches <- dataset[dataset$Speaker %in% keys(presidents),]
  # take only those speeches made while the speaker was actually the president
  actually_president <- data.frame()
  for (i in seq(1, nrow(pres_only_speeches))) {
    if (pres_only_speeches$Period[i] %in% values(presidents, keys=pres_only_speeches$Speaker[i])) {
      actually_president <- rbind(actually_president, pres_only_speeches[i,])
    }
  }
  # remove those speeches from the original dataset and return
  result <- rbind(dataset, actually_president)
  return(result[!duplicated(result,fromLast = FALSE)&!duplicated(result,fromLast = TRUE),])
}
