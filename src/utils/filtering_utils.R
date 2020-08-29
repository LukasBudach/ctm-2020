# Title     : Filter utilities
# Objective : Provides utility functions for filter functions


get_coal_count_ <- function(dataset) {
  library(stringr)

  # get the counts of words containing Kohle or kohle in each of the given dataset's speeches
  cts <- data.frame()
  for(i in seq(1, nrow(dataset))) {
    cts <- rbind(cts, list(dataset$SpeechDbId[i], sapply(strsplit(dataset$Speech[i], " "), length),
                           str_count(dataset$Speech[i], '(K|k)ohle')))
  }
  colnames(cts) <- c('SpeechDbId', 'WordCount', 'CoalCount')
  return(cts)
}


get_extended_coal_positions_ <- function(dataset, row_idx, chars_around) {
  library(stringr)

  positions <- str_locate_all(dataset$Speech[row_idx], '(K|k)ohle')[[1]]
  positions[,'start'] <- positions[,'start'] - chars_around
  positions[,'end'] <- positions[,'end'] + chars_around
  return(positions)
}


extend_positions_borders_ <- function(dataset, row_idx, positions) {
  for (i in seq(1, nrow(positions))) {
      positions[i, 'start'] <- max(positions[i, 'start'], 0)
      positions[i, 'end'] <- min(positions[i, 'end'], nchar(dataset$Speech[row_idx]))

      while (substring(dataset$Speech[row_idx], positions[i, 'start'], positions[i, 'start']) != ' ') {
        if (positions[i, 'start'] == 0) {
          break
        }
        positions[i, 'start'] <- positions[i, 'start'] - 1
      }
      while (substring(dataset$Speech[row_idx], positions[i, 'end'], positions[i, 'end']) != ' ') {
        if (positions[i, 'end'] == nchar(dataset$Speech[row_idx])) {
          break
        }
        positions[i, 'end'] <- positions[i, 'end'] + 1
      }
      if ((positions[i, 'start'] == 0) || (positions[i, 'end'] == nchar(dataset$Speech[row_idx]))) {
        next
      }
      positions[i, 'start'] <- positions[i, 'start'] + 1
      positions[i, 'end'] <- positions[i, 'end'] - 1
    }
  return(positions)
}


merge_overlapping_position_segments_ <- function(positions) {
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
  return(positions)
}


get_presidents_map <- function() {
  library(hash)
  return(hash('Dr. Erich Köhler'=c(1), 'Dr. Hermann Ehlers'=c(1, 2), 'Dr. Eugen Gerstenmaier'=c(2, 3, 4, 5),
              'Kai-Uwe Hassel'=c(5, 6), 'Dr. Annemarie Renger'=c(7), 'Dr. Karl Carstens (Fehmarn)'=c(8),
              'Richard Stücklen'=c(8, 9), 'Dr. Rainer Barzel'=c(10), 'Dr. Philipp Jenninger'=c(10, 11),
              'Dr. Rita Süssmuth'=c(11, 12, 13), 'Dr. h.c. Wolfgang Thierse'=c(14, 15),
              'Dr. Norbert Lammert'=c(16, 17, 18), 'Dr. Wolfgang Schäuble'=c(19)))
}
