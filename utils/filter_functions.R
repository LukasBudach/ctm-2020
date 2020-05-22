# Title     : Filtering functions
# Objective : Provide functions that filter a given dataset by some metric
# Created by: lukas
# Created on: 22/05/2020

source('utils/filter_utils.R')

filter_period_range_ <- function(dataset, min, max) {
  # input argument validation
  if (! is.numeric(min)) {
    stop('ERROR The given minimum period argument is not a numerical value.')
  } else if (! is.numeric(max)) {
    stop('ERROR The given maximum period argument is not a numerical value.')
  } else if (min > max) {
    stop('ERROR The given minimum period argument may not be larger than the maximum period argument.')
  }

  # do the filtering
  return(dataset[dataset$Period %in% seq(min, max), ])
}

filter_coal_count_ <- function(dataset, min_count) {
  library(stringr)

  # input argument validation
  if (! is.numeric(min_count)) {
    stop('ERROR The given minimal coal count argument is not a number.')
  }

  coal_count <- get_coal_count_(dataset)
  # filter the counts using the given threshold and return the reduced dataset
  coal_count <- coal_count[coal_count$CoalCount >= min_count,]
  return(dataset[dataset$SpeechDbId %in% coal_count$SpeechDbId,])
}

filter_coal_percentage_ <- function(dataset, min_percentage) {
  library(stringr)

  # input argument validation
  if (! is.numeric(min_percentage)) {
    stop('ERROR The given minimal coal percentage argument is not a number.')
  } else if (min_percentage > 1.0) {
    stop('ERROR The minimal coal percentage argument needs to be smaller than 1.0.')
  }

  coal_count <- get_coal_count_(dataset)
  # filter the percentages using the given threshold and return the reduced dataset
  coal_count <- coal_count[coal_count$CoalCount / coal_count$WordCount >= min_percentage,]
  return(dataset[dataset$SpeechDbId %in% coal_count$SpeechDbId,])
}

filter_coal_only_segments_ <- function(dataset, chars_around) {
  # input argument validation
  if (! is.numeric(chars_around)) {
    stop('ERROR The given number of characters to be kept around each coal word needs to be a numerical value.')
  }

  cut_speeches <- dataset
  for (ridx in seq(1, nrow(dataset))) {
    positions <- get_extended_coal_positions_(dataset, ridx, chars_around)
    positions <- extend_positions_borders_(dataset, ridx, positions)
    positions <- merge_overlapping_position_segments_(positions)

    new_speech <- ''
    for (i in seq(1, nrow(positions))) {
      if (new_speech == '') {
        new_speech <- substr(dataset$Speech[ridx], positions[i, 'start'], positions[i, 'end'])
      } else {
        new_speech <- paste(new_speech, substr(dataset$Speech[ridx], positions[i, 'start'], positions[i, 'end']))
      }
    }
    cut_speeches$Speech[ridx] <- new_speech
  }
  return(cut_speeches)
}

filter_non_coal_segments_ <- function(dataset, chars_around) {
  # input argument validation
  if (! is.numeric(chars_around)) {
    stop('ERROR The given number of characters to be removed around each coal word needs to be a numerical value.')
  }

  cut_speeches <- dataset
  for (ridx in seq(1, nrow(dataset))) {
    positions <- get_extended_coal_positions_(dataset, ridx, chars_around)
    positions <- extend_positions_borders_(dataset, ridx, positions)
    positions <- merge_overlapping_position_segments_(positions)

    new_speech <- ''
    for (i in seq(1, nrow(positions))) {
      # take the speech text that comes before the currently viewed segment
      if (new_speech == '') {
        new_speech <- paste0(new_speech, substr(dataset$Speech[ridx], 0, max(0, positions[i, 'start']-1)))
      } else {
        new_speech <- paste(new_speech, substr(dataset$Speech[ridx], positions[i-1, 'end']+1, positions[i, 'start']-1))
      }
      # make sure, that if we are on the last segment, we also take everything after the coal segment
      if (i == nrow(positions)) {
        new_speech <- paste(new_speech, substr(dataset$Speech[ridx], min(nchar(dataset$Speech[ridx]), positions[i, 'end']+1), nchar(dataset$Speech[ridx])))
      }
    }
    cut_speeches$Speech[ridx] <- new_speech
  }
  return(cut_speeches)
}

filter_non_parliament_president_speaker_<- function(dataset) {
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