# Title     : Filtering functions
# Objective : Provide functions that filter a given dataset by some metric
# Created by: lukas
# Created on: 22/05/2020


filter <- function(dataset, mode, min_period=NULL, max_period=NULL, threshold=NULL) {
  # perform input argument validation:
  required_colnames <- c('Index', 'SpeechDbId', 'Date', 'Period', 'Sitting', 'DocDbId', 'Speaker', 'Party',
                         'InterjectionCount', 'InterjectionContent', 'ParagraphCount', 'Speech')
  valid_modes <- c('p', 'cc', 'cp', 'co')

  # validate dataset
  if (! is.data.frame(dataset)) {
    stop('ERROR The given dataset is not a data.frame object, which is required.')
  } else if (ncol(dataset) != 12) {
    stop('ERROR The given dataset does not have 12 columns as required.')
  } else if (! all(colnames(dataset) == required_colnames)) {
    stop('ERROR The given dataset does not have the required column names.')
  }

  # validate given mode
  if (typeof(mode) != 'character') {
    stop('ERROR The given mode argument is not a character vector, which is required.')
  } else if (! mode %in% valid_modes) {
    stop('ERROR The given mode argument is not a supported mode.')
  } else if ((mode == 'p') & ((is.null(min_period)) | (is.null(max_period)))) {
    stop('ERROR When filtering by period, the minimal and maximal period arguments must be set.')
  } else if ((mode == 'cc') && (is.null(threshold))) {
    stop('ERROR When filtering by coal count, the threshold argument must be set.')
  } else if ((mode == 'cp') && (is.null(threshold))) {
    stop('ERROR When filtering by coal percentage, the threshold argument must be set.')
  }

  # do the filtering
  filtered_dataset <- NULL
  switch(mode,
         p={  # filter by period
           filtered_dataset <- filter_period_range_(dataset, min_period, max_period)
         },
         cc={ # filter by count of words containing Kohle or kohle per speech
           filtered_dataset <- filter_coal_count_(dataset, threshold)
         },
         cp={ # filter by percentage of words containing Kohle or kohle per speech
           filtered_dataset <- filter_coal_percentage_(dataset, threshold)
         }
  )

  # check whether the filtered dataset still has the valid shape, which is same as input dataset in this case
  if (! is.data.frame(filtered_dataset)) {
    stop('ERROR The filtered dataset is not a data.frame object, which is required.')
  } else if (ncol(filtered_dataset) != 12) {
    stop('ERROR The filtered dataset does not have 12 columns as required.')
  } else if (! all(colnames(filtered_dataset) == required_colnames)) {
    stop('ERROR The filtered dataset does not have the required column names.')
  }

  return(filtered_dataset)
}


# filtering function definitions
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

# utility function definition
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