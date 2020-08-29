# Title     : Filtering functions
# Objective : Provide functions that filter a given dataset by some metric

source('src/utils/filtering_utils.R')


filter_speeches <- function(dataset, mode, min_period=NULL, max_period=NULL, threshold=NULL, chars_around=NULL, min_pct=NULL,
                            max_pct=NULL) {
  # filter attributes:
  #   dataset -> the dataset to be processed
  #   dataset -> must be a data.frame with the columns Index, SpeechDbId, Date, Period, Sitting, DocDbId, Speaker, Party,
  #                                                    InterjectionCount, InterjectionContent, ParagraphCount, Speech
  #   mode -> filtering mode as string, options:
  #           p  -> filter by period, keeping every period starting at the given min_period and ending with the max_period
  #           cc -> filter by coal count, keeping only speeches with a coal count greater or equal to the given threshold
  #           cp -> filter by coal percentage, keeping speeches with a coal percentage greater/equal to the threshold
  #           co -> filter, keeping only parts of speeches that are within the chars_around area around an occurence of coal in the text
  #           nc -> filter, keeping only parts of speeches that are NOT within the chars_around area around an occurence of coal in the text
  #           np -> filter, removing all speeches by the Bundestagspräsident(en) in the dataset
  #           vp -> filter, removing the vocabulary the Bundestagspräsident(en) in the dataset used
  #           sw -> filter, removing the words from the speeches that are more common or less common than the given thresholds
  #
  #   returns the resulting data as data.frame with the same columns as the input (but likely less rows!)

  # perform input argument validation:
  required_colnames <- c('Index', 'SpeechDbId', 'Date', 'Period', 'Sitting', 'DocDbId', 'Speaker', 'Party',
                         'InterjectionCount', 'InterjectionContent', 'ParagraphCount', 'Speech')
  valid_modes <- c('p', 'cc', 'cp', 'co', 'nc', 'np', 'vp', 'sw')

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
  } else if ((mode == 'co') && (is.null(chars_around))) {
    stop('ERROR When filtering to keep the parts of speeches around coal only, the characters to be kept around these words need to be set.')
  } else if ((mode == 'nc') && (is.null(chars_around))) {
    stop('ERROR When filtering to keep the parts of speeches that do not contain a specified area around coal, the characters to be kept need to be set.')
  } else if ((mode == 'sw') && (is.null(min_pct) || is.null(max_pct))) {
    stop('ERROR When filtering to remove common and very uncommon words, the two percentage thresholds need to be set.')
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
         },
         co={ # only keep those parts of each speech, that are in proximity to a word containing Kohle or kohle
           filtered_dataset <- filter_coal_only_segments_(dataset, chars_around)
         },
         nc={ # only keep those parts of each speech, that are not in proximity to a word containing Kohle or kohle
           filtered_dataset <- filter_non_coal_segments_(dataset, chars_around)
         },
         np={ # only keep the speeches made by speakers that were not Bundestagspräsident at the time of the speech
           filtered_dataset <- filter_non_parliament_president_speaker_(dataset)
         },
         vp={ # remove the vocabulary that is used by the Bundestagspräsident(en) in the dataset from all the speeches
           filtered_dataset <- filter_remove_president_vocabulary_(dataset)
         },
         sw={ # remove words that are very common or very uncommon in the documents
           filtered_dataset <- filter_irrelevant_words_(dataset, min_pct, max_pct)
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


filter_remove_president_vocabulary_ <- function(dataset) {
  library(tm)
  library(stringr)

  # define all the presidents of the Bundestag
  presidents <- get_presidents_map()
  # get only those speeches made by someone that was president at some point
  pres_only_speeches <- dataset[dataset$Speaker %in% keys(presidents),]
  # take only those speeches made while the speaker was actually the president
  actually_president <- data.frame()
  for (i in seq(1, nrow(pres_only_speeches))) {
    if (pres_only_speeches$Period[i] %in% values(presidents, keys=pres_only_speeches$Speaker[i])) {
      actually_president <- rbind(actually_president, pres_only_speeches[i,])
    }
  }

  # process the speeches, removing numbers, extra white space and special characters
  corpus <- VCorpus(VectorSource(actually_president$Speech), readerControl=list(language='ger'))
  corpus <- tm_map(corpus, removeNumbers) # REMOVE NUMBERS
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern='[^a-zA-Z0-9äöüÄÖÜß ]', replacement='')
  corpus <- tm_map(corpus, stripWhitespace) # REMOVE EXTRA WHITE SPACE

  actually_president$Speech <- sapply(corpus, as.character)

  speeches_split <- strsplit(actually_president$Speech, ' ')
  vocabulary <- c()
  for (el in speeches_split) {
    vocabulary <- append(vocabulary, unique(el))
  }
  vocabulary <- unique(vocabulary)
  vocabulary <- vocabulary[nchar(vocabulary) > 1]
  vocabulary <- vocabulary[str_count(vocabulary, '(K|k)ohle') == 0]

  corpus2 <- VCorpus(VectorSource(dataset$Speech), readerControl=list(language='ger'))
  corpus2 <- tm_map(corpus2, removeNumbers) # REMOVE NUMBERS
  corpus2 <- tm_map(corpus2, removePunctuation)
  corpus2 <- tm_map(corpus2, content_transformer(gsub), pattern='[^a-zA-Z0-9äöüÄÖÜß ]', replacement='')
  corpus2 <- tm_map(corpus2, stripWhitespace) # REMOVE EXTRA WHITE SPACE

  speeches <- sapply(corpus2, as.character)
  speeches_cleansed <- c()

  pb <- txtProgressBar(min=1, max=length(speeches), initial=1)
  ctr <- 0
  for (speech in speeches) {
    speech_vector <- strsplit(speech, ' ')[[1]]
    speech_cleansed <- speech_vector[! speech_vector %in% vocabulary]
    speeches_cleansed <- append(speeches_cleansed, paste(speech_cleansed, collapse = ' '))
    ctr <- ctr + 1
    setTxtProgressBar(pb, ctr)
  }

  dataset$Speech <- speeches_cleansed
  return(dataset)
}


filter_non_parliament_president_speaker_<- function(dataset) {
  # define all the presidents of the Bundestag
  presidents <- get_presidents_map()
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


filter_irrelevant_words_ <- function(dataset, min_pct, max_pct, stem_speeches=FALSE) {
  source('src/utils/preprocessing.R')

  library(tm)
  library(stringr)
  corpus <- get_preprocessed_corpus(dataset, stem_speeches=stem_speeches, remove_numbers=TRUE)

  mat <- as.matrix(TermDocumentMatrix(corpus, control=list(tolower=FALSE)))

  n_docs <- length(colnames(mat))

  min_docs <- round(n_docs * min_pct)
  max_docs <- round(n_docs * max_pct)

  ctr <- 1
  word_occurences <- data.frame(matrix(ncol=3, nrow=0))

  for (name in rownames(mat)) {
    occurences <- 0
    for (i in seq(1, n_docs)) {
      if (mat[ctr, i] > 0) {
        occurences <- occurences + 1
      }
    }
    if ((str_count(name, '(K|k)ohle') != 0) || ((occurences >= min_docs) && (occurences <= max_docs))) {
      word_occurences <- rbind(word_occurences, list(name, occurences, occurences / n_docs))
    }
    ctr <- ctr + 1
  }

  colnames(word_occurences) <- c("word", "n_occurences", 'pct')

  filter_words <- function(speech_corpus, words) {
    formatted_speeches <- NULL
    for (i in seq(1, length(speech_corpus))) {
      parts <- unlist(strsplit(speech_corpus[[i]][[1]], ' '))
      formatted_speeches <- append(formatted_speeches, paste(parts[parts %in% words], collapse=' '))
    }
    return(formatted_speeches)
  }

  dataset$Speech <- filter_words(corpus, word_occurences$word)
  return(dataset)
}
