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
#
#   returns the resulting data as data.frame with the same columns as the input (but likely less rows!)

filter <- function(dataset, mode, min_period=NULL, max_period=NULL, threshold=NULL, chars_around=NULL) {
  source('utils/filter_functions.R')
  # perform input argument validation:
  required_colnames <- c('Index', 'SpeechDbId', 'Date', 'Period', 'Sitting', 'DocDbId', 'Speaker', 'Party',
                         'InterjectionCount', 'InterjectionContent', 'ParagraphCount', 'Speech')
  valid_modes <- c('p', 'cc', 'cp', 'co', 'nc', 'np')

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
         np= { # only keep the speeches made by speakers that were not Bundestagspräsident at the time of the speech
           filtered_dataset <- filter_non_parliament_president_speaker_(dataset)
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


# group_speeches attributes:
#   dataset -> the dataset to be processed
#   dataset -> must be a data.frame with the columns Index, SpeechDbId, Date, Period, Sitting, DocDbId, Speaker, Party,
#                                                    InterjectionCount, InterjectionContent, ParagraphCount, Speech
#
#   mode -> grouping mode as string, options:
#     none -> no grouping will be conducted, the IDs for the returned documents are their SpeechDbId
#     speaker -> grouping by speaker, the IDs for the returned documents are their speaker name and the document is all
#                speeches of the speeker concatenated
#     party -> grouping by party, the IDs for the returned documents are their party name and the document is all
#              speeches of the party concatenated
#
#   multiple_periods -> default is FALSE, set to TRUE if multiple election periods are in the dataset, will cause the
#                       period number to be appended to the speaker/party name for the returned IDs
#
#   returns the resulting data as data.frame with the two columns ID and Speech

group_speeches <- function(dataset, mode, multiple_periods=FALSE) {
  source('utils/grouping_functions.R')

  # perform input argument validation:
  required_colnames <- c('Index', 'SpeechDbId', 'Date', 'Period', 'Sitting', 'DocDbId', 'Speaker', 'Party',
                         'InterjectionCount', 'InterjectionContent', 'ParagraphCount', 'Speech')
  valid_modes <- c('none', 'speaker', 'party')

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
  }

  # do the grouping
  grouped_dataset <- NULL
  switch(mode,
         none={
           grouped_dataset <- no_grouping_(dataset)
         },
         speaker={
           grouped_dataset <- group_by_speaker_(dataset, multiple_periods)
         },
         party={
           grouped_dataset <- group_by_party_(dataset, multiple_periods)
         }
  )

  # check whether the filtered dataset still has the valid shape, which is same as input dataset in this case
  output_required_colnames <- c('ID', 'Speech')

  if (! is.data.frame(grouped_dataset)) {
    stop('ERROR The grouped dataset is not a data.frame object, which is required.')
  } else if (ncol(grouped_dataset) != 2) {
    stop('ERROR The grouped dataset does not have 12 columns as required.')
  } else if (! all(colnames(grouped_dataset) == output_required_colnames)) {
    stop('ERROR The grouped dataset does not have the required column names.')
  }

  return(grouped_dataset)
}


stem_speech <- function(x) {
  library(SnowballC)
  words <- strsplit(x, ' ')[[1]]
  words <- wordStem(words, language='german')
  return(paste(words, collapse=' '))
}

get_frequency_matrix <- function(dataset, stem_speeches=FALSE, sparse=0.999) {
  library(tm)

  corpus <- VCorpus(VectorSource(data$Speech), readerControl=list(language='ger'))
  corpus <- tm_map(corpus, removeNumbers) # REMOVE NUMBERS
  corpus <- tm_map(corpus, stripWhitespace) # REMOVE EXTRA WHITE SPACE
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern="[^a-zA-Z0-9äöüß ]", replacement='')
  if (stem_speeches) {
    corpus <- tm_map(corpus, content_transformer(stem_speech))
  } else {
    corpus <- tm_map(corpus, content_transformer(tolower)) # MAKES EVERYTHING LOWERCASE
  }
  freq_mat <- TermDocumentMatrix(corpus, control=list(tolower=FALSE))
  sparce_mat <- removeSparseTerms(freq_mat, 0.9999)
  freq <- as.matrix(sparce_mat)
  colnames(freq) <- dataset$ID
  return(freq)
}

run_wordfish <- function(tdmat, repr_1, repr_2, name, tol=1e-7) {
  source('utils/wordfish_1.3.r')

  res <- wordfish(tdmat, dir=c(repr_1, repr_2), tol=tol)
  serialize_results_text(paste0('data/', name, '.txt'), res)
  return(res)
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
