# Title     : Grouping functions
# Objective : Provide functions that are used to group multiple speeches into one larger speech
# Created by: lukas
# Created on: 22/05/2020

group_speeches <- function(dataset, mode, multiple_periods=FALSE) {
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


# grouping function definitions
no_grouping_ <- function(dataset) {
  grouped <- data.frame()
  for (i in seq(1, nrow(dataset))) {
      grouped <- rbind(grouped, list(dataset$SpeechDbId[i], dataset$Speech[i]))
  }
  colnames(grouped) <- c('ID', 'Speech')
  return(grouped)
}

group_by_speaker_ <- function(dataset, multiple_periods) {
  # if there are multiple periods in the dataset, append the period number to the speaker name
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

group_by_party_ <- function(dataset, multiple_periods) {
  # if there are multiple periods in the dataset, append the period number to the party name
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