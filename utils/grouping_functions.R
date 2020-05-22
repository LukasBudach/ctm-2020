# Title     : Grouping functions
# Objective : Provide functions that are used to group multiple speeches into one larger speech
# Created by: lukas
# Created on: 22/05/2020

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