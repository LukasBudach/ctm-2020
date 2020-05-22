# Title     : TODO
# Objective : TODO
# Created by: lukas
# Created on: 19/05/2020

load_periods <- function(data_file, min_period, max_period) {
  # read data and filter by periods
  all_data <- read_speeches(data_file)
  return(filter_period_range(all_data, min_period, max_period))
}

run_wordfish <- function(tdmat, repr_1, repr_2, name, tol=1e-7) {
  res <- wordfish(tdmat, dir=c(repr_1, repr_2), tol=tol)
  serialize_results_text(paste0('data/', name, '.txt'), res)
  return(res)
}

tdmat_regular <- function(data, min_period, max_period, concat_mode, sparse) {
  data_filtered <- data

  # create the documents object with ID and Speech field
  documents <- data.frame()
  if (concat_mode == 0) {
    for (i in seq(1, nrow(data_filtered))) {
      documents <- rbind(documents, list(data_filtered$SpeechDbId[i], data_filtered$Speech[i]))
    }
    colnames(documents) <- c('ID', 'Speech')
  } else if (concat_mode == 1) {
    documents <- concat_by_speaker(data_filtered, multiple_periods=(min_period != max_period))
  } else if (concat_mode == 2) {
    documents <- concat_by_party(data_filtered, multiple_periods=(min_period != max_period))
  } else {
    return()
  }

  # create and return the term document matrix
  return(get_frequency_matrix(documents, sparse))
}


# concat mode is 0 for none (keep speeches) 1 for speakers and 2 for parties
tdmat_coal_counts <- function(data, min_period, max_period, concat_mode, coal_thresh, sparse) {
  data_filtered <- data

  # filter by coal counts
  coal_counts <- filter_coal_count(data_filtered, coal_thresh)
  data_filtered <- data_filtered[data_filtered$SpeechDbId %in% coal_counts$SpeechDbId,]

  # create the documents object with ID and Speech field
  documents <- data.frame()
  if (concat_mode == 0) {
    for (i in seq(1, nrow(data_filtered))) {
      documents <- rbind(documents, list(data_filtered$SpeechDbId[i], data_filtered$Speech[i]))
    }
    colnames(documents) <- c('ID', 'Speech')
  } else if (concat_mode == 1) {
    documents <- concat_by_speaker(data_filtered, multiple_periods=(min_period != max_period))
  } else if (concat_mode == 2) {
    documents <- concat_by_party(data_filtered, multiple_periods=(min_period != max_period))
  } else {
    return()
  }

  # create and return the term document matrix
  return(get_frequency_matrix(documents, sparse))
}

# concat mode is 0 for none (keep speeches) 1 for speakers and 2 for parties
tdmat_coal_percent <- function(data, min_period, max_period, concat_mode, coal_thresh, sparse) {
  data_filtered <- data

  # filter by coal counts
  coal_counts <- filter_coal_percentile(data_filtered, coal_thresh)
  data_filtered <- data_filtered[data_filtered$SpeechDbId %in% coal_counts$SpeechDbId,]

  # create the documents object with ID and Speech field
  documents <- data.frame()
  if (concat_mode == 0) {
    for (i in seq(1, nrow(data_filtered))) {
      documents <- rbind(documents, list(data_filtered$SpeechDbId[i], data_filtered$Speech[i]))
    }
    colnames(documents) <- c('ID', 'Speech')
  } else if (concat_mode == 1) {
    documents <- concat_by_speaker(data_filtered, multiple_periods=(min_period != max_period))
  } else if (concat_mode == 2) {
    documents <- concat_by_party(data_filtered, multiple_periods=(min_period != max_period))
  } else {
    return()
  }

  # create and return the term document matrix
  return(get_frequency_matrix(documents, sparse))
}

# concat mode is 0 for none (keep speeches) 1 for speakers and 2 for parties
tdmat_coal_sections_only <- function(data, min_period, max_period, concat_mode, words_around, sparse) {
  data_filtered <- data

  # filter by coal counts
  data_filtered <- get_only_coal_segments(data_filtered, words_around)

  # create the documents object with ID and Speech field
  documents <- data.frame()
  if (concat_mode == 0) {
    for (i in seq(1, nrow(data_filtered))) {
      documents <- rbind(documents, list(data_filtered$SpeechDbId[i], data_filtered$Speech[i]))
    }
    colnames(documents) <- c('ID', 'Speech')
  } else if (concat_mode == 1) {
    documents <- concat_by_speaker(data_filtered, multiple_periods=(min_period != max_period))
  } else if (concat_mode == 2) {
    documents <- concat_by_party(data_filtered, multiple_periods=(min_period != max_period))
  } else {
    return()
  }

  # create and return the term document matrix
  return(get_frequency_matrix(documents, sparse))
}

# concat mode is 0 for none (keep speeches) 1 for speakers and 2 for parties
tdmat_without_coal_sections <- function(data, min_period, max_period, concat_mode, words_around, sparse) {
  data_filtered <- data

  # filter by coal counts
  data_filtered <- get_segments_without_coal(filtered_data, words_around)

  # create the documents object with ID and Speech field
  documents <- data.frame()

  if (concat_mode == 0) {
    for (i in seq(1, nrow(data_filtered))) {
      documents <- rbind(documents, list(data_filtered$SpeechDbId[i], data_filtered$Speech[i]))
    }
    colnames(documents) <- c('ID', 'Speech')
  } else if (concat_mode == 1) {
    documents <- concat_by_speaker(data_filtered, multiple_periods=(min_period != max_period))
  } else if (concat_mode == 2) {
    documents <- concat_by_party(data_filtered, multiple_periods=(min_period != max_period))
  } else {
    return()
  }

  # create and return the term document matrix
  return(get_frequency_matrix(documents, sparse))
}