# Title     : Vocabulary Visualization
# Objective : Implement functions used to visualize the changes in vocabulary when applying filtering functions

source('src/utils/io.R')
source('src/utils/filtering.R')
source('src/utils/grouping.R')
source('src/utils/preprocessing.R')


prepare_normal_funnel <- function (labels, min_period=NULL, max_period=NULL, multiple_periods=FALSE, sparse=0.999){
  # calculates the vocabulary size for all given filters, executing each filtering function on the complete dataset
  # labels argument is a vector of strings which all follow this pattern: {filterMode}(_{value})?
  # first label can be any identifier, will be the dataset size without filtering in result
  # e.g. labels=c('origin', 'co_50', 'sw_1_25', 'vp') <-> run once without filter, once with coal only 50...

  threshold <- NULL
  chars_around <- NULL
  min_pct <- NULL
  max_pct <- NULL
  wordcounts <- vector()

  for (i in seq(1, length(labels))) {
    if (i == 1){  # first label will be ignored and dataset will be read and processed without any filtering applied
      data <- read_speeches('data/database_export_search_89.csv')
      data <- filter_speeches(data, 'p', min_period, max_period)
      data_new <- group_speeches(data, 'none', multiple_periods)
    } else {
      mode <- sub("_.*", "", labels[i])
      if (mode == 'sw') {
        thresholds <- strsplit(sub(paste0(sub("_.*", "", labels[i]), '_'), "", labels[i]), '_')[[1]]
        min_pct <- as.numeric(thresholds[1])
        max_pct <- as.numeric(thresholds[2])
      }
      else if (!is.na(sub(".*_", "", labels[i]))){
        value <- sub(".*_", "", labels[i])
        if (mode %in% c("cc", "cp")) threshold <- as.numeric(value)
        else if (mode %in% c("co", "nc")) chars_around <- as.numeric(value)
      }
      data_new <- filter_speeches(data, mode, min_period, max_period, threshold, chars_around, min_pct, max_pct)
      data_new <- group_speeches(data_new, 'none', multiple_periods)
    }

    mat <- get_frequency_matrix(data_new, sparse=sparse)
    wordcounts[i] <- nrow(mat)

    rm(data_new)
    rm(mat)
  }
  return(wordcounts)
}


prepare_pipelined_funnel <- function (labels, min_period=NULL, max_period=NULL, multiple_periods=FALSE, sparse=0.999){
  # calculates the vocabulary size for all given filters, executing them in order (as if in a pipeline), starting on the
  # complete dataset and continually shrinking it down
  # labels argument is a vector of strings which all follow this pattern: {filterMode}(_{value})?
  # first label can be any identifier, will be the dataset size without filtering in result
  # e.g. labels=c('origin', 'co_50', 'sw_1_25', 'vp') <-> run once without filter, then with coal only 50, then...

  threshold <- NULL
  chars_around <- NULL
  min_pct <- NULL
  max_pct <- NULL
  wordcounts <- vector()

  for (i in seq(1, length(labels))) {
    if (i == 1){  # first label will be ignored and dataset will be read and processed without any filtering applied
      data_pipelined <- read_speeches('data/database_export_search_89.csv')
      data_pipelined <- filter_speeches(data_pipelined, 'p', min_period, max_period)
    } else {
      mode <- sub("_.*", "", labels[i])
      if (mode == 'sw') {
        thresholds <- strsplit(sub(paste0(sub("_.*", "", labels[i]), '_'), "", labels[i]), '_')[[1]]
        min_pct <- as.numeric(thresholds[1])
        max_pct <- as.numeric(thresholds[2])
      }
      else if (!is.na(sub(".*_", "", labels[i]))){
        value <- sub(".*_", "", labels[i])
        if (mode %in% c("cc", "cp")) threshold <- as.numeric(value)
        else if (mode %in% c("co", "nc")) chars_around <- as.numeric(value)
      }

      data_pipelined <- filter_speeches(data_pipelined, mode, min_period, max_period, threshold, chars_around, min_pct, max_pct)
    }

    data_grouped <- group_speeches(data_pipelined, 'none', multiple_periods)
    mat <- get_frequency_matrix(data_grouped, sparse=sparse)
    wordcounts[i] <- nrow(mat)

    rm(data_grouped)
    rm(mat)
  }
  return(wordcounts)
}


plot_funnel_diagram <- function(labels, wordcounts, filename, pipelined=FALSE){
  # uses the wordcount vectors created by the prepare_***_funnel functions to draw a funnel diagram
  # requires labels vector of same length as the labels vector used to prepare the funnel,
  # using the same vector for both is recommended

  library(ggplot2)
  library(reshape2) # for melt()

  # init columns
  data <- data.frame(labels=character(length(labels)), wordcounts=double(length(wordcounts)), rate=double(length(wordcounts)))
  data$labels <- labels
  data$wordcounts <- wordcounts

  # calculate percentage
  for (i in seq(1, nrow(data))){
    data$rate[i] <- (data$wordcounts[i] * 100) / data$wordcounts[1]
  }

  # add spacing, melt, sort
  total <- subset(data, rate==100)$wordcounts
  data$padding <- (total - data$wordcounts) / 2
  molten <- melt(data[, -3], id.var='labels')
  molten <- molten[order(molten$variable, decreasing=T), ]
  molten$labels <- factor(molten$labels, levels=rev(data$labels))

  ifelse(pipelined, ylabel <- "Pipelined Filters", ylabel <- "Separate Filters")

  ggplot(molten, aes(x=labels)) +
    geom_bar(aes(y=value, fill=variable),
        stat='identity',
        position='stack') +
    geom_text(data=data,
        aes(y=total/2, label=paste(round(rate), '%')),
        color='white',
        size=14) +
    scale_fill_manual(values=c('grey40', NA)) +
    coord_flip() +
    theme(legend.position='none',
          axis.title.y=element_text(face="bold", colour="black", size=40),
          axis.text.y=element_text(colour="black", size=30),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    labs(x=ylabel)

  ggsave(filename=filename)
}
