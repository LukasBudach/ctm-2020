# Title     : TODO
# Objective : TODO
# Created by: lukas
# Created on: 15.05.2020

speeches_by_party <- function(raw, res, filename) {
  plottable <- data.frame()
  colors <- data.frame("cducsu"="black", "fdp"="yellow", "gruene"="green", "spd"="orange", "afd"="blue", "linke"="red")
  for(i in rownames(res$documents)) {
    if (is.na(i)) {
      next
    }
    party <- unique(raw$Party[raw$SpeechDbId == i])
    if (is.na(party)) {
      next
    }
    plottable <- rbind(plottable, list(i, res$documents[as.String(i), 'omega'], colors[[party]]))
  }
  colnames(plottable) <- c('ID', 'position', 'color')

  png(filename=filename, width=600, height=600)
  plot(plottable$position, col=plottable$color)
  legend(x='topleft', legend=colnames(colors), col=as.character(colors[,]), pch=1)
  abline(h=mean(plottable$position[plottable$color == 'black']), col='black')
  abline(h=mean(plottable$position[plottable$color == 'yellow']), col='yellow')
  abline(h=mean(plottable$position[plottable$color == 'green']), col='green')
  abline(h=mean(plottable$position[plottable$color == 'orange']), col='orange')
  abline(h=mean(plottable$position[plottable$color == 'blue']), col='blue')
  abline(h=mean(plottable$position[plottable$color == 'red']), col='red')
  dev.off()
}

speaker_speeches_by_party <- function(raw, res, filename, multiple_periods=FALSE) {
  plottable <- data.frame()
  colors <- data.frame("cducsu"="black", "fdp"="yellow", "gruene"="green", "spd"="orange", "afd"="blue", "linke"="red")

  indices_no_period <- c()
  periods <- c()
  if (multiple_periods) {
    for (el in rownames(res$documents)) {
      indices_no_period <- append(indices_no_period, trimws(strsplit(el, "_")[[1]][1]))
      periods <- append(periods, trimws(strsplit(el, "_")[[1]][2]))
    }
  } else {
    indices_no_period <- rownames(res$documents)
  }

  #for(i in rownames(res$documents)) {
  #  if (is.na(i)) {
  #    next
  #  }
  #  party <- unique(raw$Party[raw$Speaker == i])
  #  if (is.na(party)) {
  #    next
  #  }
  #  plottable <- rbind(plottable, list(i, res$documents[as.String(i), 'omega'], colors[[party]]))
  #}
  #colnames(plottable) <- c('speaker', 'position', 'color')



  for(i in seq(1, length(indices_no_period))) {
    party <- unique(raw$Party[raw$Speaker == indices_no_period[i]])
    if (is.na(party)) {
      next
    }
    if (! party %in% colnames(colors)) {
      next
    }
    if (multiple_periods) {
      plottable <- rbind(plottable, list(i, periods[i], res$documents[i, 'omega'], colors[[party]]))
    } else {
      plottable <- rbind(plottable, list(i, 1, res$documents[i, 'omega'], colors[[party]]))
    }
  }
  colnames(plottable) <- c('speaker', 'period', 'position', 'color')

  if (multiple_periods) {
    means <- data.frame()
    u_periods <- unique(plottable$period)
    u_colors <- unique(plottable$color)
    for (col in u_colors) {
      for (p in u_periods) {
        means <- rbind(means, list(col, p, mean(plottable$position[(plottable$color == col) & (plottable$period == p)])))
      }
    }
    colnames(means) <- c('color', 'period', 'mean_pos')
  }

  png(filename=filename, width=1200, height=1200)
  par(oma = c(3.5, 1, 1, 1))
  if (multiple_periods) {
    plot(x=plottable$period, y=plottable$position, col=plottable$color, xlab='parliamentary period',
         ylab='position regarding coal')
    for (color in u_colors) {
      lines(x=u_periods, y=means$mean_pos[means$color == color], col=color)
      points(x=u_periods, y=means$mean_pos[means$color == color], col=color, pch=15, cex=1.5)
    }
  } else {
    plot(plottable$position, col=plottable$color, xlab='parliamentary period', ylab='position regarding coal')
    abline(h=mean(plottable$position[plottable$color == 'black']), col='black')
    abline(h=mean(plottable$position[plottable$color == 'yellow']), col='yellow')
    abline(h=mean(plottable$position[plottable$color == 'green']), col='green')
    abline(h=mean(plottable$position[plottable$color == 'orange']), col='orange')
    abline(h=mean(plottable$position[plottable$color == 'blue']), col='blue')
    abline(h=mean(plottable$position[plottable$color == 'red']), col='red')
  }
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  legend('bottom', legend=colnames(colors), col=as.character(colors[,]), pch=1, xpd=TRUE, bty='n', inset=c(0,0),
         horiz=TRUE)
  dev.off()
}

party_speeches_by_party <- function(raw, res, filename, multiple_periods=FALSE) {
  plottable <- data.frame()
  colors <- data.frame("cducsu"="black", "fdp"="yellow", "gruene"="green", "spd"="orange", "afd"="blue", "linke"="red")

  indices_no_period <- c()
  periods <- c()
  if (multiple_periods) {
    for (el in rownames(res$documents)) {
      indices_no_period <- append(indices_no_period, trimws(strsplit(el, "_")[[1]][1]))
      periods <- append(periods, trimws(strsplit(el, "_")[[1]][2]))
    }
  } else {
    indices_no_period <- rownames(res$documents)
  }

  for(i in seq(1, length(indices_no_period))) {
    party <- indices_no_period[i]
    if (! party %in% colnames(colors)) {
      next
    }
    if (multiple_periods) {
      plottable <- rbind(plottable, list(party, periods[i], res$documents[i, 'omega'], colors[[party]]))
    } else {
      plottable <- rbind(plottable, list(party, 1, res$documents[i, 'omega'], colors[[party]]))
    }
  }
  colnames(plottable) <- c('party', 'period', 'position', 'color')

  png(filename=filename, width=600, height=600)
  plot(x=plottable$period, y=plottable$position, col=plottable$color)
  par(mar = c(5,4,4,8))
  legend(x='right', legend=colnames(colors), col=as.character(colors[,]), pch=1)
  dev.off()
}

draw_eiffel_tower_diagram <- function (res, filename){
  png(filename=filename, width=6000, height=6000)
  plot(res$words[, 'b'], res$words[, 'psi'], xlab="Word Weights", ylab="Word Fixed Effect", type="n")
  text(res$words[, 'b'], res$words[, 'psi'], rownames(res$words))
  dev.off()
}

# labels := {mode}(_{value})?
prepare_normal_funnel <- function (labels, min_period=NULL, max_period=NULL, multiple_periods=FALSE, sparse=0.999){
  threshold <- NULL
  chars_around <- NULL
  wordcounts <- vector()

  for (i in seq(1, length(labels))) {
    if (i == 1){
      data <- read_speeches('data/database_export_search_89.csv')
      data <- filter(data, 'p', min_period, max_period)
      data_new <- group_speeches(data, 'none', multiple_periods)
    } else {
      mode <- sub("_.*", "", labels[i])
      if (!is.na(sub(".*_", "", labels[i]))){
        value <- sub(".*_", "", labels[i])
        if (mode %in% c("cc", "cp")) threshold <- as.numeric(value)
        else if (mode %in% c("co", "nc")) chars_around <- as.numeric(value)
      }
      data_new <- filter(data, mode, min_period, max_period, threshold, chars_around)
      data_new <- group_speeches(data_new, 'none', multiple_periods)
    }

    mat <- get_frequency_matrix(data_new, sparse=sparse)
    wordcounts[i] <- nrow(mat)

    rm(data_new)
    rm(mat)
  }
  return(wordcounts)
}

# labels := {mode}(_{value})?
prepare_pipelined_funnel <- function (labels, min_period=NULL, max_period=NULL, multiple_periods=FALSE, sparse=0.999){
  threshold <- NULL
  chars_around <- NULL
  wordcounts <- vector()

  for (i in seq(1, length(labels))) {
    if (i == 1){
      data_pipelined <- read_speeches('data/database_export_search_89.csv')
      data_pipelined <- filter(data_pipelined, 'p', min_period, max_period)
    } else {
      mode <- sub("_.*", "", labels[i])
      if (!is.na(sub(".*_", "", labels[i]))){
        value <- sub(".*_", "", labels[i])
        if (mode %in% c("cc", "cp")) threshold <- as.numeric(value)
        else if (mode %in% c("co", "nc")) chars_around <- as.numeric(value)
      }

      data_pipelined <- filter(data_pipelined, mode, min_period, max_period, threshold, chars_around)
    }

    data_grouped <- group_speeches(data_pipelined, 'none', multiple_periods)
    mat <- get_frequency_matrix(data_grouped, sparse=sparse)
    wordcounts[i] <- nrow(mat)

    rm(data_grouped)
    rm(mat)
  }
  return(wordcounts)
}

draw_funnel_diagram <- function(labels, wordcounts, filename, pipelined=FALSE){
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

  ifelse(pipelined, ylabel <- "Pipelined Filters", ylabel <- "Seperate Filters")

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