# Title     : Wordfish visualization
# Objective : Implement all data visualization functions for the Wordfish model

plot_speeches_by_party <- function(raw, res, filename) {
  # function used for plotting results with each speech being its own document

  library(NLP)

  plottable <- data.frame()
  colors <- data.frame("cducsu"="black", "fdp"="yellow", "gruene"="green", "spd"="red", "afd"="blue", "linke"="maroon2", "others"="grey")
  for(i in rownames(res$documents)) {
    if (is.na(i)) {
      next
    }
    party <- unique(raw$Party[raw$SpeechDbId == i])
    if (is.na(party)) {
      next
    }
    color <- colors[[party]]
    if (is.null(color)) {
      color <- "grey"
    }
    plottable <- rbind(plottable, list(i, res$documents[as.String(i), 'omega'], color))
  }
  colnames(plottable) <- c('ID', 'position', 'color')

  png(filename=filename, width=1200, height=1200)
  par(mar=c(9, 10, 4, 2), mgp=c(6, 1, 0), lwd=2)
  plot(plottable$position, col=plottable$color, xlab='Speech Number', ylab='Position Regarding Coal', xaxt = "n",
       cex.lab=3, cex.axis=2)

  abline(h=mean(plottable$position[plottable$color == 'black']), col='black')
  abline(h=mean(plottable$position[plottable$color == 'yellow']), col='yellow')
  abline(h=mean(plottable$position[plottable$color == 'green']), col='green')
  abline(h=mean(plottable$position[plottable$color == 'red']), col='red')
  abline(h=mean(plottable$position[plottable$color == 'blue']), col='blue')
  abline(h=mean(plottable$position[plottable$color == 'maroon2']), col='maroon2')

  axis(1, at=plottable$period, las=2, cex.lab=3, cex.axis=2)
  legend(x='topright', legend=colnames(colors), col=as.character(colors[,]), pch=1, cex=2.5)

  dev.off()
}

plot_speaker_speeches_by_party <- function(raw, res, filename, multiple_periods=FALSE, with_extreme_parties=FALSE) {
  # function used for plotting results with the speeches of a speaker being grouped together as one document

  plottable <- data.frame()
  colors <- data.frame("cducsu"="black", "fdp"="yellow", "gruene"="green", "spd"="red", "afd"="blue", "linke"="maroon2")
  if (with_extreme_parties) {
    colors <- data.frame("cducsu"="black", "fdp"="yellow", "gruene"="green", "spd"="red", "afd"="blue",
                         "linke"="maroon2", "scoredPro"="darkgreen", "scoredNeutral"="darkgrey", "scoredAnti"="hotpink")
  }

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
    party <- unique(raw$Party[raw$Speaker == indices_no_period[i]])
    if (length(party) > 1) {
      party <- party[!is.na(party)]
    }
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
  par(mar=c(7, 8, 4, 2), mgp=c(4, 1, 0), lwd=2)
  if (multiple_periods) {
    plot(x=plottable$period, y=plottable$position, col=plottable$color, xlab='Parliamentary Period',
         ylab='Position Regarding Coal', xaxt = "n", cex.lab=3, cex.axis=2)
    for (color in u_colors) {
      lines(x=u_periods, y=means$mean_pos[means$color == color], col=color)
      points(x=u_periods, y=means$mean_pos[means$color == color], col=color, pch=15, cex=3)
    }
  } else {
    plot(plottable$position, col=plottable$color, xlab='Parliamentary Period', ylab='Position Regarding Coal',
         xaxt = "n", cex.lab=3, cex.axis=2)
    abline(h=mean(plottable$position[plottable$color == 'black']), col='black')
    abline(h=mean(plottable$position[plottable$color == 'yellow']), col='yellow')
    abline(h=mean(plottable$position[plottable$color == 'green']), col='green')
    abline(h=mean(plottable$position[plottable$color == 'red']), col='red')
    abline(h=mean(plottable$position[plottable$color == 'blue']), col='blue')
    abline(h=mean(plottable$position[plottable$color == 'maroon2']), col='maroon2')
    if (with_extreme_parties) {
      abline(h=mean(plottable$position[plottable$color == 'hotpink']), col='hotpink')
      abline(h=mean(plottable$position[plottable$color == 'darkgrey']), col='darkgrey')
      abline(h=mean(plottable$position[plottable$color == 'darkgreen']), col='darkgreen')
    }
  }

  axis(1, at=plottable$period, las=2, cex.lab=3, cex.axis=2)
  legend(x='topright', legend=colnames(colors), col=as.character(colors[,]), pch=1, cex=2.5)
  dev.off()
}


plot_party_speeches_by_party <- function(res, filename, multiple_periods=FALSE, with_extreme_parties=FALSE) {
  # function used for plotting results with the speeches of a party being grouped together as one document

  plottable <- data.frame()
  colors <- data.frame("cducsu"="black", "fdp"="yellow", "gruene"="green", "spd"="red", "afd"="blue", "linke"="maroon2")
  if (with_extreme_parties) {
    colors <- data.frame("cducsu"="black", "fdp"="yellow", "gruene"="green", "spd"="red", "afd"="blue",
                         "linke"="maroon2", "scoredPro"="darkgreen", "scoredNeutral"="darkgrey", "scoredAnti"="hotpink")
  }

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
      plottable <- rbind(plottable, list(party, 'unknown', res$documents[i, 'omega'], colors[[party]]))
    }
  }
  colnames(plottable) <- c('party', 'period', 'position', 'color')

  png(filename=filename, width=600, height=600)
  plot(x=plottable$period, xlab="Legislative Period", y=plottable$position, ylab="Party Position", col=plottable$color, xaxt = "n")
  par(mar = c(5, 4, 4, 8))
  axis(1, at=plottable$period, las=2)
  legend(x='topright', legend=colnames(colors), col=as.character(colors[,]), pch=1)
  dev.off()
}


plot_word_weights_and_frequencies <- function (res, filename){
  # creates a plot placing each word in the result in a 2D space with the word weight (beta) on the x-axis
  # and the word fixed effect (psi, based on frequency) on the y-axis
  # can be used to evaluate whether words are scored as expected or whether biases in the model exists

  png(filename=filename, width=6000, height=6000)
  par(mar=c(32, 32, 4, 2), mgp=c(22, 8, 0), lwd=3)
  plot(res$words[, 'b'], res$words[, 'psi'], ylab="Word Fixed Effect", xlab="", type="n", cex.lab=14, cex.axis=10)
  title(xlab="Word Weight", cex.lab=14, cex.axis=10)
  text(res$words[, 'b'], res$words[, 'psi'], rownames(res$words))
  abline(v=0, lwd=3)
  dev.off()
}
