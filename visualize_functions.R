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


speaker_speeches_by_party <- function(raw, res, filename) {
  plottable <- data.frame()
  colors <- data.frame("cducsu"="black", "fdp"="yellow", "gruene"="green", "spd"="orange", "afd"="blue", "linke"="red")
  for(i in rownames(res$documents)) {
    if (is.na(i)) {
      next
    }
    party <- unique(raw$Party[raw$Speaker == i])
    if (is.na(party)) {
      next
    }
    plottable <- rbind(plottable, list(i, res$documents[as.String(i), 'omega'], colors[[party]]))
  }
  colnames(plottable) <- c('speaker', 'position', 'color')

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
  #abline(h=mean(plottable$position[plottable$color == 'black']), col='black')
  #abline(h=mean(plottable$position[plottable$color == 'yellow']), col='yellow')
  #abline(h=mean(plottable$position[plottable$color == 'green']), col='green')
  #abline(h=mean(plottable$position[plottable$color == 'orange']), col='orange')
  #abline(h=mean(plottable$position[plottable$color == 'blue']), col='blue')
  #abline(h=mean(plottable$position[plottable$color == 'red']), col='red')
  dev.off()
}

draw_eiffel_tower_diagram <- function (res, filename){
  png(filename=filename, width=6000, height=6000)
  plot(res$words[, 'b'], res$words[, 'psi'], xlab="Word Weights", ylab="Word Fixed Effect")
  text(res$words[, 'b'], res$words[, 'psi'], rownames(res$words))
  dev.off()
}

