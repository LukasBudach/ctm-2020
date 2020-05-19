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

draw_eiffel_tower_diagram <- function (res, filename){
  png(filename=filename, width=6000, height=6000)
  plot(res$words[, 'b'], res$words[, 'psi'], xlab="Word Weights", ylab="Word Fixed Effect")
  text(res$words[, 'b'], res$words[, 'psi'], rownames(res$words))
  dev.off()
}

