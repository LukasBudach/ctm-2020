# Title     : Wordscores visualization
# Objective : Implement all data visualization functions for the Wordscores model


plot_results_vs_expected <- function(result, run_name) {
  # creates a plot showing the prediction scores (red) and reference document scores (black)
  # can be used to quickly visually evaluate the Wordscores model performance

  filename <- paste0('data/', run_name, '_vs_expected.png')
  png(filename=filename, width=1200, height=1200)
  par(mar=c(7, 8, 4, 2), mgp=c(6, 1, 0))
  plot(result$ManualScore, xaxt='n', xlab='Document ID', ylab='Score', cex.lab=3, cex.axis=2)
  points(result$CreatedScore, col='red')
  lines(result$ManualScore)
  lines(result$CreatedScore, col='red')
  axis(1, at=1:length(result$SpeechDbId), labels=result$SpeechDbId, las=2)
  for (j in seq(1:length(result$SpeechDbId))) {
    abline(v=j, col='grey')
  }
  legend('top', legend=c('Manually created scores', 'Wordscores created scores'), col=c('black', 'red'), pch=1,
         xpd=TRUE, bty='n', inset=c(0,0), horiz=TRUE, cex=2)
  dev.off()
}


plot_word_weights <- function(trained_model, run_name) {
  # creates a plot placing each word in the model's dictionary in a 2D space with the word weight on the x-axis
  # and the y-axis as continuous counter to facilitate readability of each individual word
  # can be used to evaluate whether words are scored as expected or whether biases in the model exists

  b <- coef(trained_model)  # naming is relevant here due to later use in a data frame
  words <- as.data.frame(b)
  words$psi <- seq(1:length(b))

  words <- as.matrix(words)
  rownames(words) <- names(b)

  filename <- paste0('data/', run_name, '_word_scores.png')
  png(filename=filename, width=6000, height=6000)
  par(mar=c(32, 32, 4, 2), mgp=c(22, 8, 0), lwd=3)
  plot(words[, 'b'], words[, 'psi'], ylab="Word ID", xlab="", type="n", cex.lab=14, cex.axis=10)
  title(xlab="Word Weight", cex.lab=14, cex.axis=10)
  text(words[, 'b'], words[, 'psi'], rownames(words))
  abline(v=0, lwd=5)
  dev.off()
}


plot_speeches_by_party <- function(raw, res, filename, multiple_periods=FALSE) {
  # creates a plot placing each document (speech) score in a 2D space and showing the party mean
  # x-axis with multiple_periods=FALSE  -> continuous counter for the documents TODO: check
  # x-axis with multiple_periods=TRUE   -> parliamentary period of speeches
  # y-axis                              -> document score in the coal policy space

  plottable <- data.frame()
  colors <- data.frame("cducsu"="black", "fdp"="yellow", "gruene"="green", "spd"="red", "afd"="blue", "linke"="maroon2")

  speaker <- c()
  periods <- c()

  for (el in res$result$SpeechDbId) {
    speaker <- append(speaker, raw$Speaker[raw$SpeechDbId == el])
    if (multiple_periods) {
      periods <- append(periods, raw$Period[raw$SpeechDbId == el])
    }
  }

  for(i in seq(1, length(speaker))) {
    party <- unique(raw$Party[raw$Speaker == speaker[i]])
    if (is.na(party)) {
      next
    }
    if (!party %in% colnames(colors)) {
      next
    }
    if (multiple_periods) {
      plottable <- rbind(plottable, list(i, periods[i], res$result$CreatedScore[i], colors[[party]]))
    } else {
      plottable <- rbind(plottable, list(i, raw$Period[i], res$result$CreatedScore[i], colors[[party]]))
    }
  }

  colnames(plottable) <- c('speaker', 'period', 'position', 'color')
  png(filename=filename, width=1200, height=1200)

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

  par(mar=c(7, 8, 4, 2), mgp=c(4, 1, 0), lwd=2)
  if (multiple_periods) {
    plot(x=plottable$period, y=plottable$position, col=plottable$color, xlab='Parliamentary Period',
         ylab='Position Regarding Coal', xaxt = "n", cex.lab=3, cex.axis=2)
    for (color in u_colors) {
      lines(x=u_periods, y=means$mean_pos[means$color == color], col=color)
      points(x=u_periods, y=means$mean_pos[means$color == color], col=color, pch=15, cex=3)
    }
  } else {
    # TODO: check whether this plots correctly for only one period
    plot(x=plottable$period, y=plottable$position, col=plottable$color, xlab='Parliamentary Period',
         ylab='Position Regarding Coal', xaxt = "n", cex.lab=3, cex.axis=2)
    abline(h=mean(plottable$position[plottable$color == 'black']), col='black')
    abline(h=mean(plottable$position[plottable$color == 'yellow']), col='yellow')
    abline(h=mean(plottable$position[plottable$color == 'green']), col='green')
    abline(h=mean(plottable$position[plottable$color == 'red']), col='red')
    abline(h=mean(plottable$position[plottable$color == 'blue']), col='blue')
    abline(h=mean(plottable$position[plottable$color == 'maroon2']), col='maroon2')
  }

  axis(1, at=plottable$period, las=2, cex.lab=3, cex.axis=2)
  legend(x='topright', legend=colnames(colors), col=as.character(colors[,]), pch=1, cex=2.5)
  dev.off()
}
