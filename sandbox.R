# Title     : TODO
# Objective : TODO
# Created by: lukas
# Created on: 19/05/2020

# random stuff
for(i in seq(min(data$Period), max(data$Period))) {
  print(sprintf('Period: %d speeches: %d', i, nrow(filter_period(data, i))))
}

for (el in data) {
  print(el)
}

library(stringr)

cts <- data.frame()
for(i in seq(1, nrow(speeches))) {
  cts <- rbind(cts, list(speeches$ID[i], str_count(speeches$Speech[i], 'K*k*ohle')))
}
colnames(cts) <- c('ID', 'CoalCount')

cts <- cts[cts$CoalCount >= 3,]

d_19_coal <- d_19[d_19$SpeechDbId %in% cts$SpeechDbId,]

plottable <- data.frame()
colors <- data.frame("cducsu"="black", "fdp"="yellow", "gruene"="green", "spd"="orange", "afd"="blue", "linke"="red")
for(i in rownames(res$documents)) {
  party <- unique(data$Party[data$SpeechDbId == i])
    if (is.na(party)) {
      next
    }
  if (! party %in% colnames(colors)) {
    next
  }
  plottable <- rbind(plottable, list(i, res$documents[as.String(i), 'omega'], colors[[party]]))
}
colnames(plottable) <- c('ID', 'position', 'color')

png(filename='data/long_result_graph.png', width=1200, height=1200)
plot(plottable$position, col=plottable$color)
legend(x='topleft', legend=colnames(colors), col=as.character(colors[,]), pch=1)
abline(h=mean(plottable$position[plottable$color == 'black']), col='black')
abline(h=mean(plottable$position[plottable$color == 'yellow']), col='yellow')
abline(h=mean(plottable$position[plottable$color == 'green']), col='green')
abline(h=mean(plottable$position[plottable$color == 'orange']), col='orange')
abline(h=mean(plottable$position[plottable$color == 'blue']), col='blue')
abline(h=mean(plottable$position[plottable$color == 'red']), col='red')
dev.off()

speech_len <- sapply(strsplit(data$Speech, " "), length)
br <- seq(min(speech_len)-5, max(speech_len)+5,by=5)
png(filename='data/word_freq_speeches.png', width=2400, height=1200)
hist(speech_len, breaks=br, include.lowest=FALSE, xlim=c(min(br), max(br)), xlab='Number of words in Speech',
     ylab='Number of Speeches', col='blue')
abline(v=median(speech_len), col='blue')
dev.off()

k_count <- str_count(data$Speech, 'K*k*ohle')
br <- seq(min(k_count)-1, max(k_count)+1)
png(filename='data/kohle_count_speeches.png', width=2400, height=1200)
hist(k_count, breaks=br, include.lowest=FALSE, xlim=c(min(br), max(br)), xlab='Number of K*k*ohle matches in Speech',
     ylab='Number of Speeches', col='lightgray')
abline(v=median(k_count), col='blue')
dev.off()