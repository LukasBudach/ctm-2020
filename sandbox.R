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

# library(XML)
# paliamentarians <- xmlToDataFrame('data/german_parliamentarians.xml')
# colnames(parliamentarians) <- c('Surname', 'SurnameAlt', 'FirstName', 'FirstNameAlt', 'Title', 'AcademicTitle',
#                                 'RegionSpecifier', 'NobilityTitle', 'Prefix', 'FullName', 'Periods', 'ActiveCountry',
#                                 '')


data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)

n_data <- filter(data, 'nc', chars_around=50)
c_data <- filter(data, 'co', chars_around=50)
pres <- filter(data, 'np')
data <- filter(data, 'cc', threshold=3)
data <- filter(data, 'cp', threshold=0.06)
data <- group_speeches(data, 'none', 'TRUE')

res_gt <- unserialize_results_text('data/party_p_17_19_nc_150_green18_cdu18.txt')
res_all <- unserialize_results_text('data/party_p_17_19_green18_cdu18.txt')

res_all$documents[,'omega'] = res_all$documents[,'omega'] - res_gt$documents[,'omega']

party_speeches_by_party(raw=data, res=res_all, filename='data/party_p17_19_green18_cdu18_diff_all_nc_150.png', TRUE)

library(stringr)
words <- strsplit(data$Speech, ' ')
w_len <- NULL
for(el in words) {
  w_len <- append(w_len, nchar(el))
}

hist(w_len)
mean(w_len)

removeSpecialChars <- function(x) return (gsub("[^a-zA-Z0-9 ]","",x))

library(tm)
library(SnowballC)

stem_speech <- function(x) {
  words <- strsplit(x, ' ')[[1]]
  words <- wordStem(words, language='german')
  return(paste(words, collapse=' '))
}

data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)

corpus <- VCorpus(VectorSource(data$Speech), readerControl=list(language='ger'))
corpus <- tm_map(corpus, content_transformer(tolower)) # MAKES EVERYTHING LOWERCASE
corpus <- tm_map(corpus, removeNumbers) # REMOVE NUMBERS
corpus <- tm_map(corpus, stripWhitespace) # REMOVE EXTRA WHITE SPACE
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(gsub), pattern="[^a-zA-Z0-9äöüß ]", replacement='')

# corpus <- tm_map(corpus, stemDocument, language='german')
corpus <- tm_map(corpus, content_transformer(stem_speech))

freq_mat <- TermDocumentMatrix(corpus)
sparce_mat <- removeSparseTerms(freq_mat, 0.9999)


library(SnowballC)

getStemLanguages()
stem

x <- 'Ich schaltete ab, abschaltete und abschaltet'

test <- paste(words, collapse=' ')

stem_speech('Ich schaltete ab, abschaltete und abschaltet')

wordStem(c('Ich', 'schaltete', 'ab', 'abschaltete', 'und', 'abschaltet'), language='german')
