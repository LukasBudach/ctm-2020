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

data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)
res_gt <- unserialize_results_text('data/speaker_p_17_19_nc_100_Hofreiter18_Laemmel18.txt')
res_all <- unserialize_results_text('data/speaker_p_17_19_Hofreiter18_Laemmel18.txt')

res_all$documents[,'omega'] = res_all$documents[,'omega'] - res_gt$documents[,'omega']

speaker_speeches_by_party(raw=data, res=res_all, filename='data/speaker_p17_19_Hofreiter18_Laemmel18_diff_all_nc_100.png', TRUE)

data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=18, max_period=19)
data <- filter(data, 'vp')

labels <- c('original', 'vp', 'np', 'co_50')
wordcounts <- prepare_pipelined_funnel(labels, 18, 18, FALSE, 0.9999)
draw_funnel_diagram(labels, wordcounts, 'data/funnel_pipeline_vp_np_co_50.png', TRUE)

#####################################################################################################################
#################################################### WORDFISH #######################################################

read_scored_extremes <- function(filepath) {
  library(readr)
  col_names <- c('SpeechDbId', 'CoalScore')
  cols <- cols(SpeechDbId=col_integer(), CoalScore=col_double())
  return(read_csv(filepath, skip=1, col_names=col_names, col_types=cols))
}

data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=18, max_period=19)
scored_18 <- read_scored_extremes('data/scored_extremes_18.csv')
scored_19 <- read_scored_extremes('data/scored_extremes_19.csv')
ids_in_both_18 <- intersect(data$SpeechDbId, scored_18$SpeechDbId)
ids_in_both_19 <- intersect(data$SpeechDbId, scored_19$SpeechDbId)

scored_18 <- scored_18[scored_18$SpeechDbId %in% ids_in_both_18,]
scored_19 <- scored_19[scored_19$SpeechDbId %in% ids_in_both_19,]
scored_18$Speech <- data[data$SpeechDbId %in% scored_18$SpeechDbId,]$Speech
scored_19$Speech <- data[data$SpeechDbId %in% scored_19$SpeechDbId,]$Speech

pro_coal_18 <- scored_18[scored_18$CoalScore > 0,]
neutral_18 <- scored_18[scored_18$CoalScore == 0,]
anti_coal_18 <- scored_18[scored_18$CoalScore < 0,]
pro_coal_19 <- scored_19[scored_19$CoalScore > 0,]
neutral_19 <- scored_19[scored_19$CoalScore == 0,]
anti_coal_19 <- scored_19[scored_19$CoalScore < 0,]

data <- rbind(data, list(as.integer(0), as.integer(0), '2018-06-21', as.integer(18), as.integer(1), as.integer(0), 'ProCoal', 'scoredPro', as.integer(0), '', as.integer(0), paste(pro_coal_18$Speech, collapse=' ')))
data <- rbind(data, list(as.integer(1), as.integer(1), '2018-06-21', as.integer(18), as.integer(1), as.integer(1), 'Neutral', 'scoredNeutral', as.integer(0), '', as.integer(0), paste(neutral_18$Speech, collapse=' ')))
data <- rbind(data, list(as.integer(2), as.integer(2), '2018-06-21', as.integer(18), as.integer(1), as.integer(2), 'AntiCoal', 'scoredAnti', as.integer(0), '', as.integer(0), paste(anti_coal_18$Speech, collapse=' ')))
data <- rbind(data, list(as.integer(0), as.integer(0), '2018-06-21', as.integer(19), as.integer(1), as.integer(0), 'ProCoal', 'scoredPro', as.integer(0), '', as.integer(0), paste(pro_coal_19$Speech, collapse=' ')))
data <- rbind(data, list(as.integer(1), as.integer(1), '2018-06-21', as.integer(19), as.integer(1), as.integer(1), 'Neutral', 'scoredNeutral', as.integer(0), '', as.integer(0), paste(neutral_19$Speech, collapse=' ')))
data <- rbind(data, list(as.integer(2), as.integer(2), '2018-06-21', as.integer(19), as.integer(1), as.integer(2), 'AntiCoal', 'scoredAnti', as.integer(0), '', as.integer(0), paste(anti_coal_19$Speech, collapse=' ')))

data <- filter(data, 'co', chars_around=300)
data <- group_speeches(data, 'party', multiple_periods=TRUE)
mat <- get_frequency_matrix(data, sparse=0.9999)                 # create the TDM

res <- wordfish(input=mat, fixtwo=TRUE, fixdoc=c(16, 18, 2.5, -2.5), sigma=1, tol=5e-6)
f <- file('data/wordfish_fixtwo_test.txt', 'w+')
serialize(connection=f, object=res, ascii=TRUE)
close(f)

res <- run_wordfish(tdmat=mat,
                    repr_1=13,  # extreme pro
                    repr_2=15,  # extreme anti
                    name='party_p_18_19_co_300_proCoal_antiCoal',
                    tol=5e-6)

f <- file('data/wordfish_fixtwo_test.txt', 'r')
res <- unserialize(f)
close(f)

party_speeches_by_party_extremes(res, filename='data/party_p_18_19_co_300_scoredPro19_scoredAnti19_separate.png', TRUE)

#####################################################################################################################
#################################################### WORDSCORES #####################################################

library(quanteda)
library(quanteda.textmodels)
library(stopwords)

read_scored_extremes <- function(filepath) {
  library(readr)
  col_names <- c('SpeechDbId', 'CoalScore')
  cols <- cols(SpeechDbId=col_integer(), CoalScore=col_double())
  return(read_csv(filepath, skip=1, col_names=col_names, col_types=cols))
}

data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=19, max_period=19)
# data <- filter(data, 'vp')
# data <- filter(data, 'np')
data$Speech <- gsub(pattern='[^a-zA-Z0-9äöüÄÖÜß ]', replacement='', data$Speech)
# data <- filter(data, 'co', chars_around=300)
scored <- read_scored_extremes('data/scored_extremes.csv')
ids_in_both <- intersect(data$SpeechDbId, scored$SpeechDbId)
scored <- scored[scored$SpeechDbId %in% ids_in_both,]
scored$Speech <- data[data$SpeechDbId %in% scored$SpeechDbId,]$Speech
scored <- scored[abs(scored$CoalScore) >= 2,]

corpus <- dfm(scored$Speech, remove=stopwords(language='German'))
model <- textmodel_wordscores(corpus, scored$CoalScore, scale='linear')


virgin_corpus <- dfm(data$Speech, remove=stopwords(language='German'))
result <- predict(model, virgin_corpus, se.fit=TRUE)

readable_result <- as.data.frame(data$SpeechDbId)
readable_result$CreatedScore <- result$fit

colnames(readable_result) <- c('SpeechDbId', 'CreatedScore')

readable_result <- readable_result[with(readable_result, order(SpeechDbId)),]
scored <- scored[with(scored, order(SpeechDbId)),]

readable_result <- readable_result[readable_result$SpeechDbId %in% scored$SpeechDbId,]
readable_result$ManualScore <- scored$CoalScore

filename <- 'data/wordscores_result_19_vs_expected.png'
png(filename=filename, width=600, height=600)
plot(readable_result$ManualScore, xaxt='n', xlab='', ylab='Score')
points(readable_result$CreatedScore, col='red')
lines(readable_result$ManualScore)
lines(readable_result$CreatedScore, col='red')
axis(1, at=1:length(readable_result$SpeechDbId), labels=readable_result$SpeechDbId, las=2)
# points(as.character(readable_result$SpeechDbId), readable_result$ManualScore, col='black')
# plot(readable_result$SpeechDbId, readable_result$ManualScore, xlab="Speech ID", ylab="Score")
# points(readable_result$SpeechDbId, readable_result$CreatedScore, col='red')
for (j in seq(1:length(readable_result$SpeechDbId))) {
  abline(v=j, col='grey')
}
legend('top', legend=c('Manually created scores', 'Wordscores created scores'), col=c('black', 'red'), pch=1,
       xpd=TRUE, bty='n', inset=c(0,0), horiz=TRUE)
dev.off()

write_csv(readable_result, path='data/wordscores_result.csv')

b <- coef(model)
words <- as.data.frame(b)
words$psi <- seq(1:length(b))

words <- as.matrix(words)
rownames(words) <- names(b)

filename <- 'data/wordscores_result_eiffel.png'
png(filename=filename, width=6000, height=6000)
plot(words[, 'b'], words[, 'psi'], xlab="Word Weights", ylab="Word Fixed Effect", type="n")
text(words[, 'b'], words[, 'psi'], rownames(words))
abline(v=0)
dev.off()


prepped <- as.data.frame(b)
prepped$words <- words


draw_eiffel_tower_diagram(prepped, 'data/wordscores_result_eiffel.png')

summary(model)

