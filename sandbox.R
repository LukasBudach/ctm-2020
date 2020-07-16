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
data <- group_speeches(data, 'speaker', multiple_periods=TRUE)
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



results <- find_optimum(100, 700, 0.0, 0.1, 0.25, 0.75,
                        50,0.005, 0.1, 'data/wordscores_optim_p19.csv',
                        19, 19)

results <- find_optimum(100, 700, 0.0, 0.1, 0.25, 0.75,
                        50,0.005, 0.1, 'data/wordscores_optim_p18.csv',
                        18, 18)

results <- find_optimum(100, 700, 0.0, 0.1, 0.25, 0.75,
                        50,0.005, 0.1, 'data/wordscores_optim_p18_19.csv',
                        18, 19)

#                 Period 18               || Period 19
nina_scores <-  c(-3,   1.5,  -2,   -3,   1,  -2.0, 3,  1.5,  -2.5, 2)
lukas_scores <- c(-3,   1.5,  -1.8, -2,   2,  -0.5, 3,  1.5,  -2,   2)

pearson <- cor.test(nina_scores, lukas_scores, method='pearson')
pearson_rounded <- cor.test(round(nina_scores), round(lukas_scores), method='pearson')
kendall <- cor.test(nina_scores, lukas_scores, method='kendall')
kendall_rounded <- cor.test(round(nina_scores), round(lukas_scores), method='kendall')

pearson <- list('coeff'=pearson$estimate[[1]], 'pScore'=pearson$p.value)
pearson_rounded <- list('coeff'=pearson_rounded$estimate[[1]], 'pScore'=pearson_rounded$p.value)
kendall <- list('coeff'=kendall$estimate[[1]], 'pScore'=kendall$p.value)
kendall_rounded <- list('coeff'=kendall_rounded$estimate[[1]], 'pScore'=kendall_rounded$p.value)

corr_results <- list('pearson'=pearson, 'pearsonRounded'=pearson_rounded,
                     'kendall'=kendall, 'kendallRounded'=kendall_rounded)

write.csv(corr_results, file='data/something.csv')