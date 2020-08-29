# Title     : Sandbox
# Objective : Script used for development and testing only, some content may be outdated

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
colors <- data.frame("cducsu"="black", "fdp"="yellow", "gruene"="green", "spd"="red", "afd"="blue", "linke"="maroon2")
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
abline(h=mean(plottable$position[plottable$color == 'red']), col='red')
abline(h=mean(plottable$position[plottable$color == 'blue']), col='blue')
abline(h=mean(plottable$position[plottable$color == 'maroon2']), col='maroon2')
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
data <- filter_speeches(data, 'p', min_period=17, max_period=19)

n_data <- filter_speeches(data, 'nc', chars_around=50)
c_data <- filter_speeches(data, 'co', chars_around=50)
pres <- filter_speeches(data, 'np')
data <- filter_speeches(data, 'cc', threshold=3)
data <- filter_speeches(data, 'cp', threshold=0.06)
data <- group_speeches(data, 'none', 'TRUE')

res_gt <- unserialize_results_text('data/party_p_17_19_nc_150_green18_cdu18.txt')
res_all <- unserialize_results_text('data/party_p_17_19_green18_cdu18.txt')

res_all$documents[,'omega'] = res_all$documents[,'omega'] - res_gt$documents[,'omega']

plot_party_speeches_by_party(raw=data, res=res_all, filename='data/party_p17_19_green18_cdu18_diff_all_nc_150.png', TRUE)

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
data <- filter_speeches(data, 'p', min_period=17, max_period=19)

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
data <- filter_speeches(data, 'p', min_period=17, max_period=19)
res_gt <- unserialize_results_text('data/speaker_p_17_19_nc_100_Hofreiter18_Laemmel18.txt')
res_all <- unserialize_results_text('data/speaker_p_17_19_Hofreiter18_Laemmel18.txt')

res_all$documents[,'omega'] = res_all$documents[,'omega'] - res_gt$documents[,'omega']

plot_speaker_speeches_by_party(raw=data, res=res_all, filename='data/speaker_p17_19_Hofreiter18_Laemmel18_diff_all_nc_100.png', TRUE)

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=18, max_period=19)
data <- filter_speeches(data, 'vp')

labels <- c('original', 'co_100')
wordcounts <- prepare_pipelined_funnel(labels, 17, 19, TRUE, 0.9999)
plot_funnel_diagram(labels, wordcounts, 'data/funnel_pipeline_co_100.png', TRUE)

#####################################################################################################################
#################################################### WORDFISH #######################################################

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=18, max_period=19)
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

data <- filter_speeches(data, 'co', chars_around=300)
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



########################## Wordfish ##########################

co <- 500
sw_min <- 0.01
sw_max <- 0.25
min_p <- 18
max_p <- 18

run_name <- paste0('wordfish_p_', min_p, '_', max_p, '_co_', co, '_sw_',
                   strsplit(as.character(sw_min), "\\.")[[1]][2], '_',
                   strsplit(as.character(sw_max), "\\.")[[1]][2])


data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=min_p, max_period=max_p)
data <- filter_speeches(data, 'co', chars_around=co)
data <- filter_speeches(data, 'sw', min_pct=sw_min, max_pct=sw_max)
visualization_copy <- data
data <- group_speeches(data, '', multiple_periods=(min_p != max_p))   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)
run_name <- paste0(run_name, '_speaker', '_Hofreiter19', '_Laemmel19')
res <- run_wordfish(tdmat=mat,
                    repr_1=217, # anti
                    repr_2=215, # pro
                    name=run_name,
                    tol=1e-7)
# visualization
plot_speaker_speeches_by_party(raw=visualization_copy, res=res, filename=paste0('data/', run_name, '.png'), (min_p != max_p))
plot_word_weights_and_frequencies(res=res, filename=paste0('data/', run_name, '_words.png'))


data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=17, max_period=19)
data <- filter_speeches(data, 'nc', chars_around=50)
data <- group_speeches(data, 'speaker', 'TRUE')
mat <- get_frequency_matrix(data, sparse=0.9999)

res_gt <- run_wordfish(tdmat=mat,
                    repr_1=164, # anti
                    repr_2=188, # pro
                    name='speaker_p17_19_nc_50_Hofreiter18_Laemmel18',
                    tol=1e-7)

res_all <- unserialize_results_text('data/speaker_p_17_19_Hofreiter18_Laemmel18.txt')

res_all$documents[,'omega'] <- res_all$documents[,'omega'] - res_gt$documents[,'omega']

plot_speaker_speeches_by_party(raw=data, res=res_all, filename='data/speaker_p17_19_green18_cdu18_diff_all_nc_50.png', TRUE)


data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=17, max_period=19)
data <- filter_speeches(data, 'nc', chars_around=75)
data <- group_speeches(data, 'speaker', 'TRUE')
mat <- get_frequency_matrix(data, sparse=0.9999)

res_gt <- run_wordfish(tdmat=mat,
                    repr_1=164, # anti
                    repr_2=188, # pro
                    name='speaker_p17_19_nc_75_Hofreiter18_Laemmel18',
                    tol=1e-7)

res_all <- unserialize_results_text('data/speaker_p_17_19_Hofreiter18_Laemmel18.txt')

res_all$documents[,'omega'] <- res_all$documents[,'omega'] - res_gt$documents[,'omega']

plot_speaker_speeches_by_party(raw=data, res=res_all, filename='data/speaker_p17_19_green18_cdu18_diff_all_nc_75.png', TRUE)



data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=17, max_period=19)
data <- filter_speeches(data, 'nc', chars_around=125)
data <- group_speeches(data, 'speaker', 'TRUE')
mat <- get_frequency_matrix(data, sparse=0.9999)

res_gt <- run_wordfish(tdmat=mat,
                    repr_1=164, # anti
                    repr_2=188, # pro
                    name='speaker_p17_19_nc_125_Hofreiter18_Laemmel18',
                    tol=1e-7)

res_all <- unserialize_results_text('data/speaker_p_17_19_Hofreiter18_Laemmel18.txt')

res_all$documents[,'omega'] <- res_all$documents[,'omega'] - res_gt$documents[,'omega']

plot_speaker_speeches_by_party(raw=data, res=res_all, filename='data/speaker_p17_19_Hofreiter18_Laemmel18.png', TRUE)



min_p <- 13
max_p <- 15
run_name <- paste0(min_p-12, '_wordfish_p_', min_p, '_', max_p, '_nc_100')

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=min_p, max_period=max_p)
data <- filter_speeches(data, 'nc', chars_around=100)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=(min_p != max_p))   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)
t <- colnames(mat)
run_name <- paste0(run_name, '_speaker', '_Trittin14', '_Schaeuble14')
res <- run_wordfish(tdmat=mat,
                    repr_1=266, # anti
                    repr_2=226, # pro
                    name=run_name,
                    tol=1e-7)
# visualization
plot_speaker_speeches_by_party(raw=visualization_copy, res=res, filename=paste0('data/', run_name, '.png'), (min_p != max_p))


run_name <- paste0(min_p-12, '_wordfish_p_', min_p, '_', max_p, '_gt')

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=min_p, max_period=max_p)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=(min_p != max_p))   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)
t <- colnames(mat)
run_name <- paste0(run_name, '_speaker', '_Trittin14', '_Schaeuble14')
res <- run_wordfish(tdmat=mat,
                    repr_1=266, # anti
                    repr_2=226, # pro
                    name=run_name,
                    tol=1e-7)
# visualization
plot_speaker_speeches_by_party(raw=visualization_copy, res=res, filename=paste0('data/', run_name, '.png'), (min_p != max_p))




min_p <- 14
max_p <- 16
run_name <- paste0(min_p-12, '_wordfish_p_', min_p, '_', max_p, '_nc_80')

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=min_p, max_period=max_p)
data <- filter_speeches(data, 'nc', chars_around=80)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=(min_p != max_p))   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)
t <- colnames(mat)
run_name <- paste0(run_name, '_speaker', '_Kuenast15', '_Merkel15')
res <- run_wordfish(tdmat=mat,
                    repr_1=199, # anti
                    repr_2=226, # pro
                    name=run_name,
                    tol=1e-7)
# visualization
plot_speaker_speeches_by_party(raw=visualization_copy, res=res, filename=paste0('data/', run_name, '.png'), (min_p != max_p))


run_name <- paste0(min_p-12, '_wordfish_p_', min_p, '_', max_p, '_gt')

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=min_p, max_period=max_p)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=(min_p != max_p))   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)
t <- colnames(mat)
run_name <- paste0(run_name, '_speaker', '_Kuenast15', '_Merkel15')
res <- run_wordfish(tdmat=mat,
                    repr_1=199, # anti
                    repr_2=226, # pro
                    name=run_name,
                    tol=1e-7)
# visualization
plot_speaker_speeches_by_party(raw=visualization_copy, res=res, filename=paste0('data/', run_name, '.png'), (min_p != max_p))




min_p <- 15
max_p <- 17
run_name <- paste0(min_p-12, '_wordfish_p_', min_p, '_', max_p, '_nc_80')

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=min_p, max_period=max_p)
data <- filter_speeches(data, 'nc', chars_around=80)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=(min_p != max_p))   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)
t <- colnames(mat)
run_name <- paste0(run_name, '_speaker', '_Kuenast16', '_Merkel16')
res <- run_wordfish(tdmat=mat,
                    repr_1=169, # anti
                    repr_2=185, # pro
                    name=run_name,
                    tol=1e-7)
# visualization
plot_speaker_speeches_by_party(raw=visualization_copy, res=res, filename=paste0('data/', run_name, '.png'), (min_p != max_p))

run_name <- paste0(min_p-12, '_wordfish_p_', min_p, '_', max_p, '_gt')

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=min_p, max_period=max_p)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=(min_p != max_p))   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)
t <- colnames(mat)
run_name <- paste0(run_name, '_speaker', '_Kuenast16', '_Merkel16')
res <- run_wordfish(tdmat=mat,
                    repr_1=169, # anti
                    repr_2=185, # pro
                    name=run_name,
                    tol=1e-7)
# visualization
plot_speaker_speeches_by_party(raw=visualization_copy, res=res, filename=paste0('data/', run_name, '.png'), (min_p != max_p))




min_p <- 16
max_p <- 18
run_name <- paste0(min_p-12, '_wordfish_p_', min_p, '_', max_p, '_nc_80')

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=min_p, max_period=max_p)
data <- filter_speeches(data, 'nc', chars_around=80)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=(min_p != max_p))   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)
t <- colnames(mat)
run_name <- paste0(run_name, '_speaker', '_Kuenast17', '_Merkel17')
res <- run_wordfish(tdmat=mat,
                    repr_1=182, # anti
                    repr_2=171, # pro
                    name=run_name,
                    tol=1e-7)
# visualization
plot_speaker_speeches_by_party(raw=visualization_copy, res=res, filename=paste0('data/', run_name, '.png'), (min_p != max_p))


run_name <- paste0(min_p-12, '_wordfish_p_', min_p, '_', max_p, '_gt')

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=min_p, max_period=max_p)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=(min_p != max_p))   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)
t <- colnames(mat)
run_name <- paste0(run_name, '_speaker', '_Kuenast17', '_Merkel17')
res <- run_wordfish(tdmat=mat,
                    repr_1=182, # anti
                    repr_2=171, # pro
                    name=run_name,
                    tol=1e-7)
# visualization
plot_speaker_speeches_by_party(raw=visualization_copy, res=res, filename=paste0('data/', run_name, '.png'), (min_p != max_p))



min_p <- 17
max_p <- 19
run_name <- paste0(min_p-12, '_wordfish_p_', min_p, '_', max_p, '_nc_100')

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=min_p, max_period=max_p)
data <- filter_speeches(data, 'nc', chars_around=100)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=(min_p != max_p))   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)
t <- colnames(mat)
run_name <- paste0(run_name, '_speaker', '_Oezdemir18', '_Laemmel18')
res <- run_wordfish(tdmat=mat,
                    repr_1=295, # anti
                    repr_2=188, # pro
                    name=run_name,
                    tol=1e-7)
# visualization
plot_speaker_speeches_by_party(raw=visualization_copy, res=res, filename=paste0('data/', run_name, '.png'), (min_p != max_p))


run_name <- paste0(min_p-12, '_wordfish_p_', min_p, '_', max_p, '_gt')

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=min_p, max_period=max_p)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=(min_p != max_p))   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)
t <- colnames(mat)
run_name <- paste0(run_name, '_speaker', '_Oezdemir18', '_Laemmel18')
res <- run_wordfish(tdmat=mat,
                    repr_1=295, # anti
                    repr_2=188, # pro
                    name=run_name,
                    tol=1e-7)
# visualization
plot_speaker_speeches_by_party(raw=visualization_copy, res=res, filename=paste0('data/', run_name, '.png'), (min_p != max_p))



res_13_gt <- unserialize_results_text('data/29-08/1_wordfish_p_13_15_gt_speaker_Trittin14_Schaeuble14.txt')
res_14_gt <- unserialize_results_text('data/29-08/2_wordfish_p_14_16_gt_speaker_Kuenast15_Merkel15.txt')
res_15_gt <- unserialize_results_text('data/29-08/3_wordfish_p_15_17_gt_speaker_Kuenast16_Merkel16.txt')
res_16_gt <- unserialize_results_text('data/29-08/4_wordfish_p_16_18_gt_speaker_Kuenast17_Merkel17.txt')
res_17_gt <- unserialize_results_text('data/29-08/5_wordfish_p_17_19_gt_speaker_Oezdemir18_Laemmel18.txt')

res_13_nc <- unserialize_results_text('data/29-08/1_wordfish_p_13_15_nc_100_speaker_Trittin14_Schaeuble14.txt')
res_14_nc <- unserialize_results_text('data/29-08/2_wordfish_p_14_16_nc_80_speaker_Kuenast15_Merkel15.txt')
res_15_nc <- unserialize_results_text('data/29-08/3_wordfish_p_15_17_nc_80_speaker_Kuenast16_Merkel16.txt')
res_16_nc <- unserialize_results_text('data/29-08/4_wordfish_p_16_18_nc_80_speaker_Kuenast17_Merkel17.txt')
res_17_nc <- unserialize_results_text('data/29-08/5_wordfish_p_17_19_nc_100_speaker_Oezdemir18_Laemmel18.txt')

mask_13_13 <- ! (names(res_13_gt$documents[,'omega']) %in% names(res_14_gt$documents[, 'omega'])) & ! (names(res_13_gt$documents[,'omega']) %in% names(res_15_gt$documents[, 'omega']))
mask_13_14 <- (names(res_13_gt$documents[,'omega']) %in% names(res_14_gt$documents[, 'omega'])) & ! (names(res_13_gt$documents[,'omega']) %in% names(res_15_gt$documents[, 'omega']))
mask_13_15 <- (names(res_13_gt$documents[,'omega']) %in% names(res_14_gt$documents[, 'omega'])) & (names(res_13_gt$documents[,'omega']) %in% names(res_15_gt$documents[, 'omega']))

mask_14_14 <- ! (names(res_14_gt$documents[,'omega']) %in% names(res_15_gt$documents[, 'omega'])) & ! (names(res_14_gt$documents[,'omega']) %in% names(res_16_gt$documents[, 'omega']))
mask_14_15 <- (names(res_14_gt$documents[,'omega']) %in% names(res_15_gt$documents[, 'omega'])) & ! (names(res_14_gt$documents[,'omega']) %in% names(res_16_gt$documents[, 'omega']))
mask_14_16 <- (names(res_14_gt$documents[,'omega']) %in% names(res_15_gt$documents[, 'omega'])) & (names(res_14_gt$documents[,'omega']) %in% names(res_16_gt$documents[, 'omega']))

mask_15_15 <- ! (names(res_15_gt$documents[,'omega']) %in% names(res_16_gt$documents[, 'omega'])) & ! (names(res_15_gt$documents[,'omega']) %in% names(res_17_gt$documents[, 'omega']))
mask_15_16 <- (names(res_15_gt$documents[,'omega']) %in% names(res_16_gt$documents[, 'omega'])) & ! (names(res_15_gt$documents[,'omega']) %in% names(res_17_gt$documents[, 'omega']))
mask_15_17 <- (names(res_15_gt$documents[,'omega']) %in% names(res_16_gt$documents[, 'omega'])) & (names(res_15_gt$documents[,'omega']) %in% names(res_17_gt$documents[, 'omega']))

mask_16_16 <- ! (names(res_16_gt$documents[,'omega']) %in% names(res_17_gt$documents[, 'omega']))
mask_16_17 <- (names(res_16_gt$documents[,'omega']) %in% names(res_17_gt$documents[, 'omega'])) & (names(res_16_gt$documents[,'omega']) %in% names(res_15_gt$documents[, 'omega']))
mask_16_18 <- (names(res_16_gt$documents[,'omega']) %in% names(res_17_gt$documents[, 'omega'])) & !mask_16_17

mask_17_17 <- (names(res_17_gt$documents[,'omega']) %in% names(res_15_gt$documents[, 'omega']))
mask_17_18 <- (names(res_17_gt$documents[,'omega']) %in% names(res_16_gt$documents[, 'omega'])) & !mask_17_17
mask_17_19 <- !mask_17_17 & !mask_17_18


avg_13_gt <- res_13_gt$documents[,'omega'][mask_13_13]
avg_13_nc <- res_13_nc$documents[,'omega'][mask_13_13]

avg_14_gt <- (res_13_gt$documents[,'omega'][mask_13_14] + res_14_gt$documents[,'omega'][mask_14_14]) / 2
avg_14_nc <- (res_13_nc$documents[,'omega'][mask_13_14] + res_14_nc$documents[,'omega'][mask_14_14]) / 2

avg_15_gt <- (res_13_gt$documents[,'omega'][mask_13_15] + res_14_gt$documents[,'omega'][mask_14_15] + res_15_gt$documents[,'omega'][mask_15_15]) / 3
avg_15_nc <- (res_13_nc$documents[,'omega'][mask_13_15] + res_14_nc$documents[,'omega'][mask_14_15] + res_15_nc$documents[,'omega'][mask_15_15]) / 3

avg_16_gt <- (res_14_gt$documents[,'omega'][mask_14_16] + res_15_gt$documents[,'omega'][mask_15_16] + res_16_gt$documents[,'omega'][mask_16_16]) / 3
avg_16_nc <- (res_14_nc$documents[,'omega'][mask_14_16] + res_15_nc$documents[,'omega'][mask_15_16] + res_16_nc$documents[,'omega'][mask_16_16]) / 3

avg_17_gt <- (res_15_gt$documents[,'omega'][mask_15_17] + res_16_gt$documents[,'omega'][mask_16_17] + res_17_gt$documents[,'omega'][mask_17_17]) / 3
avg_17_nc <- (res_15_nc$documents[,'omega'][mask_15_17] + res_16_nc$documents[,'omega'][mask_16_17] + res_17_nc$documents[,'omega'][mask_17_17]) / 3

avg_18_gt <- (res_16_gt$documents[,'omega'][mask_16_18] + res_17_gt$documents[,'omega'][mask_17_18]) / 2
avg_18_nc <- (res_16_nc$documents[,'omega'][mask_16_18] + res_17_nc$documents[,'omega'][mask_17_18]) / 2

avg_19_gt <- res_17_gt$documents[,'omega'][mask_17_19]
avg_19_nc <- res_17_nc$documents[,'omega'][mask_17_19]

avg_gt <- avg_13_gt
avg_gt <- append(avg_gt, avg_14_gt)
avg_gt <- append(avg_gt, avg_15_gt)
avg_gt <- append(avg_gt, avg_16_gt)
avg_gt <- append(avg_gt, avg_17_gt)
avg_gt <- append(avg_gt, avg_18_gt)
avg_gt <- append(avg_gt, avg_19_gt)

avg_nc <- avg_13_nc
avg_nc <- append(avg_nc, avg_14_nc)
avg_nc <- append(avg_nc, avg_15_nc)
avg_nc <- append(avg_nc, avg_16_nc)
avg_nc <- append(avg_nc, avg_17_nc)
avg_nc <- append(avg_nc, avg_18_nc)
avg_nc <- append(avg_nc, avg_19_nc)

carrier_mat <- matrix(ncol=2, nrow=length(avg_gt))
colnames(carrier_mat) <- c('omega', 'alpha')

avg_gt_carrier <- list('documents'=carrier_mat)
avg_gt_carrier$documents[,'omega'] <- avg_gt

avg_nc_carrier <- list('documents'=carrier_mat)
avg_nc_carrier$documents[,'omega'] <- avg_nc

res_13_diff <- res_13_gt
res_14_diff <- res_14_gt
res_15_diff <- res_15_gt
res_16_diff <- res_16_gt
res_17_diff <- res_17_gt

res_all_diff <- list('documents'=carrier_mat)


res_13_diff$documents[,'omega'] <- res_13_gt$documents[,'omega'] - res_13_nc$documents[,'omega']
res_14_diff$documents[,'omega'] <- res_14_gt$documents[,'omega'] - res_14_nc$documents[,'omega']
res_15_diff$documents[,'omega'] <- res_15_gt$documents[,'omega'] - res_15_nc$documents[,'omega']
res_16_diff$documents[,'omega'] <- res_16_gt$documents[,'omega'] - res_16_nc$documents[,'omega']
res_17_diff$documents[,'omega'] <- res_17_gt$documents[,'omega'] - res_17_nc$documents[,'omega']

res_all_diff$documents[,'omega'] <- avg_gt_carrier$documents[,'omega'] - avg_nc_carrier$documents[,'omega']
rownames(res_all_diff$documents) <- names(avg_gt)

data <- read_speeches('data/database_export_search_89.csv')
data_13 <- filter_speeches(data, 'p', min_period=13, max_period=15)
data_14 <- filter_speeches(data, 'p', min_period=14, max_period=16)
data_15 <- filter_speeches(data, 'p', min_period=15, max_period=17)
data_16 <- filter_speeches(data, 'p', min_period=16, max_period=18)
data_17 <- filter_speeches(data, 'p', min_period=17, max_period=19)
data_all <- filter_speeches(data, 'p', min_period=13, max_period=19)

plot_speaker_speeches_by_party(raw=data_13, res=res_13_diff, filename='data/1_speaker_p13_15_diff.png', TRUE)
plot_speaker_speeches_by_party(raw=data_14, res=res_14_diff, filename='data/2_speaker_p14_16_diff.png', TRUE)
plot_speaker_speeches_by_party(raw=data_15, res=res_15_diff, filename='data/3_speaker_p15_17_diff.png', TRUE)
plot_speaker_speeches_by_party(raw=data_16, res=res_16_diff, filename='data/4_speaker_p16_18_diff.png', TRUE)
plot_speaker_speeches_by_party(raw=data_17, res=res_17_diff, filename='data/5_speaker_p17_19_diff.png', TRUE)

plot_speaker_speeches_by_party(raw=data_all, res=res_all_diff, filename='data/0_speaker_p13_19_diff.png', TRUE)


s <- res_13_gt$documents[,'omega'][t]


min_p <- 18
max_p <- 19
run_name <- paste0(min_p-12, '_wordfish_p_', min_p, '_', max_p, '_nc_100')

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=min_p, max_period=max_p)
data <- filter_speeches(data, 'nc', chars_around=100)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=(min_p != max_p))   # group the speeches by their party


# best result Wordfish
data <- read_speeches('data/database_export_search_89.csv')
data_17 <- filter_speeches(data, 'p', min_period=17, max_period=19)

res_17_gt <- unserialize_results_text('data/07-06/speaker_p_17_19_Hofreiter18_Laemmel18.txt')
res_17_nc <- unserialize_results_text('data/07-06/speaker_p_17_19_nc_100_Hofreiter18_Laemmel18.txt')
res_17_diff <- res_17_gt
res_17_diff$documents[,'omega'] <- res_17_gt$documents[,'omega'] - res_17_nc$documents[,'omega']

plot_speaker_speeches_by_party(data_17, res_17_diff, 'data/final_report/wordfish_best.png', TRUE)

# best wordscores result
library(readr)

data <- read_speeches('data/database_export_search_89.csv')
data_17 <- filter_speeches(data, 'p', min_period=17, max_period=19)

cols <- cols(SpeechDbId=col_integer(),
             CreatedScore=col_integer())
res <- list(result=read_csv('data/wordscores_rounded_p_18_19_co_500_sw_1_25_no_numbers_scores_rounded_result.csv', col_types=cols))
plot_speeches_by_party(raw=data_17, res=res, filename=paste0('data/final_report/wordscores_best.png'), groupedByParty=FALSE, multiple_periods=TRUE)


# initial result (all periods)

data <- read_speeches('data/database_export_search_89.csv')

res <- unserialize_results_text('data/long_result.txt')
plot_speeches_by_party(data, res, 'data/final_report/all_speeches_result.png')


# best wordscores run for model visualization

co <- 500
sw_min <- 0.01
sw_max <- 0.25
min_p <- 18
max_p <- 19
use_only_extrema <- FALSE

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=min_p, max_period=max_p)
data <- filter_speeches(data, 'co', chars_around=co)
data <- filter_speeches(data,'sw', min_pct=sw_min, max_pct=sw_max)
raw <- data
reference <- read_reference_speeches(use_rounded_scores=TRUE)
reference <- attach_speeches(data, reference)
if (use_only_extrema) {
  reference <- reference[abs(reference$CoalScore) >= 2,]
}

run_name <- paste0('final_report/wordscores_rounded_p_', min_p, '_', max_p, '_co_', co, '_sw_', round(sw_min*100), '_', round(sw_max*100), '_no_numbers_scores_rounded')

res <- run_wordscores(data, reference, run_name, round_result=TRUE, return_model=TRUE)

plot_word_weights(res$model, run_name)





dataset <- data

library(tm)

corpus <- VCorpus(VectorSource(dataset$Speech), readerControl=list(language='ger'))
if (remove_numbers) {
  corpus <- tm_map(corpus, removeNumbers) # REMOVE NUMBERS
}
corpus <- tm_map(corpus, stripWhitespace) # REMOVE EXTRA WHITE SPACE
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(gsub), pattern="[^a-zA-Z0-9äöüÄÖÜß ]", replacement='')
if (stem_speeches) {
  corpus <- tm_map(corpus, content_transformer(stem_speech))
} else {
  corpus <- tm_map(corpus, content_transformer(tolower)) # MAKES EVERYTHING LOWERCASE
}

freq_mat <- TermDocumentMatrix(corpus, control=list(tolower=FALSE))
sparce_mat <- removeSparseTerms(freq_mat, 0.9999)
freq <- as.matrix(sparce_mat)
colnames(freq) <- dataset$ID