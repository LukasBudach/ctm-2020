# Title     : TODO
# Objective : TODO
# Created by: lukas
# Created on: 13/05/2020


# random stuff
for(i in seq(min(data$Period), max(data$Period))) {
  print(sprintf('Period: %d speeches: %d', i, nrow(filter_period(data, i))))
}

# read the data
data <- read_speeches('data/database_export_search_89.csv')
d_19 <- filter_period(data, 19)

# use for coal percentages
coal_counts <- filter_coal_percentile(d_19, 0.005)
d_19_coal_perc <- d_19[d_19$SpeechDbId %in% coal_counts$SpeechDbId,]
speeches <- concat_by_speaker(d_19_coal_perc)
mat <- get_frequency_matrix(speeches, 0.9999)
res <- wordfish(mat, dir=c(46, 30))
serialize_results_text('data/speakers_coal_pct_05_Wirth_Movassat.txt', res)
speeches_by_party(d_19, res, 'data/speakers_coal_pct_05_Wirth_Movassat.png')


# use for coal counts
coal_counts <- filter_coal_count(d_19, 3)
d_19_coal_count <- d_19[d_19$SpeechDbId %in% coal_counts$SpeechDbId,]
speeches <- concat_by_speaker(d_19_coal_count)
mat <- get_frequency_matrix(speeches, 0.9999)
res <- wordfish(mat, dir=c(32, 6))
serialize_results_text('data/speakers_coal_count_3_Kotre_Beutin.txt', res)
speeches_by_party(d_19, res, 'data/speakers_coal_count_3_Kotre_Beutin.png')


res <- unserialize_results_text('data/test_speakers_19_fixtwo.txt')

library(stringr)

cts <- data.frame()
for(i in seq(1, nrow(speeches))) {
  cts <- rbind(cts, list(speeches$ID[i], str_count(speeches$Speech[i], 'K*k*ohle') / sapply(strsplit(speeches$Speech[i], " "), length)))
}
colnames(cts) <- c('ID', 'CoalPct')

library(stringr)

cts <- data.frame()
for(i in seq(1, nrow(speeches))) {
  cts <- rbind(cts, list(speeches$ID[i], str_count(speeches$Speech[i], 'K*k*ohle')))
}
colnames(cts) <- c('ID', 'CoalCount')

cts <- cts[cts$CoalCount >= 3,]

d_19_coal <- d_19[d_19$SpeechDbId %in% cts$SpeechDbId,]

