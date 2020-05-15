# Title     : TODO
# Objective : TODO
# Created by: lukas
# Created on: 13/05/2020


# random stuff
for(i in seq(min(data$Period), max(data$Period))) {
  print(sprintf('Period: %d speeches: %d', i, nrow(filter_period(data, i))))
}

# actually doing stuff here-
data <- read_speeches('data/database_export_search_89.csv')
d_19 <- filter_period(data, 19)
speeches <- concat_by_speaker(d_19)
speeches <- speeches[speeches$ID %in% d_19$Speaker[(d_19$Party == 'gruene') | (d_19$Party == 'afd') | (d_19$Party == 'cducsu')],]
mat <- get_frequency_matrix(speeches, 0.99)
# res <- wordfish(mat, dir=c(1,2), tol=0.000015)
# Müller, Weidl, Pos-score, neg-score
res <- wordfish(mat, dir=c(28, 45), tol=0.00015)

# write and read the result
serialize_results_text('data/test_speakers_19_dir_gac_3.txt', res)

speeches_by_party(d_19, res, 'data/test_speakers_19_dir_gac_3.png')


res <- unserialize_results_text('data/test_speakers_19_fixtwo.txt')
