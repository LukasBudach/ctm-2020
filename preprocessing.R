# Title     : TODO
# Objective : TODO
# Created by: lukas
# Created on: 13/05/2020

# read the data
data <- read_speeches('data/database_export_search_89.csv')
d_19 <- filter(data, 'p', min_period=19, max_period=19)


# read serialized results
res <- unserialize_results_text('data/speaker_p_17_19_co_200_Hofreiter_Hollnagel.txt')


# run wordfish for coal counts
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=19, max_period=19)     # filter to period 19 only
data <- filter(data, 'cc', threshold=3)                     # filter by coal count for >= 3 times coal per speech
visualization_copy <- data
data <- group_speeches(data, 'speaker')                    # group the speeches by their speaker
mat <- get_frequency_matrix(data, sparse=0.9999)                 # create the TDM
res <- run_wordfish(tdmat=mat,
                    repr_1=32, # representative for position 1
                    repr_2=6,  # representative for position 2
                    name='speaker_p_19_cc_3_Kotre_Beutin',
                    tol=1e-7)
# visualization
speaker_speeches_by_party(raw=visualization_copy, res=res, filename='data/speaker_p_19_cc_3_Kotre_Beutin.png')


# run wordfish for coal percentages
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=19, max_period=19)     # filter to period 19 only
data <- filter(data, 'cp', threshold=0.005)                 # filter by coal percentage >= 0.005 (0.5%)
visualization_copy <- data
data <- group_speeches(data, 'speaker')                    # group the speeches by their speaker
mat <- get_frequency_matrix(data, sparse=0.9999)                 # create the TDM
res <- run_wordfish(tdmat=mat,
                    repr_1=30,    # representative for position 1
                    repr_2=46,    # representative for position 2
                    name='speaker_p_19_cp_0005_Movassat_Wirth',
                    tol=1e-7)
# visualization
speaker_speeches_by_party(raw=visualization_copy, res=res, filename='data/speaker_p_19_cp_0005_Movassat_Wirth.png')


# run wordfish for periods 17-19 without any specific filtering, grouped by party
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)         # filter to period 17-19
visualization_copy <- data
data <- group_speeches(data, 'party', multiple_periods=TRUE)   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)                     # create the TDM
res <- run_wordfish(tdmat=mat,
                    repr_1=7,   # representative for position 1
                    repr_2=9,   # representative for position 2
                    name='party_p_17_19_green18_cdu18',
                    tol=1e-7)
# visualization
party_speeches_by_party(raw=visualization_copy, res=res, filename='data/party_p_17_19_green18_cdu18.png', TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/party_p_17_19_green18_cdu18_words.png')


# run wordfish for periods 17-19 without any specific filtering, grouped by speaker
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)           # filter to period 17-19
data <- group_speeches(data, 'speaker', multiple_periods=TRUE)   # group the speeches by their speaker
mat <- get_frequency_matrix(data, sparse=0.9999)
res <- run_wordfish(tdmat=mat,
                    repr_1=46, # representative for position 1
                    repr_2=30, # representative for position 2
                    name='speaker_p_17_19_Kießling_Roth',
                    tol=1e-7)
# visualization
draw_eiffel_tower_diagram(res=res, filename='data/speaker_p_17_19_Kießling_Roth.png')

# run wordfish for period 19 without any specific filtering, grouped by speaker
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)           # filter to period 17-19
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=TRUE)   # group the speeches by their speaker
mat <- get_frequency_matrix(data, sparse=0.9999)
res <- run_wordfish(tdmat=mat,
                    repr_1=164, # representative for position 1
                    repr_2=188, # representative for position 2
                    name='speaker_p_17_19_Hofreiter18_Laemmel18',
                    tol=1e-7)
# visualization
speaker_speeches_by_party(raw=visualization_copy, res=res, filename='data/speaker_p_17_19_Hofreiter18_Laemmel18.png', multiple_periods=TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/speaker_p_17_19_Hofreiter18_Laemmel18_words.png')


# run wordfish for periods 17-19 on only the text surrounding coal, grouped by party
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)         # filter to period 17-19
data <- filter(data, 'co', chars_around=200)                    # filter speeches to contain only a 100 character space around the keyword coal
visualization_copy <- data
data <- group_speeches(data, 'party', multiple_periods=TRUE)   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999, stem_speeches=FALSE)
res <- run_wordfish(tdmat=mat,
                    repr_1=7, # representative for position 1
                    repr_2=9, # representative for position 2
                    name='party_p_17_19_co_200_green18_cdu18',
                    tol=1e-7)
# visualization
party_speeches_by_party(raw=visualization_copy, res=res, filename='data/party_p_17_19_co_200_green18_cdu18.png', TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/party_p_17_19_co_200_green18_cdu18_words.png')


# run wordfish for periods 17-19 on only the text surrounding coal, grouped by speaker
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)         # filter to period 17-19
data <- filter(data, 'co', chars_around=100)                    # filter speeches to contain only a 100 character space around the keyword coal
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=TRUE) # group the speeches by their speaker
mat <- get_frequency_matrix(data, sparse=0.9999)
res <- run_wordfish(tdmat=mat,
                    repr_1=164, # representative for position 1
                    repr_2=188, # representative for position 2
                    name='speaker_p_17_19_co_100_Hofreiter18_Laemmel18',
                    tol=1e-7)
# visualization
speaker_speeches_by_party(raw=visualization_copy, res=res, filename='data/speaker_p_17_19_co_100_Hofreiter18_Laemmel18.png', TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/speaker_p_17_19_co_100_Hofreiter18_Laemmel18_words.png')


# run wordfish for periods 17-19 on only the text that doesn't include coal, grouped by party
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)         # filter to period 17-19
data <- filter(data, 'nc', chars_around=100)                    # filter speeches to exclude a 100 character space around the keyword coal
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=TRUE) # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)
res <- run_wordfish(tdmat=mat,
                    repr_1=164, # representative for position 1
                    repr_2=188, # representative for position 2
                    name='speaker_p_17_19_nc_100_Hofreiter18_Laemmel18',
                    tol=1e-7)
# visualization
speaker_speeches_by_party(raw=visualization_copy, res=res, filename='data/speaker_p_17_19_nc_100_Hofreiter18_Laemmel18.png', multiple_periods=TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/speaker_p_17_19_nc_100_Hofreiter18_Laemmel18_words.png')


# run wordfish for periods 17-19 on only the text surrounding coal and having remove the parliament president, grouped by party
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)         # filter to period 17-19
data <- filter(data, 'co', chars_around=150)                    # filter speeches to contain only a 100 character space around the keyword coal
data <- filter(data, 'np')                                      # remove speeches by the active Bundestagspräsident
visualization_copy <- data
data <- group_speeches(data, 'party', multiple_periods=TRUE)   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)
res <- run_wordfish(tdmat=mat,
                    repr_1=7, # representative for position 1
                    repr_2=9, # representative for position 2
                    name='party_p_17_19_co_150_np_green18_cdu18',
                    tol=1e-7)
# visualization
party_speeches_by_party(raw=visualization_copy, res=res, filename='data/party_p_17_19_co_150_np_green18_cdu18.png', TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/party_p_17_19_co_150_np_green18_cdu18_words.png')


# run wordfish for periods 17-19 without any specific filtering, grouped by party
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)          # filter to period 17-19
data <- filter(data, 'vp')
data <- filter(data, 'np')
data <- filter(data, 'co', chars_around=100)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=TRUE)  # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)                      # create the TDM
res <- run_wordfish(tdmat=mat,
                    repr_1=7,   # representative for position 1
                    repr_2=9,   # representative for position 2
                    name='speaker_p_17_19_co_100_vp_np_green18_cdu18',
                    tol=1e-7)
speaker_speeches_by_party(raw=visualization_copy, res=res, filename='data/speaker_p_17_19_co_100_vp_np_green18_cdu18.png', TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/speaker_p_17_19_co_100_vp_np_green18_cdu18_words.png')


# funnel diagrams with separately executed filters
labels <- c('origin', 'np', 'vp')
wordcounts <- prepare_normal_funnel(labels, 17, 19, TRUE, 0.9999)
draw_funnel_diagram(labels, wordcounts, 'data/funnel_president.png')

labels <- c('origin', 'nc_100', 'nc_200', 'nc_300', 'nc_400', 'nc_500', 'nc_600')
wordcounts <- prepare_normal_funnel(labels, 17, 19, TRUE, 0.9999)
draw_funnel_diagram(labels, wordcounts, 'data/funnel_no_coal.png')

labels <- c('origin', 'co_600', 'co_500', 'co_400', 'co_300', 'co_200', 'co_100')
wordcounts <- prepare_normal_funnel(labels, 17, 19, TRUE, 0.9999)
draw_funnel_diagram(labels, wordcounts, 'data/funnel_coal_only.png')


# funnel diagrams with pipelined filters
labels <- c('original', 'co_300', 'vp', 'np')
wordcounts <- prepare_pipelined_funnel(labels, 17, 19, TRUE, 0.9999)
draw_funnel_diagram(labels, wordcounts, 'data/funnel_pipeline_co_300_vp_np.png', TRUE)

labels <- c('original', 'vp', 'np', 'co_300')
wordcounts <- prepare_pipelined_funnel(labels, 17, 19, TRUE, 0.9999)
draw_funnel_diagram(labels, wordcounts, 'data/funnel_pipeline_vp_np_co_300.png', TRUE)

# run wordfish with extremes for period 18 and 19, grouped by speaker
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=18, max_period=19)
scored <- read_scored_extremes('data/scored_extremes_rounded.csv')
ids_in_both <- intersect(data$SpeechDbId, scored$SpeechDbId)
scored <- scored[scored$SpeechDbId %in% ids_in_both,]
scored$Speech <- data[data$SpeechDbId %in% scored$SpeechDbId,]$Speech

pro_coal <- scored[scored$CoalScore > 0,]
neutral <- scored[scored$CoalScore == 0,]
anti_coal <- scored[scored$CoalScore < 0,]
data <- rbind(data, list(as.integer(0), as.integer(0), '2018-06-21', as.integer(18), as.integer(1), as.integer(0), 'ProCoal', 'scoredPro', as.integer(0), '', as.integer(0), paste(pro_coal$Speech, collapse=' ')))
data <- rbind(data, list(as.integer(1), as.integer(1), '2018-06-21', as.integer(18), as.integer(1), as.integer(1), 'Neutral', 'scoredNeutral', as.integer(0), '', as.integer(0), paste(neutral$Speech, collapse=' ')))
data <- rbind(data, list(as.integer(2), as.integer(2), '2018-06-21', as.integer(18), as.integer(1), as.integer(2), 'AntiCoal', 'scoredAnti', as.integer(0), '', as.integer(0), paste(anti_coal$Speech, collapse=' ')))

data <- filter(data, 'co', chars_around=300)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=TRUE)
mat <- get_frequency_matrix(data, sparse=0.9999)                 # create the TDM

res <- wordfish(input=mat, fixtwo=TRUE, fixdoc=c(13, 15, 2, -2), sigma=1, tol=5e-6)
f <- file('data/party_p_18_19_co_300_scoredPro_scoredAnti.txt', 'w+')
serialize(connection=f, object=res, ascii=TRUE)
close(f)

res <- run_wordfish(tdmat=mat,
                    repr_1=13,  # extreme pro
                    repr_2=15,  # extreme anti
                    name='party_p_18_19_co_300_scoredPro_scoredAnti',
                    tol=5e-6)

f <- file('data/party_p_18_19_co_300_scoredPro_scoredAnti.txt', 'r')
res <- unserialize(f)
close(f)

speaker_speeches_by_party_extremes(res, filename='data/party_p_18_19_co_300_scoredPro_scoredAnti.png', multiple_periods=TRUE)

# run wordfish with extremes for period 18, grouped by speaker
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=18, max_period=18)
scored <- read_scored_extremes('data/scored_extremes_rounded.csv')
ids_in_both <- intersect(data$SpeechDbId, scored$SpeechDbId)
scored <- scored[scored$SpeechDbId %in% ids_in_both,]
scored$Speech <- data[data$SpeechDbId %in% scored$SpeechDbId,]$Speech

pro_coal <- scored[scored$CoalScore > 0,]
neutral <- scored[scored$CoalScore == 0,]
anti_coal <- scored[scored$CoalScore < 0,]
data <- rbind(data, list(as.integer(0), as.integer(0), '2018-06-21', as.integer(18), as.integer(1), as.integer(0), 'ProCoal', 'scoredPro', as.integer(0), '', as.integer(0), paste(pro_coal$Speech, collapse=' ')))
data <- rbind(data, list(as.integer(1), as.integer(1), '2018-06-21', as.integer(18), as.integer(1), as.integer(1), 'Neutral', 'scoredNeutral', as.integer(0), '', as.integer(0), paste(neutral$Speech, collapse=' ')))
data <- rbind(data, list(as.integer(2), as.integer(2), '2018-06-21', as.integer(18), as.integer(1), as.integer(2), 'AntiCoal', 'scoredAnti', as.integer(0), '', as.integer(0), paste(anti_coal$Speech, collapse=' ')))

data <- filter(data, 'co', chars_around=300)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=FALSE)
mat <- get_frequency_matrix(data, sparse=0.9999)                 # create the TDM

res <- wordfish(input=mat, fixtwo=TRUE, fixdoc=c(146, 148, 2, -2), sigma=1, tol=5e-6)
f <- file('data/party_p_18_co_300_scoredPro_scoredAnti.txt', 'w+')
serialize(connection=f, object=res, ascii=TRUE)
close(f)

res <- run_wordfish(tdmat=mat,
                    repr_1=13,  # extreme pro
                    repr_2=15,  # extreme anti
                    name='party_p_18_co_300_scoredPro_scoredAnti',
                    tol=5e-6)

f <- file('data/party_p_18_co_300_scoredPro_scoredAnti.txt', 'r')
res <- unserialize(f)
close(f)

speaker_speeches_by_party_extremes(raw=visualization_copy, res=res, filename='data/party_p_18_co_300_scoredPro_scoredAnti.png', multiple_periods=FALSE)

# run wordfish with extremes for period 19, grouped by speaker
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=19, max_period=19)
scored <- read_scored_extremes('data/scored_extremes_rounded.csv')
ids_in_both <- intersect(data$SpeechDbId, scored$SpeechDbId)
scored <- scored[scored$SpeechDbId %in% ids_in_both,]
scored$Speech <- data[data$SpeechDbId %in% scored$SpeechDbId,]$Speech

pro_coal <- scored[scored$CoalScore > 0,]
neutral <- scored[scored$CoalScore == 0,]
anti_coal <- scored[scored$CoalScore < 0,]
data <- rbind(data, list(as.integer(0), as.integer(0), '2018-06-21', as.integer(19), as.integer(1), as.integer(0), 'ProCoal', 'scoredPro', as.integer(0), '', as.integer(0), paste(pro_coal$Speech, collapse=' ')))
data <- rbind(data, list(as.integer(1), as.integer(1), '2018-06-21', as.integer(19), as.integer(1), as.integer(1), 'Neutral', 'scoredNeutral', as.integer(0), '', as.integer(0), paste(neutral$Speech, collapse=' ')))
data <- rbind(data, list(as.integer(2), as.integer(2), '2018-06-21', as.integer(19), as.integer(1), as.integer(2), 'AntiCoal', 'scoredAnti', as.integer(0), '', as.integer(0), paste(anti_coal$Speech, collapse=' ')))

data <- filter(data, 'co', chars_around=300)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=FALSE)
mat <- get_frequency_matrix(data, sparse=0.9999)                 # create the TDM

res <- wordfish(input=mat, fixtwo=TRUE, fixdoc=c(168, 170, 2, -2), sigma=1, tol=5e-6)
f <- file('data/party_p_19_co_300_scoredPro_scoredAnti.txt', 'w+')
serialize(connection=f, object=res, ascii=TRUE)
close(f)

res <- run_wordfish(tdmat=mat,
                    repr_1=13,  # extreme pro
                    repr_2=15,  # extreme anti
                    name='party_p_19_co_300_scoredPro_scoredAnti',
                    tol=5e-6)

f <- file('data/party_p_19_co_300_scoredPro_scoredAnti.txt', 'r')
res <- unserialize(f)
close(f)

speaker_speeches_by_party_extremes(raw=visualization_copy, res=res, filename='data/party_p_19_co_300_scoredPro_scoredAnti.png', multiple_periods=FALSE)

# run quanteda wordfish with extremes for period 19, grouped by speaker
library(quanteda)
library(quanteda.textmodels)

data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=19, max_period=19)
scored <- read_scored_extremes('data/scored_extremes_rounded.csv')
ids_in_both <- intersect(data$SpeechDbId, scored$SpeechDbId)
scored <- scored[scored$SpeechDbId %in% ids_in_both,]
scored$Speech <- data[data$SpeechDbId %in% scored$SpeechDbId,]$Speech

pro_coal <- scored[scored$CoalScore > 0,]
neutral <- scored[scored$CoalScore == 0,]
anti_coal <- scored[scored$CoalScore < 0,]
data <- rbind(data, list(as.integer(0), as.integer(0), '2018-06-21', as.integer(19), as.integer(1), as.integer(0), 'ProCoal', 'scoredPro', as.integer(0), '', as.integer(0), paste(pro_coal$Speech, collapse=' ')))
data <- rbind(data, list(as.integer(1), as.integer(1), '2018-06-21', as.integer(19), as.integer(1), as.integer(1), 'Neutral', 'scoredNeutral', as.integer(0), '', as.integer(0), paste(neutral$Speech, collapse=' ')))
data <- rbind(data, list(as.integer(2), as.integer(2), '2018-06-21', as.integer(19), as.integer(1), as.integer(2), 'AntiCoal', 'scoredAnti', as.integer(0), '', as.integer(0), paste(anti_coal$Speech, collapse=' ')))

data <- filter(data, 'co', chars_around=300)
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=FALSE)
#mat <- get_frequency_matrix(data, sparse=0.9999)
corpus <- get_corpus(data, sparse=0.9999)
dfm <- dfm(corpus, verbose=FALSE)

textmodel_wordfish(
  x=dfm,
  dir = c(1, 2),
  priors = c(Inf, Inf, 3, 1),
  tol = c(1e-06, 1e-08),
  dispersion = "poisson",
  dispersion_level = c("feature", "overall"),
  dispersion_floor = 0,
  sparse = FALSE,
  abs_err = FALSE,
  svd_sparse = TRUE,
  residual_floor = 0.5
)
