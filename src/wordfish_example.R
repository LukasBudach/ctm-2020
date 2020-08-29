# Title     : Wordfish example usage
# Objective : Demonstrate how to use the Wordfish model in this project

##############################################
######## Example simple Wordfish run #########
##############################################

# source('src/utils/preprocessing.R') this is somewhat broken, the functions work properly when executed on their own but not if included using the source command
source('src/utils/filtering.R')
source('src/utils/grouping.R')
source('src/wordfish/io.R')
source('src/wordfish/visualization.R')
source('src/wordfish/wordfish.R')

co <- 100               # filter speeches to contain only a 100 character space around the keyword coal
min_p <- 17
max_p <- 19             # filter to period 17-19
group_mode <- 'speaker' # group the speeches by their speaker

run_name <- paste0('wordfish_',
                   group_mode, '_',
                   'p_', min_p, '_', max_p, '_',
                   'co_', co, '_')

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=min_p, max_period=max_p)
data <- filter_speeches(data, 'co', chars_around=co)
visualization_copy <- data
data <- group_speeches(data, group_mode, multiple_periods=(max_p > min_p))
mat <- get_frequency_matrix(data, sparse=0.9999)
t <- rownames(mat)

# stop execution here, choose appropriate representatives for both positions manually
# the document names (speech ID or speaker or party) are stored in colnames(mat)
# repeated executions have the exact same document order
run_name <- paste0(run_name, 'Hofreiter18_', 'Laemmel18')

res <- run_wordfish(tdmat=mat,
                    repr_1=164, # representative for anti coal position
                    repr_2=188, # representative for pro coal position
                    name=run_name,
                    tol=1e-7)
# visualization
plot_speaker_speeches_by_party(raw=visualization_copy, res=res, filename=paste0('data/', run_name, '.png'), multiple_periods=(max_p > min_p))
plot_word_weights_and_frequencies(res=res, filename=paste0('data/', run_name, '_words.png'))

##############################################
######### Best result Wordfish run ###########
##############################################

# source('src/utils/preprocessing.R') this is somewhat broken, the functions work properly when executed on their own but not if included using the source command
source('src/utils/filtering.R')
source('src/utils/grouping.R')
source('src/wordfish/io.R')
source('src/wordfish/visualization.R')
source('src/wordfish/wordfish.R')

# create a ground truth file
data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=17, max_period=19)
data <- group_speeches(data, 'speaker', 'TRUE')
mat <- get_frequency_matrix(data, sparse=0.9999)

res <- run_wordfish(tdmat=mat,
                    repr_1=164, # anti
                    repr_2=188, # pro
                    name='speaker_p17_19_Hofreiter18_Laemmel18',
                    tol=1e-2)

# create the "difference" score, so score only the non-coal context
data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=17, max_period=19)
data <- filter_speeches(data, 'nc', chars_around=100)
data <- group_speeches(data, 'speaker', 'TRUE')
mat <- get_frequency_matrix(data, sparse=0.9999)

res <- run_wordfish(tdmat=mat,
                    repr_1=164, # anti
                    repr_2=188, # pro
                    name='speaker_p17_19_nc_100_Hofreiter18_Laemmel18',
                    tol=1e-2)

# load the results, subtract and plot the now coal context scores
data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=17, max_period=19)

res_gt <- unserialize_results_text('data/speaker_p17_19_Hofreiter18_Laemmel18.txt')
res_nc <- unserialize_results_text('data/speaker_p17_19_nc_100_Hofreiter18_Laemmel18.txt')
res_diff <- res_gt
res_diff$documents[,'omega'] <- res_gt$documents[,'omega'] - res_nc$documents[,'omega']

plot_speaker_speeches_by_party(data, res_diff, 'data/wordfish_best.png', TRUE)


######################################################################
######### Wordfish with artificially added extreme parties ###########
######################################################################

# source('src/utils/preprocessing.R') this is somewhat broken, the functions work properly when executed on their own but not if included using the source command
source('src/utils/filtering.R')
source('src/utils/grouping.R')
source('src/wordfish/io.R')
source('src/wordfish/visualization.R')
source('src/wordfish/wordfish.R')

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=18, max_period=19)
data <- filter_speeches(data, 'co', chars_around=300)
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
data <- group_speeches(data, 'speaker', multiple_periods=TRUE)
mat <- get_frequency_matrix(data, sparse=0.9999)

res <- run_wordfish(tdmat=mat,
                    repr_1=13,  # extreme pro
                    repr_2=15,  # extreme anti
                    name='speaker_p_18_19_co_300_scoredPro_scoredAnti_5e-6_dir',
                    tol=5e-6)


#####################################
###### Example funnel diagram #######
#####################################
source('src/utils/visualize_vocabulary.R')

labels <- c('origin', 'nc_100', 'nc_200', 'nc_300', 'nc_400', 'nc_500', 'nc_600')
wordcounts <- prepare_normal_funnel(labels, 17, 19, TRUE, 0.9999)
plot_funnel_diagram(labels, wordcounts, 'data/funnel_no_coal.png')
