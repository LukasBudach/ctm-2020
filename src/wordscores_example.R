# Title     : Wordscores example usage
# Objective : Demonstrate how to use the Wordscores model in this project


##############################################
######## Example full wordscores run #########
##############################################
source('src/wordscores/io.R')
source('src/wordscores/utils.R')
source('src/wordscores/visualization.R')
source('src/wordscores/wordscores.R')
source('src/utils/filtering.R')

co <- 500
sw_min <- 0.01
sw_max <- 0.25
min_p <- 18
max_p <- 19
rounded_ref <- TRUE
rounded_res <- TRUE
remove_numbers <- TRUE
use_only_extrema <- FALSE

data <- read_speeches('data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=min_p, max_period=max_p)
data <- filter_speeches(data, 'co', chars_around=co)
data <- filter_speeches(data,'sw', min_pct=sw_min, max_pct=sw_max)
raw <- data
reference <- read_reference_speeches(use_rounded_scores=rounded_ref)
reference <- attach_speeches(data, reference)

if (use_only_extrema) {
  reference <- reference[abs(reference$CoalScore) >= 2,]
}

run_name <- paste0('wordscores_',
                   ifelse(rounded_res, 'rounded_', ''),
                   'p_', min_p, '_', max_p, '_',
                   'co_', co, '_',
                   'sw_',
                   strsplit(as.character(sw_min), "\\.")[[1]][2], '_',
                   strsplit(as.character(sw_max), "\\.")[[1]][2], '_',
                   ifelse(remove_numbers, 'no_numbers_', ''),
                   'scores_',
                   ifelse(use_only_extrema, 'extremes_', ''),
                   ifelse(rounded_ref, 'rounded_', 'raw_'))

res <- run_wordscores(data, reference, run_name, round_result=rounded_res, return_scored_reference_separately=TRUE,
                      return_model=TRUE, remove_numbers=remove_numbers)

plot_speeches_by_party(raw=raw, res=res, filename=paste0('data/', run_name, '.png'), multiple_periods=TRUE)
plot_results_vs_expected(res$ref_result, run_name)
plot_word_weights(res$model, run_name)

#####################################
###### Example funnel diagram #######
#####################################
source('src/utils/visualize_vocabulary.R')

labels <- c('original', 'co_500', 'sw_0.01_0.25')
wc <- prepare_pipelined_funnel(labels, min_period=18, max_period=19)
plot_funnel_diagram(labels, wc, 'data/funnel_pipeline_co_500_sw_01_25.png', pipelined=TRUE)

#################################################
###### Example finding optimal parameters #######
#################################################
source('src/wordscores/utils.R')

results <- find_optimum(400, 410, 0.068, 0.082, 0.45, 0.46,
                        50,0.001, 0.02, 'data/wordscores_optim_specific.csv',
                        19, 19)

