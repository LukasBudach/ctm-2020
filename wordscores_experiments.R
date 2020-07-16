##############################################
######## Example full wordscores run #########
##############################################

co <- 400
sw_min <- 0.07
sw_max <- 0.45
use_only_extrema <- FALSE

data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=19, max_period=19)
data <- filter(data, 'co', chars_around=co)
data <- filter(data,'sw', min_pct=sw_min, max_pct=sw_max)
raw <- data
reference <- read_reference_speeches(use_rounded_scores=FALSE)
reference <- attach_speeches(data, reference)
if (use_only_extrema) {
  reference <- reference[abs(reference$CoalScore) >= 2,]
}

run_name <- paste0('wordscores_rounded_co', co, '_sw_', round(sw_min*100), '_', round(sw_max*100), '_no_numbers_scores_raw')

res <- run_wordscores(data, reference, run_name, round_result=TRUE, return_scored_reference_separately=TRUE,
                      return_model=TRUE)

plot_speeches_by_party(raw=raw, res=res, filename='data/wordscores_speaker_19_rounded.png', groupedByParty=FALSE)
plot_results_vs_expected(res$refResult, run_name)
plot_word_weights(res$model, run_name)

#####################################
###### Example funnel diagram #######
#####################################

labels <- c('original', 'co_400', 'sw_0.07_0.45')
wc <- prepare_pipelined_funnel(labels, min_period=19, max_period=19)
draw_funnel_diagram(labels, wc, 'data/funnel_pipeline_co_400_sw_07_45.png', pipelined=TRUE)

#################################################
###### Example finding optimal parameters #######
#################################################

results <- find_optimum(400, 410, 0.068, 0.082, 0.45, 0.46,
                        50,0.001, 0.02, 'data/wordscores_optim_specific.csv',
                        19, 19)
