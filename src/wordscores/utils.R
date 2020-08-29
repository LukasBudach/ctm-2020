# Title     : Wordscores utility functions
# Objective : Implement required utility and misc functions for Wordscores

source('src/wordscores/io.R')
source('src/wordscores/wordscores.R')


attach_speeches <- function(raw_speeches, reference_speeches) {
  # matches the IDs of the reference speeches in the raw_speeches dataframe to add the speech itself to the
  # reference_speeches dataframe

  ids_in_both <- intersect(raw_speeches$SpeechDbId, reference_speeches$SpeechDbId)
  reference_speeches <- reference_speeches[reference_speeches$SpeechDbId %in% ids_in_both,]
  reference_speeches$Speech <- raw_speeches[raw_speeches$SpeechDbId %in% reference_speeches$SpeechDbId,]$Speech
  return(reference_speeches)
}


find_optimum <- function(min_co, max_co, min_min_pct, max_min_pct, min_max_pct, max_max_pct, step_co, step_min_pct,
                         step_max_pct, filepath, min_period, max_period, use_only_extrema=FALSE) {
  # only for development, change as needed
  # used to test ranges of preprocessing parameters and calculate test metrics in order to optimize the preprocessing
  # for a Wordscores model

  source('src/utils/filtering.R')

  library(ggpubr)

  results <- as.data.frame(matrix(nrow=0, ncol=13))
  colnames(results) <- c('roundedRes', 'roundedRef', 'co', 'swMin', 'swMax', 'meanSquared', '05pct', '25pct', 'mean',
                         '75pct', '95pct', 'pearsonCoeff', 'pValue')
  data <- read_speeches('data/database_export_search_89.csv')

  for (round_result in c(TRUE, FALSE)) {
    for (use_rounded_scores in c(TRUE, FALSE)) {
      for (i in seq(min_co, max_co, step_co)) {
        for (j in seq(min_min_pct, max_min_pct, step_min_pct)) {
          for (k in seq(min_max_pct, max_max_pct, step_max_pct)) {
            data_filtered <- filter_speeches(data, 'p', min_period=min_period, max_period=max_period)
            data_filtered <- filter_speeches(data_filtered, 'co', chars_around=i)
            data_filtered <- filter_speeches(data_filtered,'sw', min_pct=j, max_pct=k)
            scored <- read_reference_speeches(use_rounded_scores)
            scored <- attach_speeches(data_filtered, scored)
            if (use_only_extrema) {
              scored <- scored[abs(scored$CoalScore) >= 2,]
            }
            cat('\n----------------------------------------------\n\n')
            print(paste('round res', round_result, 'round ref', use_rounded_scores, 'co', i, 'min', j, 'max', k))
            res <- run_wordscores(data_filtered, scored, 'optimization_run', return_errors=TRUE,
                                  return_scored_reference_separately=TRUE, round_result=round_result)

            squared_err <- res$errors * res$errors
            pearson <- cor.test(res$ref_result$ManualScore, res$ref_result$CreatedScore, method='pearson')

            results <- rbind(results, list(round_result, use_rounded_scores, i, j, k, mean(squared_err),
                                           quantile(res$errors, 0.05), quantile(res$errors, 0.25), mean(res$errors),
                                           quantile(res$errors, 0.75), quantile(res$errors, 0.95),
                                           pearson$estimate[[1]], pearson$p.value))
            colnames(results) <- c('roundedRes', 'roundedRef', 'co', 'swMin', 'swMax', 'meanSquared', '05pct',
                                   '25pct', 'mean', '75pct', '95pct', 'pearsonCoeff', 'pValue')
          }
        }
      }
    }
  }

  write_csv(results, path=filepath)
  return(results)
}
