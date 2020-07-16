read_refence_speeches <- function(use_rounded_scores=FALSE) {
  library(readr)
  col_names <- c('SpeechDbId', 'CoalScore')
  cols <- cols(SpeechDbId=col_integer(), CoalScore=col_double())
  if (use_rounded_scores) {
    filepath <- 'data/scored_extremes_rounded.csv'
  } else {
    filepath <- 'data/scored_extremes.csv'
  }

  return(read_csv(filepath, skip=1, col_names=col_names, col_types=cols))
}


attach_speeches <- function(raw_speeches, reference_speeches) {
  ids_in_both <- intersect(raw_speeches$SpeechDbId, reference_speeches$SpeechDbId)
  reference_speeches <- reference_speeches[reference_speeches$SpeechDbId %in% ids_in_both,]
  reference_speeches$Speech <- raw_speeches[raw_speeches$SpeechDbId %in% reference_speeches$SpeechDbId,]$Speech
  return(reference_speeches)
}


run_wordscores <- function (raw_speeches, reference_speeches, run_name, amplify_result=FALSE, round_result=FALSE,
                            return_errors=FALSE, return_scored_reference_separately=FALSE, stem_speeches=FALSE,
                            remove_numbers=TRUE, return_model=FALSE) {
  library(quanteda)
  library(quanteda.textmodels)
  library(readr)
  library(stopwords)

  scored_corpus <- get_preprocessed_corpus(reference_speeches, stem_speeches=stem_speeches, remove_numbers=remove_numbers)
  whole_corpus <- get_preprocessed_corpus(group_speeches(raw_speeches, 'none'), stem_speeches=stem_speeches, remove_numbers=remove_numbers)

  feat_mat <- dfm(corpus(scored_corpus), remove=stopwords('de'))
  model <- textmodel_wordscores(feat_mat, reference_speeches$CoalScore, scale='linear')

  virgin_feat_mat <- dfm(corpus(whole_corpus), remove=stopwords('de'))
  result <- predict(model, virgin_feat_mat)

  readable_result <- as.data.frame(raw_speeches$SpeechDbId)
  readable_result$CreatedScore <- result
  colnames(readable_result) <- c('SpeechDbId', 'CreatedScore')

  readable_result <- readable_result[with(readable_result, order(SpeechDbId)),]
  reference_speeches <- reference_speeches[with(reference_speeches, order(SpeechDbId)),]

  if (amplify_result) {
    readable_result$CreatedScore <- readable_result$CreatedScore * (3 / max(abs(readable_result$CreatedScore)))
  }
  if (round_result) {
    readable_result$CreatedScore <- round(readable_result$CreatedScore)
  }

  # create subset of the result scores which were also provided as reference texts
  reference_result <- readable_result[readable_result$SpeechDbId %in% reference_speeches$SpeechDbId,]
  reference_result$ManualScore <- reference_speeches$CoalScore

  errors <- abs(reference_result$CreatedScore - reference_result$ManualScore)
  squared_err <- errors * errors
  print(paste('Mean error:        ', mean(errors)))
  print(paste('Mean squared error:', mean(squared_err)))
  print(paste('Median error:      ', median(errors)))
  print(paste('05% Quantile error:', quantile(errors, 0.05)))
  print(paste('25% Quantile error:', quantile(errors, 0.25)))
  print(paste('75% Quantile error:', quantile(errors, 0.75)))
  print(paste('95% Quantile error:', quantile(errors, 0.95)))

  write_csv(readable_result, path=paste0('data/', run_name, '_result.csv'))

  res <- list('result'=readable_result)
  if (return_errors) {
    res$errors <- errors
  }
  if (return_scored_reference_separately) {
    res$refResult <- reference_result
  }
  if (return_model) {
    res$model <- model
  }

  return(res)
}


plot_results_vs_expected <- function(result, run_name) {
  filename <- paste0('data/', run_name, '_vs_expected.png')
  png(filename=filename, width=1200, height=1200)
  plot(result$ManualScore, xaxt='n', xlab='', ylab='Score')
  points(result$CreatedScore, col='red')
  lines(result$ManualScore)
  lines(result$CreatedScore, col='red')
  axis(1, at=1:length(result$SpeechDbId), labels=result$SpeechDbId, las=2)
  for (j in seq(1:length(result$SpeechDbId))) {
    abline(v=j, col='grey')
  }
  legend('top', legend=c('Manually created scores', 'Wordscores created scores'), col=c('black', 'red'), pch=1,
         xpd=TRUE, bty='n', inset=c(0,0), horiz=TRUE)
  dev.off()
}

plot_word_weights <- function(trained_model, run_name) {
  # naming is relevant here!
  b <- coef(trained_model)
  words <- as.data.frame(b)
  words$psi <- seq(1:length(b))

  words <- as.matrix(words)
  rownames(words) <- names(b)

  filename <- paste0('data/', run_name, '_word_scores.png')
  png(filename=filename, width=6000, height=6000)
  plot(words[, 'b'], words[, 'psi'], xlab="Word Weights", ylab="Word Fixed Effect", type="n")
  text(words[, 'b'], words[, 'psi'], rownames(words))
  abline(v=0)
  dev.off()
}

# only for development, change as needed
find_optimum <- function(min_co, max_co, min_min_pct, max_min_pct, min_max_pct, max_max_pct, step_co, step_min_pct,
                         step_max_pct, filepath, min_period, max_period, use_only_extrema=FALSE) {
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
            data_filtered <- filter(data, 'p', min_period=min_period, max_period=max_period)
            data_filtered <- filter(data_filtered, 'co', chars_around=i)
            data_filtered <- filter(data_filtered,'sw', min_pct=j, max_pct=k)
            scored <- read_refence_speeches(use_rounded_scores)
            scored <- attach_speeches(data_filtered, scored)
            if (use_only_extrema) {
              scored <- scored[abs(scored$CoalScore) >= 2,]
            }
            cat('\n----------------------------------------------\n\n')
            print(paste('round res', round_result, 'round ref', use_rounded_scores, 'co', i, 'min', j, 'max', k))
            res <- run_wordscores(data_filtered, scored, 'trash', return_errors=TRUE,
                                  return_scored_reference_separately=TRUE, round_result=round_result)

            squared_err <- res$errors * res$errors
            pearson <- cor.test(res$refResult$ManualScore, res$refResult$CreatedScore, method='pearson')

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
