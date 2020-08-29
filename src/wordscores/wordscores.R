# Title     : Wordscores
# Objective : Implementation of the Wordscores approach

run_wordscores <- function (raw_speeches, reference_speeches, run_name, amplify_result=FALSE, round_result=FALSE,
                            return_errors=FALSE, return_scored_reference_separately=FALSE, return_model=FALSE,
                            stem_speeches=FALSE, remove_numbers=TRUE) {
  # runs the Wordscores algorithm as provided by the quanteda.textmodels package
  #
  # function parameters:
  #   raw_speeches        -> dataframe containing the speeches to be scored
  #   reference_speeches  -> dataframe containing the previously scored reference documents
  #   run_name            -> string, name the result is to be exported under
  #   amplify_result      -> if TRUE, scales the prediction result setting the largest absolute score to 3
  #   round_result        -> if TRUE, rounds the floating point prediction results to the next integer
  #   return_errors       -> if TRUE, returns the calculated prediction error in addition to the results
  #   return_scored_reference_separately  -> if TRUE, returns the prediction results of the reference documents separately
  #   return_model        -> if TRUE, returns the created Wordscores model in addition to the results
  #   stem_speeches       -> if TRUE, the documents are stemmed in preprocessing
  #   remove_numbers      -> if TRUE, all numbers are removed from every document in preprocessing
  #
  # returns a list with named fields:
  #   field name          -> contained data
  #   result              -> Wordscores prediction for all of the raw speeches
  #   errors              -> calculated prediction error
  #   ref_result          -> reference and prediction scores for the reference texts
  #   model               -> created Wordscores model

  source('src/utils/grouping.R')
  source('src/utils/preprocessing.R')

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
    res$ref_result <- reference_result
  }
  if (return_model) {
    res$model <- model
  }

  return(res)
}
