# Title     : Sentiment analysis
# Objective : Running sentiment analysis on the political speeches and processing the results
# Comment   : No longer in use, results were never good enough to be employed for further work, only use was to identify
#             those speeches with the strongest sentiment in order to limit the candidates for manual scoring

# only needs to be executed once, as the dictionary of positive/negative words does not need to change
dictionaryGerman <- get_sentiment_dictionary()

raw <-read_speeches('../../data/database_export_search_89.csv')
raw <- filter_speeches(raw, 'p', min_period=18, max_period=18)
raw <- filter_speeches(raw, 'np')
raw <- group_speeches(raw, 'none', multiple_periods=TRUE)

data <- read_speeches('../../data/database_export_search_89.csv')
data <- filter_speeches(data, 'p', min_period=18, max_period=18)
data <- filter_speeches(data, 'vp')
data <- filter_speeches(data, 'np')
data <- filter_speeches(data, 'co', chars_around=300)
data <- group_speeches(data, 'none', multiple_periods=TRUE)
# data <- data[nchar(data$Speech) < 1000,]

data$SpeechStem <- get_stemmed_speeches(data)

library(SentimentAnalysis)
data$Sentiment <- analyzeSentiment(data$SpeechStem, language='german', rules=list('GermanSentiment'=list(ruleLinearModel, dictionaryGerman)))$GermanSentiment

data$Speech <- raw$Speech
data <- data[(data$Sentiment <= quantile(data$Sentiment, 0.05)) | (data$Sentiment >= quantile(data$Sentiment, 0.95)),]

data$SpeechStem <- NULL
data <- data[with(data, order(Sentiment)),]

write_delim(data, 'data/extreme_sentiment_co_18_test.csv', delim='\n')

topic_scores <- read_topic_scores('../../data/topic_scores_Kohle.csv')

topic_scores <- apply_threshold(threshold = 0, topic_scores = topic_scores)
data <- calculate_total_sentiment(topic_scores, data)

data <- calculate_pro_coal_sentiment(topic_scores)
data <- calculate_anti_coal_sentiment(topic_scores)

calc_all_weighted_sentiments <- function(dataset, topic_scores) {
  for (name in colnames(topic_scores)) {
    if (name == 'SpeechDbId') {
      next
    }
    weights <- get_weights_vector(initialize_with=0)
    weights[name] <- 1
    weighted_topics <- as.data.frame(sweep(as.matrix(topic_scores), MARGIN=2, as.matrix(weights), '*'))
    weighted_topics <- rowSums(weighted_topics)

    for (i in seq(1, nrow(dataset))) {
      row_number <- which(topic_scores$SpeechDbId == dataset$ID[i])
      dataset[i, name] <- dataset$Sentiment[i] * weighted_topics[row_number]
    }
  }
  return(dataset)
}

plot_weighted_sentiments <- function(dataset) {
  relevant_cols <- colnames(dataset)
  relevant_cols <- relevant_cols[! relevant_cols %in% c('ID', 'Speech', 'SpeechStem')]
  relevant_dataset <- dataset[relevant_cols]
  t <- relevant_dataset[1,]
  t <- as.character(relevant_dataset[2,])
  png(filename=paste0('data/Sentiment_', dataset$ID, '.png'), width=600, height=600)
  plot(as.character(relevant_dataset[2,]), xaxt='n', xlab='', ylab='Score')
  axis(1, at=1:length(relevant_cols), labels=relevant_cols, , las=2)
  for (i in seq(1, nrow(dataset))) {
    png(filename=paste0('data/Sentiment_', dataset$ID[i], '.png'), width=1000, height=1000)
    par(mar = c(15, 5, 5, 5)) # Set the margin on all sides to 6
    plot(as.character(relevant_dataset[i,]), xaxt='n', xlab='', ylab='Score')
    axis(1, at=1:length(relevant_cols), labels=relevant_cols, , las=2)
    abline(h=0, col='grey')
    for (j in seq(1, length(relevant_cols))) {
      abline(v=j, col='grey')
    }
    dev.off()
  }
}

d <- data[relevant_cols][,1]

data <- calc_all_weighted_sentiments(data, topic_scores)

plot_weighted_sentiments(data)


relevant_cols <- colnames(data)
relevant_cols <- relevant_cols[! relevant_cols %in% c('ID', 'Speech', 'SpeechStem')]
relevant_data <- data[relevant_cols]
t <- relevant_data[1,]
t <- as.character(relevant_data[2,])
png(filename=paste0('data/Sentiment_', data$ID, '.png'), width=600, height=600)
plot(as.character(relevant_data[2,]), xaxt='n', xlab='', ylab='Score')
axis(1, at=1:length(relevant_cols), labels=relevant_cols, , las=2)
for (i in seq(1, nrow(data))) {
  png(filename=paste0('data/Sentiment_', data$ID[i], '.png'), width=1000, height=1000)
  par(mar = c(15, 5, 5, 5)) # Set the margin on all sides to 6
  plot(as.character(relevant_data[i,]), xaxt='n', xlab='', ylab='Score')
  axis(1, at=1:length(relevant_cols), labels=relevant_cols, , las=2)
  abline(h=0, col='grey')
  for (j in seq(1, length(relevant_cols))) {
    abline(v=j, col='grey')
  }
  dev.off()
}


weights <- get_weights_vector(initialize_with=0)
weights$GreenPolicies <- 1
weights['GreenPolicies'] <- 1

weights <- get_weights_vector(initialize_with=0)
weights['GreenPolicies'] <- 1
weighted_topics <- as.data.frame(sweep(as.matrix(topic_scores), MARGIN=2, as.matrix(weights), '*'))
weighted_topics <- rowSums(weighted_topics)

data[1, 'test'] <- 4
data <- calculate_weighted_sentiment(data, topic_scores, weights)