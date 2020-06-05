# Title     : TODO
# Objective : TODO
# Created by: lukas
# Created on: 29/05/2020

# only needs to be executed once, as the dictionary of positive/negative words does not need to change
dictionaryGerman <- get_sentiment_dictionary()

data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=19, max_period=19)
data <- group_speeches(data, 'none', multiple_periods=TRUE)
# data <- data[nchar(data$Speech) < 1000,]

data$SpeechStem <- get_stemmed_speeches(data)

library(SentimentAnalysis)
data$Sentiment <- analyzeSentiment(data$SpeechStem, language='german', rules=list('GermanSentiment'=list(ruleLinearModel, dictionaryGerman)))$GermanSentiment

data <- data[(data$Sentiment <= quantile(data$Sentiment, 0.05)) | (data$Sentiment >= quantile(data$Sentiment, 0.95)),]

topic_scores <- read_topic_scores('data/topic_scores_Kohle.csv')
topic_scores <- apply_threshold(threshold = threshold, topic_scores = topic_scores)
data <- calculate_total_sentiment(topic_scores, data)
