# Title     : TODO
# Objective : TODO
# Created by: lukas
# Created on: 29/05/2020

# only needs to be executed once, as the dictionary of positive/negative words does not need to change
dictionaryGerman <- get_sentiment_dictionary()

data2 <- read_speeches('data/database_export_search_89.csv')
data2 <- filter(data, 'p', min_period=19, max_period=19)
data2 <- group_speeches(data, 'none', multiple_periods=TRUE)
#data <- data[nchar(data$Speech) < 1000,]

data$SpeechStem <- get_stemmed_speeches(data)

library(SentimentAnalysis)
data$Sentiment <- analyzeSentiment(data$SpeechStem, language='german', rules=list('GermanSentiment'=list(ruleLinearModel, dictionaryGerman)))$GermanSentiment

data <- data[(data$Sentiment <= quantile(data$Sentiment, 0.05)) | (data$Sentiment >= quantile(data$Sentiment, 0.95)),]
