# Title     : TODO
# Objective : TODO
# Created by: lukas
# Created on: 13/05/2020

library(readr)

col_names <- c('Index', 'SpeechDbId', 'Date', 'Period', 'Sitting', 'DocDbId', 'Speaker', 'Party', 'InterjectionCount', 'InterjectionContent', 'ParagraphCount', 'Speech')
cols <- readr::cols(Index=col_integer(),
                    SpeechDbId=col_integer(),
                    Date=col_date(),
                    Period=col_integer(),
                    Sitting=col_integer(),
                    DocDbId=col_integer(),
                    Speaker=col_character(),
                    Party=col_character(),
                    InterjectionCount=col_integer(),
                    InterjectionContent=col_character(),
                    ParagraphCount=col_integer(),
                    Speech=col_character())

data <- readr::read_csv('../coal-discourse/datasets/database_export_search_89.csv', skip=1, col_names=col_names, col_types=cols)

# summary(data)
# plot(table(data$ParagraphCount))

# speech <- data[[13, "Speech"]]


corpus <- tm::VCorpus(tm::VectorSource(data$Speech))
corpus <- tm::tm_map(corpus, tm::content_transformer(tolower)) # MAKES EVERYTHING LOWERCASE
corpus <- tm::tm_map(corpus, tm::removeNumbers) # REMOVE NUMBERS
corpus <- tm::tm_map(corpus, tm::stripWhitespace) # REMOVE EXTRA WHITE SPACE
freq_mat <- tm::TermDocumentMatrix(corpus)
sparce_mat <- tm::removeSparseTerms(freq_mat, 0.996) # 0.9997
freq <- as.matrix(sparce_mat)
colnames(freq) <- data$SpeechDbId

res <- wordfish(freq, dir=c(1,2))
