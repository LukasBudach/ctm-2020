# Title     : TODO
# Objective : TODO
# Created by: lukas
# Created on: 29/05/2020

source('preprocessing_functions.R')

get_sentiment_dictionary <- function() {
  library(readr)
  library(stringr)
  library(dplyr)
  library(SnowballC)
  library(SentimentAnalysis)

  # read the SentiWS dictionary
  col_names <- c('Word', 'Score', 'Alt')
  cols <- cols(Word=col_character(),
               Score=col_double(),
               Alt=col_character()
               )
  dict_pos <- read_delim('data/SentiWS_v1.8c_Positive.txt', '	', skip=0, col_names=col_names, col_types=cols)
  dict_neg <- read_delim('data/SentiWS_v1.8c_Negative.txt', '	', skip=0, col_names=col_names, col_types=cols)

  dict_pos$Word <- gsub(dict_pos$Word, pattern="\\|.*", replacement='')
  dict_neg$Word <- gsub(dict_neg$Word, pattern="\\|.*", replacement='')

  dict_new <- dict_pos[, !(names(dict_pos) %in% c('Alt'))]
  dict_new <- rbind(dict_new, dict_neg[, !(names(dict_neg) %in% c('Alt'))])

  alternative_words <- str_split(dict_pos$Alt, ',')
  alternative_words <- append(alternative_words, str_split(dict_neg$Alt, ','))

  pb <- txtProgressBar(min=1, max=nrow(dict_new), initial=1)
  it_range <- seq(1, nrow(dict_new))
  for (i in it_range) {
    setTxtProgressBar(pb, i)
    alt_word_vec <- alternative_words[[i]]
    if (anyNA(alt_word_vec)) {
      next
    }
    for (j in alt_word_vec) {
      dict_new <- rbind(dict_new, list(j, dict_new$Score[i]))
    }
  }
  dict_new <- dict_new %>% arrange(Word)

  # stem the dictionary words and remove duplicates
  dict_new$Word <- wordStem(dict_new$Word, language='german')
  dict_new <- dict_new[!duplicated(dict_new$Word),]

  # return the sentiment dictionary
  return(SentimentDictionaryWeighted(dict_new$Word, dict_new$Score))
}

get_stemmed_speeches <- function(dataset) {
  speeches <- dataset$Speech

  for (i in seq(1, nrow(dataset))) {
    speeches[i] <- stem_speech(dataset$Speech[i])
  }
  return(speeches)
}

read_topic_scores <- function(filepath) {
  library(readr)
  col_names <- c('SpeechDbId', 'EnvironmentalProtection', 'DebateGovernmentPolicy', 'EconomicPolicy', 'AgriculturalPolicy', 'GreenPolicies', 'Reunification', 'CoalMining', 'EuropeanCoalAndSteelCommunity', 'SubsidyReduction', 'NuclearPhaseOut', 'CommonTerms', 'Procedural', 'ResearchAndDevelopment', 'TransportPolicy', 'RisksOfNuclearEnergy', 'FiscalReform', 'EnergyTransitionAndPowerMarket', 'JobMarket', 'Procedural2', 'InternationalCooperation', 'NaturalResources', 'HardCoalSubsidies', 'StructuralAdjustment', 'PolicyStatements', 'ForeignPolicy', 'Budget2', 'FederalGovernment', 'Budget', 'HousingAndSocialSecurity', 'EnergySupplyMix', 'EconomicPolicy2', 'EconomicPolicyAndParticipation', 'ClimateProtection', 'TaxPolicy', 'CoalPolicyAndPrices')
  cols <- cols(SpeechDbId=col_integer(),
               EnvironmentalProtection=col_double(),
               DebateGovernmentPolicy=col_double(),
               EconomicPolicy=col_double(),
               AgriculturalPolicy=col_double(),
               GreenPolicies=col_double(),
               Reunification=col_double(),
               CoalMining=col_double(),
               EuropeanCoalAndSteelCommunity=col_double(),
               SubsidyReduction=col_double(),
               NuclearPhaseOut=col_double(),
               CommonTerms=col_double(),
               Procedural=col_double(),
               ResearchAndDevelopment=col_double(),
               TransportPolicy=col_double(),
               RisksOfNuclearEnergy=col_double(),
               FiscalReform=col_double(),
               EnergyTransitionAndPowerMarket=col_double(),
               JobMarket=col_double(),
               Procedural2=col_double(),
               InternationalCooperation=col_double(),
               NaturalResources=col_double(),
               HardCoalSubsidies=col_double(),
               StructuralAdjustment=col_double(),
               PolicyStatements=col_double(),
               ForeignPolicy=col_double(),
               Budget2=col_double(),
               FederalGovernment=col_double(),
               Budget=col_double(),
               HousingAndSocialSecurity=col_double(),
               EnergySupplyMix=col_double(),
               EconomicPolicy2=col_double(),
               EconomicPolicyAndParticipation=col_double(),
               ClimateProtection=col_double(),
               TaxPolicy=col_double(),
               CoalPolicyAndPrices=col_double())
    return(read_csv(filepath, skip=3, col_names=col_names, col_types=cols))
}

make_binary_vec <- function(vec, threshold) {
  return(sapply(vec, FUN=make_binary_val, threshold=threshold))
}

make_binary_val <- function(val, threshold) {
  return(ifelse(val > threshold, val, 0))
}

apply_threshold <- function(threshold, topic_scores){
  threshold <- mean(topic_scores$GreenPolicies)
  speechID <- topic_scores$SpeechDbId
  colNames <- colnames(topic_scores)
  topic_scores <- as.data.frame(sapply(topic_scores[,-1], FUN=make_binary_vec, threshold=threshold, simplify=FALSE))

  topic_scores <- cbind(speechID, topic_scores)
  colnames(topic_scores) <- colNames
  return(topic_scores)
}

get_weights_vector <- function(initialize_with=1) {
  col_names <- c('SpeechDbId', 'EnvironmentalProtection', 'DebateGovernmentPolicy', 'EconomicPolicy', 'AgriculturalPolicy', 'GreenPolicies', 'Reunification', 'CoalMining', 'EuropeanCoalAndSteelCommunity', 'SubsidyReduction', 'NuclearPhaseOut', 'CommonTerms', 'Procedural', 'ResearchAndDevelopment', 'TransportPolicy', 'RisksOfNuclearEnergy', 'FiscalReform', 'EnergyTransitionAndPowerMarket', 'JobMarket', 'Procedural2', 'InternationalCooperation', 'NaturalResources', 'HardCoalSubsidies', 'StructuralAdjustment', 'PolicyStatements', 'ForeignPolicy', 'Budget2', 'FederalGovernment', 'Budget', 'HousingAndSocialSecurity', 'EnergySupplyMix', 'EconomicPolicy2', 'EconomicPolicyAndParticipation', 'ClimateProtection', 'TaxPolicy', 'CoalPolicyAndPrices')
  weights <- data.frame(matrix(data=initialize_with, ncol=length(col_names), nrow=1))
  colnames(weights) <- col_names
  return(weights)
}

calculate_total_sentiment <- function (topic_scores, data){
  anti_coal_topics <- (topic_scores$ClimateProtection + topic_scores$EnergySupplyMix + topic_scores$EnergyTransitionAndPowerMarket + topic_scores$EnvironmentalProtection + topic_scores$GreenPolicies + topic_scores$SubsidyReduction)/6
  pro_coal_topics <- (topic_scores$EuropeanCoalAndSteelCommunity + topic_scores$HardCoalSubsidies + topic_scores$JobMarket + topic_scores$NaturalResources)/4

  for (i in seq(1, nrow(data))) {
    row_number <- which(topic_scores$SpeechDbId == data$ID[i])
    data$TotalSentiment[i] <- (data$Sentiment[i] * pro_coal_topics[row_number]) - (data$Sentiment[i] * anti_coal_topics[row_number])
  }
  return(data)
}

calculate_pro_coal_sentiment <- function (topic_scores){
  pro_coal_topics <- (topic_scores$EuropeanCoalAndSteelCommunity + topic_scores$HardCoalSubsidies + topic_scores$JobMarket + topic_scores$NaturalResources)/4

  for (i in seq(1, nrow(data))) {
    row_number <- which(topic_scores$SpeechDbId == data$ID[i])
    data$ProCoalSentiment[i] <- data$Sentiment[i] * pro_coal_topics[row_number]
  }
  return(data)
}

calculate_anti_coal_sentiment <- function (topic_scores){
  anti_coal_topics <- (topic_scores$ClimateProtection + topic_scores$EnergySupplyMix + topic_scores$EnergyTransitionAndPowerMarket + topic_scores$EnvironmentalProtection + topic_scores$GreenPolicies + topic_scores$SubsidyReduction)/6

  for (i in seq(1, nrow(data))) {
    row_number <- which(topic_scores$SpeechDbId == data$ID[i])
    data$AntiCoalSentiment[i] <- data$Sentiment[i] * anti_coal_topics[row_number]
  }
  return(data)
}

calculate_weighted_sentiment <- function(dataset, topic_scores, weights) {
  weighted_topics <- as.data.frame(sweep(as.matrix(topic_scores), MARGIN=2, as.matrix(weights), '*'))
  weighted_topics <- rowSums(weighted_topics)

  for (i in seq(1, nrow(dataset))) {
    row_number <- which(topic_scores$SpeechDbId == dataset$ID[i])
    dataset$WeightedSentiment[i] <- dataset$Sentiment[i] * weighted_topics[row_number]
  }
  return(dataset)
}