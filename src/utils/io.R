# Title     : General I/O
# Objective : Define generally required data input/output functions

read_speeches <- function(filepath) {
  library(readr)
  col_names <- c('Index', 'SpeechDbId', 'Date', 'Period', 'Sitting', 'DocDbId', 'Speaker', 'Party', 'InterjectionCount', 'InterjectionContent', 'ParagraphCount', 'Speech')
  cols <- cols(Index=col_integer(),
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
    return(read_csv(filepath, skip=1, col_names=col_names, col_types=cols))
}