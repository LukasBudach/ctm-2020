# Title     : TODO
# Objective : TODO
# Created by: lukas
# Created on: 13/05/2020


# random stuff
for(i in seq(min(data$Period), max(data$Period))) {
  print(sprintf('Period: %d speeches: %d', i, nrow(filter_period(data, i))))
}

# actually doing stuff here
data <- read_speeches('data/database_export_search_89.csv')
d_6 <- filter_period(data, 6)
mat <- get_frequency_matrix(d_6, 0.99)
res <- wordfish(mat, dir=c(1,2))
