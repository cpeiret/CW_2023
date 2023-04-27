
# Introduction ------------------------------------------------------------

# Generate a cumulative access function to measure the distance it takes
# to get all LSOAs in Liverpool to access all the amenities.


# Libraries ---------------------------------------------------------------

library(tidyr)

# Data --------------------------------------------------------------------

index_adults <- read.csv('https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/cummulative_access_totals_adults.csv')

# Prepare data so it's in the right format
patterns <- c('schools','cinemas','atm','supermarket','pharmacies','gyms','park')

for (i in patterns) {
  test <- index_adults[grepl(i, index_adults$index),]
  print(ggplot(test, aes(X0)) +
    stat_ecdf(geom = 'step'))
}