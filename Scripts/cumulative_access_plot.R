
# Introduction ------------------------------------------------------------

# Generate a cumulative access function to measure the distance it takes
# to get all LSOAs in Liverpool to access all the amenities.


# Libraries ---------------------------------------------------------------

library(tidyr)
library(latticeExtra)

# Data --------------------------------------------------------------------

index_adults <- read.csv('https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/cummulative_access_totals_adults.csv')

# Prepare data so it's in the right format
# Generate column of distances
index_adults$dist <- readr::parse_number(index_adults$index)
index_adults$X0_norm <- (index_adults$X0 - min(index_adults$X0)) / (max(index_adults$X0) - min(index_adults$X0))

# For each amenity, calculate cumulative access at LSOA level.
patterns <- c('schools','cinemas','atm','supermarket','pharmacies','gyms','park')

dfs <- list()
for (i in patterns) {
  dfs[[i]] <- index_adults[grepl(i, index_adults$index),]
}

colours <- c('#001219','#0a9396','#94d2bd','#e9d8a6','#ee9b00','#ca6702','#9b2226')
labels <- c('ATMs','Cinemas','Gyms','Parks','Pharmacies','Schools','Supermarkets')

ggplot() +
  # Add layers
  geom_line(data = dfs[[1]], aes(x = dist, y = X0_norm, colour = patterns[1]), size = 1.5) +
  geom_line(data = dfs[[2]], aes(x = dist, y = X0_norm, colour = patterns[2]), size = 1.5) +
  geom_line(data = dfs[[3]], aes(x = dist, y = X0_norm, colour = patterns[3]), size = 1.5) +
  geom_line(data = dfs[[4]], aes(x = dist, y = X0_norm, colour = patterns[4]), size = 1.5) +
  geom_line(data = dfs[[5]], aes(x = dist, y = X0_norm, colour = patterns[5]), size = 1.5) +
  geom_line(data = dfs[[6]], aes(x = dist, y = X0_norm, colour = patterns[6]), size = 1.5) +
  geom_line(data = dfs[[7]], aes(x = dist, y = X0_norm, colour = patterns[7]), size = 1.5) +
  
  # Add theme and labels
  theme_minimal() +
  ggtitle('Cumulative access to amenities from LSOAs') +
  xlab('Distance (m)') +
  ylab('Cumulative access') +
  theme(legend.text = element_text(size = 16),
        axis.text = element_text(size = 12)) +
  scale_colour_manual('',values = colours, labels = labels)










