
# Introduction ------------------------------------------------------------

# Create an interactive map to show amenities

# Libraries ---------------------------------------------------------------

library(sf)
library(leaflet)
library(leafpop)
library(leaflet.extras2)

# Data --------------------------------------------------------------------


# Download data -----------------------------------------------------------
urls <- list(
  'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/atms_gdf.geojson',
  'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/cinemas_gdf.geojson',
  'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/parks_gdf.geojson',
  'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/pharmacies_gdf.geojson',
  'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/pitches_gdf.geojson',
  'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/playgrounds_gdf.geojson',
  'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/schools_gdf.geojson',
  'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/supermarkets_gdf.geojson'
)

amenities <- list()
for (i in 1:length(urls)) {
  amenities[[i]] <- st_read(urls[i])
}

names(amenities) <- c('atms','cinemas','parks','pharmacies','pitches','playgrounds',
                      'schools','supermarkets')



# Plot maps ---------------------------------------------------------------







