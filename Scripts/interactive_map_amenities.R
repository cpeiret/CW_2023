
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

test <- amenities$atms

# Plot maps ---------------------------------------------------------------

leaflet() %>% 
  # Add basemap
  addProviderTiles(providers$CartoDB.Positron,
                   layerId = 'baseid_1') %>% 
  
  # Add atms
  addCircleMarkers(data = st_centroid(amenities$atms),
             group = 'ATMs',
             radius = 5,
             clusterOptions = markerClusterOptions()) %>% 
  
  # Add cinemas
  addCircleMarkers(data = st_centroid(amenities$cinemas),
             group = 'Cinemas',
             radius = 5,
             clusterOptions = markerClusterOptions()) %>%

  # Add parks
  addCircleMarkers(data = st_centroid(st_make_valid(amenities$parks)),
                   group = 'Parks',
                   radius = 5,
                   clusterOptions = markerClusterOptions()) %>%
  # Add Pharmacies
  addCircleMarkers(data = st_centroid(amenities$pharmacies),
                   group = 'Pharmacies',
                   radius = 5,
                   clusterOptions = markerClusterOptions()) %>%
  # Add pitches
  addCircleMarkers(data = st_centroid(amenities$pitches),
                   group = 'Pitches',
                   radius = 5,
                   clusterOptions = markerClusterOptions()) %>%
  # Add playgrounds
  addCircleMarkers(data = st_centroid(st_make_valid(amenities$playgrounds)),
                   group = 'Playgrounds',
                   radius = 5,
                   clusterOptions = markerClusterOptions()) %>%
  # Add schools
  addCircleMarkers(data = st_centroid(st_make_valid(amenities$schools)),
                   group = 'Schools',
                   radius = 5,
                   clusterOptions = markerClusterOptions()) %>%
  # Add supermarkets
  addCircleMarkers(data = st_centroid(st_make_valid(amenities$supermarkets)),
                   group = 'Supermarkets',
                   radius = 5,
                   clusterOptions = markerClusterOptions()) %>%
  
  # Add layer control
  addLayersControl(baseGroups = c('ATMs','Cinemas','Parks','Pharmacies',
                                  'Pitches','Playgrounds','Schools',
                                  'Supermarkets'),
                   options = layersControlOptions(collapsed = FALSE))

