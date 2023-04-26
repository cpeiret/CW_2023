
# Introduction ------------------------------------------------------------

# Generate an interactive map


# Libraries ---------------------------------------------------------------

library(sf)
library(leaflet)
library(leafpop)
library(leaflet.extras2)


# Data --------------------------------------------------------------------

url_1 <- 'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_adults_lsoa.geojson'
access_adults <- st_read(url_1)

url_2 <- 'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_seniors_lsoa.geojson'
access_seniors <- st_read(url_2)

url_3 <- 'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_children_lsoa.geojson'
access_children <- st_read(url_3)


# Multi-layer map with pop-up spider chart ------------------------------------

bins <- c(0:7)
pal <- colorBin("YlOrRd", domain = access$score, bins = bins)


leaflet() %>% 
  
  # Add basemap
  addProviderTiles(providers$CartoDB.Positron,
                   layerId = 'baseid_1') %>%
  
  # Add adults index
  addPolygons(data = access_adults,
              group = 'Index adults',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = popupGraph(radar_charts_adults)
              ) %>% 
  
  # Add seniors index
  addPolygons(data = access_seniors,
              group = 'Index seniors',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = FALSE),
              popup = popupGraph(radar_charts_seniors)
              ) %>% 
  
  # Add layer control
  addLayersControl(overlayGroups = c('Index adults','Index seniors'))






