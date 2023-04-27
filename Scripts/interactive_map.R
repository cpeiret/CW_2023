
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

bins_1 <- c(0:7)
bins_2 <- c(0:7)
bins_3 <- c(0:4)
pal_1 <- colorBin("RdYlGn", domain = access_adults$score, bins = bins_1)
pal_2 <- colorBin("RdYlGn", domain = access_seniors$score, bins = bins_2)
pal_3 <- colorBin("RdYlGn", domain = access_children$score, bins = bins_3)


leaflet() %>% 
  
  # Add basemap
  addProviderTiles(providers$CartoDB.Positron,
                   layerId = 'baseid_1') %>%
  
  # Add adults index
  addPolygons(data = access_adults,
              group = 'Index adults',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 1,
              fillColor = ~pal_1(score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = popupGraph(radar_charts_adults)
              ) %>% 
  # # Add legend
  # addLegend(data = access_adults, position = "bottomright",
  #           pal = pal_1, values = ~score, opacity = 1) %>% 

  
  # Add seniors index
  addPolygons(data = access_seniors,
              group = 'Index seniors',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 1,
              fillColor = ~pal_2(score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = FALSE),
              popup = popupGraph(radar_charts_seniors)
              ) %>% 
  # # Add legend
  # addLegend(data = access_seniors, position = "bottomright",
  #           pal = pal_2, values = ~score, opacity = 1) %>% 

  
  # Add children index
  addPolygons(data = access_children,
              group = 'Index children',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 1,
              fillColor = ~pal_3(score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = FALSE),
              popup = popupGraph(radar_charts_children)
  ) %>% 
  
  # # Add legend
  # addLegend(data = access_children, position = "bottomright",
  #           pal = pal_3, values = ~score, opacity = 1,
  #           ) %>% 
  
  # Add layer control
  addLayersControl(baseGroups = c('Index adults','Index seniors', 'Index children'))



# Test map ----------------------------------------------------------------
pal_1 <- colorBin("viridis", domain = access_adults$score, bins = bins_1)


leaflet() %>% 
  
  # Add basemap
  addProviderTiles(providers$CartoDB.Positron,
                   layerId = 'baseid_1') %>%
  
  # Add adults index
  addPolygons(data = access_adults,
              group = 'Index adults',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 1,
              fillColor = ~pal_1(score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)
  ) %>% 
  # Add legend
  addLegend(data = access_children, position = "bottomright",
            pal = pal_1, values = ~score, opacity = 1,
  )
  
  




