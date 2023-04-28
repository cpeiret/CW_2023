
# Introduction ------------------------------------------------------------

# Generate an interactive map


# Libraries ---------------------------------------------------------------

library(sf)
library(leaflet)
library(leafpop)
library(leaflet.extras2)


# Data --------------------------------------------------------------------

url_1 <- 'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_adults_buildings_small.geojson'
access_adults <- st_read(url_1) %>% 
  st_drop_geometry()

url_2 <- 'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_seniors_buildings.geojson'
access_seniors <- st_read(url_2) %>% 
  st_drop_geometry()

url_3 <- 'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_children_buildings_small.geojson'
access_children <- st_read(url_3) %>% 
  st_drop_geometry()
 

# Spider charts for adults ------------------------------------------------

# Get max and min values
columns <- colnames(access_adults[3:9])
max_values <- apply(access_adults[, columns], 2, max)
min_values <- apply(access_adults[, columns], 2, min)

radar_charts_adults <- list()

for (i in 1:nrow(access_adults)) {
  df <- rbind(max_values, min_values, access_adults[i,][3:9])
  radarchart(df,
             # Customize the polygon
             pcol = '#00AFBB', pfcol = scales::alpha('#00AFBB', 0.5), plwd = 2, plty = 1,
             # Customize the grid
             cglcol = "grey", cglty = 1, cglwd = 0.8,
             # Customize the axis
             axislabcol = "grey")
  radar_charts_adults[[i]] <- recordPlot()
}

# Spider charts for children ------------------------------------------------

# Get max and min values
columns <- colnames(access_children[3:6])
max_values <- apply(access_children[, columns], 2, max)
min_values <- apply(access_children[, columns], 2, min)

radar_charts_children <- list()

for (i in 1:nrow(access_children)) {
  df <- rbind(max_values, min_values, access_children[i,][3:6])
  radarchart(df,
             # Customize the polygon
             pcol = '#00AFBB', pfcol = scales::alpha('#00AFBB', 0.5), plwd = 2, plty = 1,
             # Customize the grid
             cglcol = "grey", cglty = 1, cglwd = 0.8,
             # Customize the axis
             axislabcol = "grey")
  radar_charts_children[[i]] <- recordPlot()
}

# Data --------------------------------------------------------------------

url_1 <- 'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_adults_buildings_small.geojson'
access_adults <- st_read(url_1)

url_2 <- 'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_seniors_buildings.geojson'
access_seniors <- st_read(url_2)

url_3 <- 'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_children_buildings_small.geojson'
access_children <- st_read(url_3)

access_adults <- access_adults[!st_geometry_type(access_adults$geometry) == 'POINT',]
access_seniors <- access_adults[!st_geometry_type(access_seniors$geometry) == 'POINT',]
access_children <- access_children[!st_geometry_type(access_children$geometry) == 'POINT',]


# Multi-layer map with pop-up spider chart ------------------------------------

bins_1 <- c(0:7)
bins_2 <- c(0:7)
bins_3 <- c(0:4)
pal_1 <- colorBin("RdYlGn", domain = access_adults$score, bins = bins_1)
pal_2 <- colorBin("RdYlGn", domain = access_seniors$score, bins = bins_2)
pal_3 <- colorBin("RdYlGn", domain = access_children$score, bins = bins_3)

# Regular map -------------------------------------------------------------

leaflet() %>% 
  
  # Add basemap
  addProviderTiles(providers$CartoDB.DarkMatter,
                   layerId = 'baseid_1') %>%
  
  # Add adults index
  addPolygons(data = access_adults,
              group = 'Index adults',
              color = NA, weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 1,
              fillColor = ~pal_1(score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = popupGraph(radar_charts_adults)
  ) %>% 
  
  # Add children index
  addPolygons(data = access_children,
              group = 'Index children',
              color = NA, weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 1,
              fillColor = ~pal_1(score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = popupGraph(radar_charts_children)
              ) %>% 
  # Add layer control
  addLayersControl(baseGroups = c('Index adults', 'Index children'),
                   options = layersControlOptions(collapsed = FALSE))


# Slider map --------------------------------------------------------------

leaflet() %>%
  addMapPane("left", zIndex = 0) %>%
  addMapPane("right", zIndex = 0) %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group="carto", layerId = "baseid",
                   options = pathOptions(pane = "right")) %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group="carto", layerId = "cartoid",
                   options = pathOptions(pane = "left")) %>%
  
  addPolygons(data = access_adults,
              group = 'Index adults',
              color = NA, weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 1,
              fillColor = ~pal_2(score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = FALSE),
              options = pathOptions(pane = 'left')
  ) %>% 
  
  
  addPolygons(data = access_children,
              group = 'Index children',
              color = NA, weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 1,
              fillColor = ~pal_2(score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = FALSE),
              options = pathOptions(pane = 'right')
  ) %>% 
  
  # addLayersControl(overlayGroups = c("Index adults","Index children")) %>%
  addSidebyside(layerId = "sidecontrols",
                rightId = "baseid",
                leftId = "baseid")



