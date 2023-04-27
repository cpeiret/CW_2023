
# Introduction ------------------------------------------------------------

# Generate an interactive map


# Libraries ---------------------------------------------------------------

library(sf)
library(leaflet)
library(leafpop)
library(leaflet.extras2)


# Data --------------------------------------------------------------------

url_1 <- 'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_adults_lsoa.geojson'
access_adults <- st_read(url_1) %>% 
  st_drop_geometry()

url_2 <- 'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_seniors_lsoa.geojson'
access_seniors <- st_read(url_2) %>% 
  st_drop_geometry()

url_3 <- 'https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_children_lsoa.geojson'
access_children <- st_read(url_3) %>% 
  st_drop_geometry()



# Spider charts for adults ------------------------------------------------

# Get max and min values
columns <- colnames(access_adults[2:8])
max_values <- apply(access_adults[, columns], 2, max)
min_values <- apply(access_adults[, columns], 2, min)

radar_charts_adults <- list()

for (i in 1:nrow(access_adults)) {
  df <- rbind(max_values, min_values, access_adults[i,][2:8])
  radarchart(df,
             # Customize the polygon
             pcol = '#00AFBB', pfcol = scales::alpha('#00AFBB', 0.5), plwd = 2, plty = 1,
             # Customize the grid
             cglcol = "grey", cglty = 1, cglwd = 0.8,
             # Customize the axis
             axislabcol = "grey")
  radar_charts_adults[[i]] <- recordPlot()
}


# Spider charts for seniors ------------------------------------------------

# Get max and min values
columns <- colnames(access_seniors[2:8])
max_values <- apply(access_seniors[, columns], 2, max)
min_values <- apply(access_seniors[, columns], 2, min)

radar_charts_seniors <- list()

for (i in 1:nrow(access_seniors)) {
  df <- rbind(max_values, min_values, access_seniors[i,][2:8])
  radarchart(df,
             # Customize the polygon
             pcol = '#00AFBB', pfcol = scales::alpha('#00AFBB', 0.5), plwd = 2, plty = 1,
             # Customize the grid
             cglcol = "grey", cglty = 1, cglwd = 0.8,
             # Customize the axis
             axislabcol = "grey")
  radar_charts_seniors[[i]] <- recordPlot()
}

# Spider charts for children ------------------------------------------------

# Get max and min values
columns <- colnames(access_children[2:5])
max_values <- apply(access_children[, columns], 2, max)
min_values <- apply(access_children[, columns], 2, min)

radar_charts_children <- list()

for (i in 1:nrow(access_children)) {
  df <- rbind(max_values, min_values, access_children[i,][2:5])
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
  # Add legend
  addLegend(data = access_adults, position = "bottomright", group = 'Index adults',
            pal = pal_1, values = ~score, opacity = 1, title = 'Adults') %>%

  
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
  # Add legend
  addLegend(data = access_adults, position = "bottomright", group = 'Index seniors',
            pal = pal_1, values = ~score, opacity = 1, title = 'Seniors') %>%
  

  
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
  
  # Add legend
  addLegend(data = access_children, position = "bottomright", group = 'Index children',
            pal = pal_3, values = ~score, opacity = 1, title = 'Children'
            ) %>%
  
  # Add layer control
  addLayersControl(baseGroups = c('Index adults','Index seniors', 'Index children'),
                   options = layersControlOptions(collapsed = FALSE))


# Slider map --------------------------------------------------------------

leaflet() %>%
  addMapPane("left", zIndex = 0) %>%
  addMapPane("right", zIndex = 0) %>%
  addProviderTiles(providers$CartoDB.Positron, group="carto", layerId = "baseid",
                   options = pathOptions(pane = "right")) %>%
  addProviderTiles(providers$CartoDB.Positron, group="carto", layerId = "cartoid",
                   options = pathOptions(pane = "left")) %>%
  
  addPolygons(data = access_adults,
              group = 'Index adults',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 1,
              fillColor = ~pal_2(score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = FALSE),
              options = pathOptions(pane = 'left')
  ) %>% 
  
  
  addPolygons(data = access_seniors,
              group = 'Index seniors',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 1,
              fillColor = ~pal_2(score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = FALSE),
              options = pathOptions(pane = 'right')
  ) %>% 
  
  # addLayersControl(overlayGroups = c("Index adults","Index seniors")) %>%
  addSidebyside(layerId = "sidecontrols",
                rightId = "baseid",
                leftId = "baseid")



# Test map ----------------------------------------------------------------


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
  addLegend(data = access_adults, position = "bottomright", group = 'Index adults',
            pal = pal_1, values = ~score, opacity = 1, title = 'Adults') %>%
  
  
  # Add seniors index
  addPolygons(data = access_seniors,
              group = 'Index seniors',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 1,
              fillColor = ~pal_2(score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = FALSE)
  ) %>% 
  # Add legend
  addLegend(data = access_adults, position = "bottomright", group = 'Index seniors',
            pal = pal_1, values = ~score, opacity = 1, title = 'Seniors') %>%
  
  
  # Add layer control
  addLayersControl(baseGroups = c('Index adults','Index seniors'),
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  
  htmlwidgets::onRender("
    function(el, x) {
      var updateLegend = function () {
          var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

          document.querySelectorAll('.legend').forEach(a => a.hidden=true);
          document.querySelectorAll('.legend').forEach(l => {
            if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
          });
      };
      updateLegend();
      this.on('baselayerchange', e => updateLegend());
    }")







