
# Introduction ------------------------------------------------------------

# Create a radar plot for each LSOA


# Libraries ---------------------------------------------------------------

library(sf)
library(fmsb)

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










