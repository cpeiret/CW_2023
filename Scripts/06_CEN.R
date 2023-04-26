
# Introduction ------------------------------------------------------------

# Download census data
# Map census data
# Compare to accessibility score

# Libraries ---------------------------------------------------------------

library(nomisr)
library(dplyr)
library(arrow)
library(sfarrow)
library(sf)
library(ggplot2)
library(ggpubr)

# Parameters --------------------------------------------------------------

study_areas <- c('cambridge','liverpool','milton_keynes',
                 'newcastle','nottingham')

# Household composition ---------------------------------------------------

# # Download
# hh <- nomis_get_data(id='NM_2023_1',
#                      time = 'latest',
#                      geography = 'TYPE150',
#                      measures = 20301,
#                      tidy = TRUE
#                      )
# Clean
hh_clean <- hh %>% dplyr::select(geography_code,
                           c_2021_hhcomp_15_name,
                           c_2021_hhcomp_15,
                           obs_value)
remove <- c(0,1001:1007)
hh_clean <- hh_clean %>% dplyr::filter(!c_2021_hhcomp_15 %in% remove)
hh_clean$c_2021_hhcomp_15_name <- stringr::str_replace(hh_clean$c_2021_hhcomp_15_name, "One-person household", "OPH")
hh_clean$c_2021_hhcomp_15_name <- stringr::str_replace(hh_clean$c_2021_hhcomp_15_name, "Single family household", "SFH")
hh_clean$c_2021_hhcomp_15_name <- stringr::str_replace(hh_clean$c_2021_hhcomp_15_name, "Other household types", "OHT")

write_parquet(hh_clean, './data/hh_comp.parquet')

# Add geo data and map ---------------------------------------------------

for (i in 1:length(study_areas)) {
  
  # Load data
  hh <- read_parquet('./data/hh_comp.parquet')
  oa <- st_read_parquet(paste0('./data/',study_areas[i],'/oa.parquet'))
  
  # Add geo data
  hh_geo <- merge(hh_clean,oa,by.x = 'geography_code', by.y= 'OA21CD')
  hh_geo <- st_as_sf(hh_geo, sf_column_name = 'geometry')
  
  for (j in 1:14) {
    df <- hh_geo %>% dplyr::filter(c_2021_hhcomp_15 == j)
    plots[[j]] <- ggplot() +
      geom_sf(data = df, aes(fill = obs_value), col = NA) +
      scale_fill_viridis_c('%',option = 'F') +
      theme_void() +
      labs(title = LETTERS[j]) +
      theme(text = element_text(size=12),
            plot.margin = unit(c(0,0.2,0,1), 'lines'))
  }
  pdf(paste0('./outputs/',study_areas[i],'/HH_comp.pdf'), paper = 'USr', width = 14, height = 8.5)
  print(ggarrange(plotlist = plots, ncol = 5, nrow = 3))
  dev.off()
  
}



