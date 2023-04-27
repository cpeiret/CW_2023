Packages = c("plyr", "ggplot2","tmap", "basemaps", "raster", "geojsonsf", "biscale", "cowplot", "sf","dplyr")

## Now load or install&load all
package.check <- lapply(
  Packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)



convert = read.csv("convert.csv")
LSOA = st_read(dsn = (paste0(getwd(), "/LSOA")),layer="LSOA_2011_EW_BFC_V3")

Index_Adult = geojson_sf("https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_adults_lsoa.geojson")%>%
  st_drop_geometry()
Index_Child = geojson_sf("https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_children_lsoa.geojson")%>%
  st_drop_geometry()
Index_Seniors = geojson_sf("https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_seniors_lsoa.geojson")%>%
  st_drop_geometry()



hh = read.csv("household_composition.csv")%>%
  separate(1, c("LSOA21CD", NA), sep = " : ")%>%
  select(1,
         total_households = 2,
         one_person_66 = 4,
         all_66 = 6,
         dep1 = 8,
         dep2 = 10,
         dep3 = 12,
         dep4 = 14)%>%
  mutate(., over_66 = one_person_66+all_66,
         over_66 = over_66/total_households,
         children = dep1+dep2+dep3+dep4,
         children = children/total_households)%>%
  select(LSOA21CD, over_66, children)

Sex = read.csv("sex.csv")%>%
  select(LSOA21CD = 2,
         female_perc)
ethnic = read.csv("ethic_group.csv")%>%
  select(LSOA21CD = 2,
         white_perc)
disability = read.csv("disability.csv")%>%
  separate(1, c("LSOA21CD", NA), sep = " : ")

Corr_data = hh%>%
  left_join(., Sex)%>%
  left_join(.,ethnic)%>%
  left_join(., disability)%>%
  left_join(Index_Adult%>%
              select(LSOA21CD, adult_index = score_norm),. , by = "LSOA21CD")%>%
  left_join(Index_Child%>%
              select(LSOA21CD, child_index = score_norm),.,by = "LSOA21CD" )%>%
  left_join(Index_Seniors%>%
              select(LSOA21CD, senior_index = score_norm),.,by = "LSOA21CD")

write.csv(Corr_data, "git/corrplot_variables.csv")

Corr_data%>%
  select(-"LSOA21CD")%>%
  cor(., method = "pearson")%>%
  corrplot.mixed(., tl.pos = 'lt', tl.col = "black", sig.level = 0.05) 






#results_corrplot


IMD_rank = read.csv("imd_score.csv")%>%
  select(1,2,5)%>%
  left_join(., read.csv("imd.csv"))%>%
  rename(LSOA11CD = 1)
SAMHI = read.csv("samhi.csv")%>%
  filter(., year=="2019")%>%
  rename(LSOA11CD = 1)%>%
  mutate(samhi_rank = rank(-samhi_index))
Liverpool= sf::st_read(dsn = (paste0(getwd(), "/LAD")),layer="LAD_DEC_2021_GB_BFC")%>%
  filter(., LAD21NM == "Liverpool")

#Bivariate plot

bivariate = left_join(SAMHI%>%
                        select(LSOA11CD, samhi_rank), 
                      IMD_rank%>%
                        select(LSOA11CD, imd_rank))%>%
  left_join(LSOA,., by = "LSOA11CD")%>%
  filter(., LSOA11CD %in% Index_Adult$LSOA11CD)


data <- bivariate%>%
  mutate(imd_rank = as.numeric(imd_rank),
         samhi_rank = as.numeric(samhi_rank))%>%
  bi_class(., x = imd_rank, y = samhi_rank, style = "quantile", dim = 3, keep_factors = FALSE, dig_lab = 3)

#install.packages('ggsn')
#library(ggsn)

LSOA_Dec = bivariate%>%
  mutate(imd_decile_liv = ntile(imd_rank, 10),
         samhi_decile_liv = ntile(samhi_rank, 10))%>%
  filter(., imd_decile_liv == 1 & samhi_decile_liv == 1)

map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = NA, size = 0.05, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Comparing Mental Health and Deprivation"
  ) +
  bi_theme()+
  theme(plot.title = element_text(size = 14, face = "bold"))+
  geom_sf(data = LSOA_Dec, fill = NA)
legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "IMD Rank",
                    ylab = "SAMHI Rank",
                    size = 8)


Plot = ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.65, 0.55, 0.17, 0.22)

ggsave(
  paste0("C:/Users/b9054751/OneDrive - Newcastle University/PhD/Data/CW_2023/bivariate.jpeg"),
  plot = Plot
)


# make some bbox magic
bbox_new <- st_bbox(bivariate) # current bounding box
xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.1 * xrange) # xmax - right
bbox_new[2] <- bbox_new[2] - (0.1 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.1 * yrange) # ymax - top
bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon
  
set_defaults(map_service = "osm", map_type = "streets")
basemap = basemap_raster(bbox_new)
projection(basemap) <- CRS("+init=EPSG:4326")

#Merseyside = st_union (bivariate)
Map1 = 
  tm_shape(basemap, bbox = bbox_new) + tm_rgb(saturation = 0.2) +
  tm_shape(Liverpool, bbox = bbox_new)+
  tm_borders(col = "red")+
  #tm_shape(Merseyside)+
  #tm_borders(col = "blue")+
  tm_shape(LSOA_Dec)+
  tm_borders(col = "black")


tmap_save(
  tm = Map1,
  paste0("C:/Users/b9054751/OneDrive - Newcastle University/PhD/data/CW_2023/deciles_map.jpeg")
)


#Linear Regression


Index_Child = geojson_sf("https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_children_lsoa.geojson")%>%
  st_drop_geometry()
Index_Seniors = geojson_sf("https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_seniors_lsoa.geojson")%>%
  st_drop_geometry()

Index_Adult = geojson_sf("https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_adults_lsoa.geojson")%>%
  st_drop_geometry()%>%
  left_join(.,convert, by = "LSOA21CD")%>%
  filter(., !is.na(LSOA11CD))%>%
  left_join(., SAMHI, by = "LSOA11CD")%>%
  left_join(., IMD_rank%>%
              select(LSOA11CD, imd_rank), by = "LSOA11CD")%>%
  select(c(1,2,3,4,5,6,7,9,8,25,26))
  

library(corrplot)
cor(Index_Adult%>%
      select(-c(LSOA21CD, score_norm)), method = "pearson")%>%
  corrplot.mixed(., tl.pos = 'lt', tl.col = "black", sig.level = 0.05) 

lmModel_adult <- lm(samhi_rank ~ . , data = Index_Adult%>%
                    select(-c(LSOA21CD, score_norm, imd_rank)))


Index_Child = geojson_sf("https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_children_lsoa.geojson")%>%
  st_drop_geometry()%>%
  left_join(.,convert, by = "LSOA21CD")%>%
  filter(., !is.na(LSOA11CD))%>%
  left_join(., SAMHI, by = "LSOA11CD")%>%
  left_join(., IMD_rank%>%
              select(LSOA11CD, imd_rank), by = "LSOA11CD")%>%
  select(c(1,2,3,4,5,7,22, 23))

library(corrplot)
cor(Index_Child%>%
      select(-c(LSOA21CD, score_norm)), method = "pearson")%>%
  corrplot.mixed(., tl.pos = 'lt', tl.col = "black", sig.level = 0.05) 

lmModel_child <- lm(samhi_rank ~ . , data = Index_Child%>%
                      select(-c(LSOA21CD, score_norm, imd_rank)))

Index_Seniors = geojson_sf("https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_seniors_lsoa.geojson")%>%
  st_drop_geometry()%>%
  left_join(.,convert, by = "LSOA21CD")%>%
  filter(., !is.na(LSOA11CD))%>%
  left_join(., SAMHI, by = "LSOA11CD")%>%
  left_join(., IMD_rank%>%
              select(LSOA11CD, imd_rank), by = "LSOA11CD")%>%
  select(c(1,2,3,4,5,6,7,9,8,25, 26))

library(corrplot)
cor(Index_Seniors%>%
      select(-c(LSOA21CD, score_norm)), method = "pearson")%>%
  corrplot.mixed(., tl.pos = 'lt', tl.col = "black", sig.level = 0.05) 

lmModel_seniors <- lm(samhi_rank ~ . , data = Index_Seniors%>%
                      select(-c(LSOA21CD, score_norm, imd_rank)))




###All variables are used within the final selection when the variables are normalised 


library(jtools)
library(broom)
library(broom.mixed)
library(ggplot2)
#install.packages("ggstance")
library(ggstance)

#Thesis Plot
plot_summs(lmModel_adult , lmModel_child, lmModel_seniors,
                  model.names = c("Adults", "Children","Seniors"))
                  




library(texreg)
# export to doc
# export to html
texreg::htmlreg(list(lmModel_adult , lmModel_child, lmModel_seniors),file='models_html.html',
                custom.model.names = c("Adults", "Children","Seniors"),
                # custom.coef.names = c( "Intercept: Number of Tweets",
                #                        "Total Population",
                #                        "Area",
                #                        "Age 16-29 (%)",
                #                        "Age 30-44 (%)",
                #                        "Age 45-59 (%)",
                #                        "Age 60 (%)",
                #                        "Workday Population",
                #                        "Median House Price (£)",
                #                        "Qualification Level 4 or above (%)",
                #                        "Non-white Population (%)",
                #                        "Female Population (%)",
                #                        "Net Income (£)",
                #                        "Household: Couple with dep. child"),
                leading.zero = T,
                digits = 4)


lmModel_scores = Index_Adult%>% select(LSOA21CD, samhi_rank, adult_index = score_norm)%>%
  left_join(Index_Child%>%
              select(LSOA21CD, child_index = score_norm))%>%
  left_join(Index_Seniors%>%
              select(LSOA21CD, senior_index = score_norm))


  
cor(lmModel_scores%>%
      select(-c(LSOA21CD)), method = "pearson")%>%
  corrplot.mixed(., tl.pos = 'lt', tl.col = "black", sig.level = 0.05) 


lmModel_imd_seniors <- lm(samhi_rank ~ . , data = Index_Seniors%>%
                        select(-c(LSOA21CD, score_norm)))
lmModel_imd_adults <- lm(samhi_rank ~ . , data = Index_Adult%>%
                            select(-c(LSOA21CD, score_norm)))
lmModel_imd_child <- lm(samhi_rank ~ . , data = Index_Child%>%
                           select(-c(LSOA21CD, score_norm)))


texreg::htmlreg(list(lmModel_imd_adults , lmModel_imd_child, lmModel_imd_seniors),file='models_imd_html.html',
                custom.model.names = c("Adults", "Children","Seniors"),
                # custom.coef.names = c( "Intercept: Number of Tweets",
                #                        "Total Population",
                #                        "Area",
                #                        "Age 16-29 (%)",
                #                        "Age 30-44 (%)",
                #                        "Age 45-59 (%)",
                #                        "Age 60 (%)",
                #                        "Workday Population",
                #                        "Median House Price (£)",
                #                        "Qualification Level 4 or above (%)",
                #                        "Non-white Population (%)",
                #                        "Female Population (%)",
                #                        "Net Income (£)",
                #                        "Household: Couple with dep. child"),
                leading.zero = T,
                digits = 4)
