Packages = c("plyr", "ggplot2", "tmap", "basemaps", "raster", "geojsonsf", "corrplot", "biscale", "cowplot", "sf","dplyr", "tidyverse")

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
LSOA = geojson_sf("https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/LSOA.geojson")

Index_Adult = geojson_sf("https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_adults_lsoa.geojson")%>%
  st_drop_geometry()
Index_Child = geojson_sf("https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_children_lsoa.geojson")%>%
  st_drop_geometry()
Index_Seniors = geojson_sf("https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_seniors_lsoa.geojson")%>%
  st_drop_geometry()




#Step 1 - which sub-populations are failed by the index?
#Correlation matrix
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
         over_66 = over_66/total_households*100,
         children = dep1+dep2+dep3+dep4,
         children = children/total_households*100)%>%
  select(LSOA21CD, over_66, children)

Sex = read.csv("sex.csv")%>%
  select(LSOA21CD = 2,
         female_perc)
ethnic = read.csv("ethic_group.csv")%>%
  select(LSOA21CD = 2,
         white_perc)%>%
  mutate(white_perc = 100 - white_perc)
disability = read.csv("disability.csv")%>%
  separate(1, c("LSOA21CD", NA), sep = " : ")

vehicles = read.csv("no_vehicle.csv")%>%
  select(2,no_vehicle = 6)

general_health = read.csv("general_health.csv")%>%
  mutate(bad_verybad_health = (Bad.health+Very.bad.health)/Total..All.usual.residents*100)%>%
  select(2,6)

IMD_rank = read.csv("imd_score.csv")%>%
  select(1,2,5)%>%
  left_join(., read.csv("imd.csv"))%>%
  rename(LSOA11CD = 1)%>%
  left_join(., convert, by = "LSOA11CD")%>%
  select(imd_score, imd_rank, LSOA21CD, LSOA11CD)

SAMHI = read.csv("samhi.csv")%>%
  filter(., year=="2019")%>%
  rename(LSOA11CD = 1)%>%
  mutate(samhi_rank = rank(-samhi_index))%>%
  left_join(., convert, by = "LSOA11CD")%>%
  select(samhi_index, samhi_rank, LSOA21CD, LSOA11CD)

Liverpool= sf::st_read(dsn = (paste0(getwd(), "/LAD")),layer="LAD_DEC_2021_GB_BFC")%>%
  filter(., LAD21NM == "Liverpool")



Corr_data = hh%>%
  left_join(., Sex, by = "LSOA21CD")%>%
  left_join(.,ethnic, by = "LSOA21CD")%>%
  left_join(., disability, by = "LSOA21CD")%>%
  left_join(., vehicles, by = "LSOA21CD")%>%
  left_join(., general_health, by = "LSOA21CD")%>%
  left_join(., IMD_rank, by = "LSOA21CD")%>%
  left_join(., SAMHI, by = "LSOA21CD")%>%
  left_join(Index_Adult%>%
              select(LSOA21CD, adult_index = score),. , by = "LSOA21CD")%>%
  left_join(Index_Child%>%
              select(LSOA21CD, child_index = score),.,by = "LSOA21CD" )%>%
  left_join(Index_Seniors%>%
              select(LSOA21CD, senior_index = score),.,by = "LSOA21CD")%>%
  #filter(!is.na(imd_score))%>%
  #filter(!(LSOA21CD %in% c("E01034409", "E01034401", "E01034404")))
  select(-c(LSOA11CD.x, LSOA11CD.y))%>%
  group_by(LSOA21CD)%>%
  summarise_all(median)

Corr_data$imd_score[is.na(Corr_data$imd_score)]<- median(Corr_data$imd_score, na.rm = TRUE)
Corr_data$samhi_index[is.na(Corr_data$samhi_index)]<-median(Corr_data$samhi_index, na.rm = TRUE)

rm(hh, Sex, ethnic, disability, vehicles, general_health)
write.csv(Corr_data, "git/corrplot_variables.csv")

Corr_data%>%
  select(-c("LSOA21CD", "imd_rank", "samhi_rank"))%>%
  rename("Senior Index" = 1,
         "Child Index" = 2,
         "Adult Index" = 3,
         "Households over 66 (%)" = 4,
         "Households with children (%)" = 5,
         "Female (%)" = 6, 
         "Non-White (%)" = 7, 
         "Registered Disabled (%)" = 8,
         "No Car Household (%)" = 9,
         "Bad or Very Bad Health (%)" = 10,
         "IMD Score" = 11,
         "Samhi Score" = 12)%>%
  cor(., method = "pearson")%>%
  corrplot.mixed(., tl.pos = 'lt', tl.col = "black", sig.level = 0.05) 

#Bivariate plot

Corr_Vis = LSOA%>%
  left_join(., Corr_data, by = "LSOA21CD")%>%
  filter(!is.na(senior_index))
bivariate = Corr_Vis%>%
  select(c(2,13,14,15,16,17,18,19,20,21,22,23,24,25,26, 27))%>%
  bi_class(., x = adult_index, y = imd_score, style = "quantile", dim = 3, keep_factors = FALSE, dig_lab = 3)

map <- ggplot() +
  geom_sf(data = bivariate, mapping = aes(fill = bi_class), color = "grey", size = 0.05, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Comparing Accessibility and Deprivation"
  ) +
  bi_theme()+
  theme(plot.title = element_text(size = 14, face = "bold"))

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Accessibility Score",
                    ylab = "IMD Score",
                    size = 8)


Plot = ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.65, 0.55, 0.17, 0.22)
ggsave(
  paste0("C:/Users/b9054751/OneDrive - Newcastle University/PhD/Data/CW_2023/git/CW_2023/Figures/bivariate_acc_dep.jpeg"),
  plot = Plot
)


bivariate = Corr_Vis%>%
  select(c(2,13,14,15,16,17,18,19,20,21,22,23,24,25,26))%>%
  bi_class(., x = adult_index, y = no_vehicle, style = "quantile", dim = 3, keep_factors = FALSE, dig_lab = 3)

map <- ggplot() +
  geom_sf(data = bivariate, mapping = aes(fill = bi_class), color = "grey", size = 0.05, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Comparing Accessibility and Vehicle Availability"
  ) +
  bi_theme()+
  theme(plot.title = element_text(size = 14, face = "bold"))

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Accessibility Score",
                    ylab = "No Car Household (%)",
                    size = 8)


Plot = ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.65, 0.55, 0.17, 0.22)
ggsave(
  paste0("C:/Users/b9054751/OneDrive - Newcastle University/PhD/Data/CW_2023/git/CW_2023/Figures/bivariate_acc_vehicle.jpeg"),
  plot = Plot
)


adult_data = Corr_Vis%>%
  select(c(2,13,14,15,16,17,18,19,20,21,22,23,24,25, 26))%>%
  left_join(Index_Adult%>%
              select("LSOA21CD", adult_score = score))

box = adult_data%>%
  mutate(., rank = case_when(adult_score<=2 ~ "Lowest Accessibility",
                             adult_score>6 ~ "Highest Accessibility"))%>%
  filter(!is.na(rank))%>%
  select(-c("LSOA21CD", "imd_rank", "samhi_rank", "senior_index", "child_index", "adult_index"))%>%
  st_drop_geometry()%>%
  rename("Households over 66 (%)" = 1,
         "Households with children (%)" = 2,
         "Female (%)" = 3, 
         "Non-White (%)" = 4, 
         "Registered Disabled (%)" = 5,
         "No Car Household (%)" = 6,
         "Bad or Very Bad Health (%)" = 7)%>%
  gather(., population_metric, median_percentage, c(1,2,3,4,5,6,7), factor_key = TRUE )%>%
  ggplot(aes(x = population_metric, y = median_percentage, fill = rank))+
  geom_boxplot()+
  labs(y = "Population (%)",
       x = "Population Characteristic",
       title = "Comparing Population Characteristics of Highest and Lowest Accessibility")+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  ylim(0,80)
  
ggsave(
  paste0("C:/Users/b9054751/OneDrive - Newcastle University/PhD/Data/CW_2023/git/CW_2023/Figures/popn_acc_boxplot_adult.jpeg"),
  plot = box
)

Child_data = Corr_Vis%>%
  st_drop_geometry()%>%
  select(c(2,13,14,15,16,17,18,19,20,21,22,23,24,25, 26))%>%
  left_join(Index_Child%>%
              select("LSOA21CD", child_score = score))
  
box = Child_data%>%
  mutate(., rank = case_when(child_score<=1 ~ "Lowest Accessibility",
                             child_score>3 ~ "Highest Accessibility"))%>%
  filter(!is.na(rank))%>%
  select(-c("LSOA21CD", "imd_rank", "samhi_rank", "senior_index", "child_index", "adult_index"))%>%
  st_drop_geometry()%>%
  rename("Households over 66 (%)" = 1,
         "Households with children (%)" = 2,
         "Female (%)" = 3, 
         "Non-White (%)" = 4, 
         "Registered Disabled (%)" = 5,
         "No Car Household (%)" = 6,
         "Bad or Very Bad Health (%)" = 7)%>%
  gather(., population_metric, median_percentage, c(1,2,3,4,5,6,7), factor_key = TRUE )%>%
  ggplot(aes(x = population_metric, y = median_percentage, fill = rank))+
  geom_boxplot()+
  labs(y = "Population (%)",
       x = "Population Characteristic",
       title = "Comparing Population Characteristics of Highest and Lowest Accessibility")+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  ylim(0,80)

ggsave(
  paste0("C:/Users/b9054751/OneDrive - Newcastle University/PhD/Data/CW_2023/git/CW_2023/Figures/popn_acc_boxplot_child.jpeg"),
  plot = box
)
  
#Comparing SAMHI

ggplot(data = Corr_data, aes(x = child_index, y = samhi_index))+
  geom_point()

ggplot(data = Corr_data, aes(x = adult_index, y = samhi_index))+
  geom_point()


bivariate = Corr_Vis%>%
  select(c(2,13,14,15,16,17,18,19,20,21,22,23,24,25,26))%>%
  bi_class(., x = adult_index, y = samhi_index, style = "quantile", dim = 3, keep_factors = FALSE, dig_lab = 3)

map <- ggplot() +
  geom_sf(data = bivariate, mapping = aes(fill = bi_class), color = "grey", size = 0.05, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Comparing Accessibility and Mental Health Score"
  ) +
  bi_theme()+
  theme(plot.title = element_text(size = 14, face = "bold"))

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Accessibility Score",
                    ylab = "Mental Health",
                    size = 8)


Plot = ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.65, 0.55, 0.17, 0.22)
ggsave(
  paste0("C:/Users/b9054751/OneDrive - Newcastle University/PhD/Data/CW_2023/git/CW_2023/Figures/bivariate_acc_MH.jpeg"),
  plot = Plot
)


#Bivariate plot


bivariate = Corr_Vis%>%
  select(c(2,13,14,15,16,17,18,19,20,21,22,23,24,25, 26))%>%
  bi_class(., x = imd_score, y = samhi_index, style = "quantile", dim = 3, keep_factors = FALSE, dig_lab = 3)


LSOA_Dec = bivariate%>%
  mutate(imd_decile_liv = ntile(imd_score, 10),
         samhi_decile_liv = ntile(samhi_index, 10))%>%
  filter(., imd_decile_liv == 10 & samhi_decile_liv == 10)

map <- ggplot() +
  geom_sf(data = bivariate, mapping = aes(fill = bi_class), color = "grey", size = 0.05, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Comparing Mental Health and Deprivation"
  ) +
  bi_theme()+
  theme(plot.title = element_text(size = 14, face = "bold"))+
  geom_sf(data = LSOA_Dec, fill = NA, color = "black")
legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "IMD Rank",
                    ylab = "SAMHI Rank",
                    size = 8)


  Plot = ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.65, 0.55, 0.17, 0.22)
ggsave(
  paste0("C:/Users/b9054751/OneDrive - Newcastle University/PhD/Data/CW_2023/git/CW_2023/Figures/bivariate.jpeg"),
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
  paste0("C:/Users/b9054751/OneDrive - Newcastle University/PhD/data/CW_2023/git/CW_2023/Figures/deciles_map.jpeg")
)

write.csv(LSOA_Dec%>%
            st_drop_geometry(), "C:/Users/b9054751/OneDrive - Newcastle University/PhD/data/CW_2023/git/CW_2023/LSOA_Bottom_Deciles.csv")

#Linear Regression

Index_Adult = geojson_sf("https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_adults_lsoa.geojson")%>%
  st_drop_geometry()%>%
  left_join(.,convert, by = "LSOA21CD")%>%
  left_join(., SAMHI, by = "LSOA21CD")%>%
  left_join(., IMD_rank%>%
              select(LSOA21CD, imd_score), by = "LSOA21CD")%>%
  select(c(1,2,3,4,5,6,7,9,8,10,21,18))%>%
  group_by(LSOA21CD)%>%
  summarise_all(median)

Index_Adult$imd_score[is.na(Index_Adult$imd_score)]<- median(Index_Adult$imd_score, na.rm = TRUE)
Index_Adult$samhi_index[is.na(Index_Adult$samhi_index)]<-median(Index_Adult$samhi_index, na.rm = TRUE)

mean = data.frame(colMeans(Index_Adult%>% select(-LSOA21CD)))


library(corrplot)
cor(Index_Adult%>%
      select(-c(LSOA21CD, score_norm)), method = "pearson")%>%
  corrplot.mixed(., tl.pos = 'lt', tl.col = "black", sig.level = 0.05) 

lmModel_adult_samhi <- lm(samhi_index ~ . , data = Index_Adult%>%
                    select(-c(LSOA21CD, score, score_norm, imd_score)))
lmModel_adult_imd <- lm(imd_score ~ . , data = Index_Adult%>%
                      select(-c(LSOA21CD, score, score_norm, samhi_index)))


Index_Child = geojson_sf("https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_children_lsoa.geojson")%>%
  st_drop_geometry()%>%
  left_join(.,convert, by = "LSOA21CD")%>%
  left_join(., SAMHI, by = "LSOA21CD")%>%
  left_join(., IMD_rank%>%
              select(LSOA21CD, imd_score), by = "LSOA21CD")%>%
  select(c(1,2,3,4,5,6,7,18,15))%>%
  group_by(LSOA21CD)%>%
  summarise_all(median)

Index_Child$imd_score[is.na(Index_Child$imd_score)]<- median(Index_Child$imd_score, na.rm = TRUE)
Index_Child$samhi_index[is.na(Index_Child$samhi_index)]<-median(Index_Child$samhi_index, na.rm = TRUE)


library(corrplot)
cor(Index_Child%>%
      select(-c(LSOA21CD, score_norm)), method = "pearson")%>%
  corrplot.mixed(., tl.pos = 'lt', tl.col = "black", sig.level = 0.05) 


lmModel_child_samhi <- lm(samhi_index ~ . , data = Index_Child%>%
                            select(-c(LSOA21CD, score, score_norm, imd_score)))
lmModel_child_imd <- lm(imd_score ~ . , data = Index_Child%>%
                          select(-c(LSOA21CD, score, score_norm, samhi_index)))




Index_Seniors = geojson_sf("https://raw.githubusercontent.com/cpeiret/CW_2023/main/Data/index_seniors_lsoa.geojson")%>%
  st_drop_geometry()%>%
  left_join(.,convert, by = "LSOA21CD")%>%
  left_join(., SAMHI, by = "LSOA21CD")%>%
  left_join(., IMD_rank%>%
              select(LSOA21CD, imd_score), by = "LSOA21CD")%>%
  select(c(1,2,3,4,5,6,7,9,8,10,21,18))%>%
  group_by(LSOA21CD)%>%
  summarise_all(median)

Index_Seniors$imd_score[is.na(Index_Seniors$imd_score)]<- median(Index_Seniors$imd_score, na.rm = TRUE)
Index_Seniors$samhi_index[is.na(Index_Seniors$samhi_index)]<-median(Index_Seniors$samhi_index, na.rm = TRUE)



library(corrplot)
cor(Index_Seniors%>%
      select(-c(LSOA21CD, score_norm)), method = "pearson")%>%
  corrplot.mixed(., tl.pos = 'lt', tl.col = "black", sig.level = 0.05) 

lmModel_senior_samhi <- lm(samhi_index ~ . , data = Index_Seniors%>%
                            select(-c(LSOA21CD, score, score_norm, imd_score)))
lmModel_senior_imd <- lm(imd_score ~ . , data = Index_Seniors%>%
                          select(-c(LSOA21CD, score, score_norm, samhi_index)))




###All variables are used within the final selection when the variables are normalised 


library(jtools)
library(broom)
library(broom.mixed)
library(ggplot2)
#install.packages("ggstance")
library(ggstance)

#Thesis Plot
plot_summs(lmModel_adult_imd , lmModel_child_imd, lmModel_senior_imd,
                  model.names = c("Adults", "Children","Seniors"))
                  

plot_summs(lmModel_adult_samhi , lmModel_child_samhi, lmModel_senior_samhi,
           model.names = c("Adults", "Children","Seniors"))





library(texreg)
# export to doc
# export to html
texreg::htmlreg(list(lmModel_adult_imd , lmModel_child_imd, lmModel_senior_imd),file='git/CW_2023/Figures/imd_models_html.html',
                custom.model.names = c("Adults", "Children","Seniors"),
                 custom.coef.names = c( "Intercept:IMD",
                                        "Parks",
                                        "Schools",
                                        "Supermarkets",
                                        "Pharmacies",
                                        "ATMs",
                                        "Cinemas",
                                        "Gyms",
                                        "Playgrounds",
                                        "Pitches"),
                leading.zero = T,
                digits = 4)

Bottom_Dec = bivariate%>%
  mutate(imd_decile_liv = ntile(imd_score, 10),
         samhi_decile_liv = ntile(samhi_index, 10))%>%
  filter(., imd_decile_liv == 10 & samhi_decile_liv == 10)

Top_Dec = bivariate%>%
  mutate(imd_decile_liv = ntile(imd_score, 10),
         samhi_decile_liv = ntile(samhi_index, 10))%>%
  filter(., imd_decile_liv == 1 & samhi_decile_liv == 1)

Map1 = 
  tm_shape(basemap, bbox = bbox_new) + tm_rgb(saturation = 0.2) +
  tm_shape(Liverpool, bbox = bbox_new)+
  tm_borders(col = "red")+
  tm_shape(Top_Dec)+
  tm_borders(col = "blue")+
  tm_shape(Bottom_Dec)+
  tm_borders(col = "black")


tmap_save(
  tm = Map1,
  paste0("C:/Users/b9054751/OneDrive - Newcastle University/PhD/data/CW_2023/git/CW_2023/Figures/deciles_map.jpeg")
)


All_Indices = Index_Adult%>%
  select(LSOA21CD, Adult_Index = score)%>%
  left_join(.,Index_Child%>%
              select(LSOA21CD, Child_Index = score) )%>%
  left_join(., Index_Seniors%>%
              select(LSOA21CD, Senior_Index = score))%>%
  mutate(rank = case_when(LSOA21CD %in% Bottom_Dec$LSOA21CD ~ "Bottom Decile",
                          LSOA21CD %in% Top_Dec$LSOA21CD ~ "Top Decile"))%>%
  filter(!is.na(rank))

All_Indices%>%
  select(-LSOA21CD)%>%
  gather(., Index, Median_Score, Adult_Index:Senior_Index, factor_key = TRUE )%>%
  group_by(Index, rank)%>%
  summarise_all(median)%>%
  ggplot(aes(x = Index, y = Median_Score, fill = rank))+
  geom_bar(stat = "identity",position=position_dodge())

  

nrow(Index_Adult%>%
       filter(score==7))


Bottom_Dec = Index_Adult%>%
  filter(LSOA21CD %in% Bottom_Dec$LSOA21CD)%>%
  mutate(rank = "bottom")
Top_Dec = Index_Adult%>%
  filter(LSOA21CD %in% Top_Dec$LSOA21CD)%>%
  mutate(rank = "top")

Test = Index_Adult%>%
  mutate(rank = case_when(LSOA21CD %in% Bottom_Dec$LSOA21CD ~ "Bottom Decile",
                          LSOA21CD %in% Top_Dec$LSOA21CD ~ "Top Decile"))%>%
  select(-LSOA21CD)%>%
  gather(., destination, count, parks:gyms, factor_key = TRUE )%>%
  group_by(destination, rank)%>%
  summarise_all(median)%>%
  mutate(Index = "Adult")%>%
  
  rbind(., Index_Child%>%
          mutate(rank = case_when(LSOA21CD %in% Bottom_Dec$LSOA21CD ~ "Bottom Decile",
                                  LSOA21CD %in% Top_Dec$LSOA21CD ~ "Top Decile"))%>%
          select(-LSOA21CD)%>%
          gather(., destination, count, parks:pitches, factor_key = TRUE )%>%
          group_by(destination, rank)%>%
          summarise_all(median)%>%
          mutate(Index = "Child"))%>%
  rbind(., Index_Seniors%>%
          mutate(rank = case_when(LSOA21CD %in% Bottom_Dec$LSOA21CD ~ "Bottom Decile",
                                  LSOA21CD %in% Top_Dec$LSOA21CD ~ "Top Decile"))%>%
          select(-LSOA21CD)%>%
          gather(., destination, count, parks:gyms, factor_key = TRUE )%>%
          group_by(destination, rank)%>%
          summarise_all(median)%>%
          mutate(Index = "Senior"))%>%
  filter(!is.na(rank))%>%
  ggplot(aes(x = destination, y = count, fill = rank))+
  geom_bar(stat = "identity",position=position_dodge())+
  facet_wrap(vars(Index))
  

Top_Dec%>%
  rbind(Bottom_Dec)%>%
  select(-LSOA21CD)%>%
  gather(., destination, count, parks:gyms, factor_key = TRUE )%>%
  group_by(destination, rank)%>%
  summarise_all(median)%>%
  ggplot(aes(x = destination, y = count, fill = rank))+
  geom_bar(stat = "identity",position=position_dodge())


facet_wrap(vars(name), nrow = 1)





Top_Dec%>%
  rbind(Bottom_Dec)%>%
  gather(., destination, count, parks:gyms, factor_key = TRUE )%>%
  group_by(destination, rank)%>%
  summarise_all(median)%>%
  
  ggplot(aes(x = destination, y = count, fill = rank))+
  geom_bar(stat = "identity")
