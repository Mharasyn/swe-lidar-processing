# Calculate basin-wide SWE using a linear regression of all density measurements (in situ) of snow across the whole basin, divided by season
# Created by Maddie Harasyn 2023/2024 (madison.harasyn@usask.ca)

# load libraries
library(tidyverse)
library(modelr)
library(ggpubr)
library(ggplot2)
library(raster)
library(fasterize)

# variables ###########################################################
#snow year to use for density calculation (the year that has Jan/Feb/Mar in the snow year)
snowyear <- 2023

#folder name to output swe maps to (in Z:\lidar-processing-basin\outputs\swe)
folder_name <- 'Reg_Basinwide'


#location of field data 
survey <- read.csv('Z:/lidar-processing-basin/data/survey_data/survey_points_seasonSorted.csv')%>%
  filter(!Site %in% c('Snow Experiment'))%>% #snow experiment not within basin, not in data 2022 and onwards
  mutate(Date=as.POSIXct(Date, "%m/%d/%Y", tz="Etc/GMT+6"))%>% ##If survey data isnt being read in properly, try changing the date format  %Y/%m-%d
  filter(Date >= as.Date(paste(as.character(snowyear - 1), "-09-01", sep="")) & Date <= as.Date(paste(as.character(snowyear),"-07-01", sep=""))) #filter survey data to only use snow year for regression calculation

full_survey <- read.csv('Z:/lidar-processing-basin/data/survey_data/survey_points_seasonSorted.csv')%>%
  filter(!Site %in% c('Snow Experiment')) #snow experiment not within basin, not in data 2022 and onwards

# data table conversions ###########################################################
#convert snow depth to mm in table
survey$Depth <- survey$Hs_insitu *1000

# plot depth vs density to check relationships #########
survey |>
  filter(is.na(Density) == F) |>
  filter(is.na(Season) == F) |>
  ggplot(aes(x = Hs_insitu, y = Density))+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) + #regression line plotting on graph

  geom_point(aes(colour = Site)) +
  facet_wrap(as.formula(paste("~", 'Season'))) +
  stat_cor(aes(label = paste(after_stat(rr.label))), label.x.npc = 0.65, label.y.npc = 0.2, size = 3) +
  stat_cor(aes(label = paste(after_stat(p.label))), label.x.npc = 0.65, label.y.npc = 0.05, size = 3) +
  theme_bw()

ggsave(paste0('outputs/swe/snYr', as.character(snowyear), '/', folder_name, '/RegressionPlot.png'), width = 6, height = 4)


# calculate the linear regression for each survey date followed process from: ####
# https://r4ds.had.co.nz/many-models.html#many-models

lm_model <- function(df) {
  lm(Density ~ Hs_insitu, data = df)
}

#create nest model for regression
nest <- survey |> 
  filter(is.na(Density) == F) |> 
  group_by_at('Season') |> 
  nest() |> 
  mutate(model = map(data, lm_model),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions))

##create table to categorize dates as spring or winter
seasonTable <- full_survey[c('Identifier', 'Season')] %>%
  group_by(Identifier) %>%
  slice(1)


###Sort tifs by season, apply regression model from that season to the lidar depth raster to get density raster ####
all_tifs <- list.files(path = "data/Hs/", pattern = '.tif')
output_statement <- c()
for (tif in all_tifs){
  if (seasonTable[[2]][[which(seasonTable$Identifier == substr(tif, 1,6))]] == 'Winter'){
    model_season <- nest$model[[which(nest$Season == 'Winter')]]
    raster_Hs <- raster(paste("data/Hs/", tif, sep=""))
    names(raster_Hs) <- 'Hs_insitu'
    raster_density <- predict(raster_Hs, model_season)
    
    raster_swe <- (raster_Hs * raster_density)
    writeRaster(raster_swe, paste0('outputs/swe/snYr', as.character(snowyear), '/', folder_name, '/', tif), overwrite = TRUE) #output .tif of swe map
    
    output_statement <- append(output_statement, tif)
    
  }
  else{ ##SPRING DATES
    model_season <- nest$model[[which(nest$Season == 'Spring')]]
    raster_Hs <- raster(paste("data/Hs/", tif, sep=""))
    names(raster_Hs) <- 'Hs_insitu'
    raster_density <- predict(raster_Hs, model_season)
    
    raster_swe <- (raster_Hs * raster_density)
    writeRaster(raster_swe, paste0('outputs/swe/snYr', as.character(snowyear), '/', folder_name, '/', tif), overwrite = TRUE) #output .tif of swe map
    
    output_statement <- append(output_statement, tif)
    
  }
}

#print output statement to tell user which method of SWE calc was applied to which tif
print(paste("SWE regression applied to: ", output_statement))

