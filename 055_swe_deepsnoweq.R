# Calculate basin-wide SWE using the deep snow equation (Pomeroy and Gray 1995), divided by season
# Created by Maddie Harasyn 2023/2024 (madison.harasyn@usask.ca)

# load libraries
library(tidyverse)
library(ggplot2)
library(terra)
library(raster)
library(sf)

# variables ###########################################################
#snow year to use for density calculation (the year that has Jan/Feb/Mar in the snow year)
snowyear <- 2023

#folder name to output swe maps to (in Z:\lidar-processing-basin\outputs\swe)
folder_name <- 'DeepSnow_Equation'


#location of field data 
survey <- read.csv('Z:/lidar-processing-basin/data/survey_data/survey_points_seasonSorted.csv')%>%
  filter(!Site %in% c('Snow Experiment'))%>% #snow experiment not within basin, not in data 2022 and onwards
  mutate(Date=as.POSIXct(Date, "%m/%d/%Y", tz="Etc/GMT+6"))%>% ##If survey data isnt being read in properly, try changing the date format  %Y/%m-%d
  filter(Date >= as.Date(paste(as.character(snowyear - 1), "-09-01", sep="")) & Date <= as.Date(paste(as.character(snowyear),"-07-01", sep=""))) #filter survey data to only use snow year for regression calculation

full_survey <- read.csv('Z:/lidar-processing-basin/data/survey_data/survey_points_seasonSorted.csv')%>%
  filter(!Site %in% c('Snow Experiment')) #snow experiment not within basin, not in data 2022 and onwards

# data table conversions ###########################################################
#convert snow depth to cm in table
survey$Depth <- survey$Hs_insitu *100

## Deep snow equations ####
#define deep snow equations, working with Depth column which is in cm
##adds an extra column to the survey data that calculates SWE based on deep snow equations (>60 cm, dry and wet equation)
sweEq_winter <- function(x){((4.88*x) -204.7*(1-exp(-x/67.3)))} 
sweEq_spring <- function(x){((5.22*x) -204.7*(1-exp(-x/67.3)))}

densityEq_winter <- function(x){((488 - (20470/(x))) *(1-exp(-(x)/67.3)))} 
densityEq_spring <- function(x){((522 - (20470/(x))) *(1-exp(-(x)/67.3)))} 

survey$deep_SWE<-NA
for (i in 1:nrow(survey)){
  if(survey$Depth[i] >= 60 & survey$Season[i] == 'Winter') {
    survey$deep_SWE[i] = sweEq_winter(survey$Depth[i])}
  else if(survey$Depth[i] >= 60 & survey$Season[i] == 'Spring'){
    survey$deep_SWE[i] = sweEq_spring(survey$Depth[i])  }
  else {
    survey$deep_SWE[i] <- NA
  }
}

# plot depth vs density against deep snow function #########
survey |>
  filter(is.na(Density) == F) |>
  filter(Season == "Spring") |>
  ggplot(aes(x = Depth, y = Density)) +
  labs(title = "Spring", x = "Depth (cm)", y = "Density (kg/m3)") +
  stat_function(fun = densityEq_winter) +
  ylim(0, 500) +
  xlim(60, 300) +
  geom_point(aes(colour = Site)) +
  theme_bw()

ggsave(paste0('outputs/swe/snYr', as.character(snowyear), '/', folder_name, '/DeepSnowEq_Spring.png'), width = 6, height = 5)

#### Density equations ######
#make a nest to store the average densities for each Season - apply on snow > 0.6 m
nest <- survey %>%
  group_by(Season)%>% 
  nest () %>%
  mutate(mean_density = map_dbl(data, ~ mean(.x$Density, na.rm=TRUE)),
         stdv_density = map_dbl(data, ~ sd(.x$Density, na.rm=TRUE)))%>% 
  nest()

# deep snow equations to apply to snow depth rasters #####
dry_equation <- function(x) { #need to multiply raster vals by 100 to convert to cm for deep snow equation
  ifelse(x >= 0.6, (4.88*(x*100))-204.7*(1-exp(-(x*100)/67.3)), x * nest$data[[which(nest$Season == 'Winter')]][[2]])  # Applies deep snow eq if greater or equal to 0.6 m, otherwise use avg basin wide density 
}

wet_equation <- function(x) {
  ifelse(x >= 0.6, (5.22*(x*100))-204.7*(1-exp(-(x*100)/67.3)), x * nest$data[[which(nest$Season == 'Spring')]][[2]])  # Applies deep snow eq if greater or equal to 0.6 m, otherwise use avg basin wide density 
}

##create table to categorize dates as spring or winter
seasonTable <- full_survey[c('Identifier', 'Season')] %>%
  group_by(Identifier) %>%
  slice(1)

###Sort tifs by season, apply avg density from that season to the lidar depth raster to get density raster ####
all_tifs <- list.files(path = "data/Hs/", pattern = '.tif')
output_statement <- c()
for (tif in all_tifs){ ##WINTER DATES
  if (seasonTable[[2]][[which(seasonTable$Identifier == substr(tif, 1,6))]] == 'Winter'){
    raster_Hs <- raster(paste("data/Hs/", tif, sep=""))
    names(raster_Hs) <- 'Hs_insitu'
    raster_swe <- calc(raster_Hs, fun = dry_equation)
    
    writeRaster(raster_swe, paste0('outputs/swe/snYr', as.character(snowyear), '/', folder_name, '/', tif), overwrite = TRUE) #output .tif of swe map
    
  }
  else{ ##SPRING DATES
    raster_Hs <- raster(paste("data/Hs/", tif, sep=""))
    names(raster_Hs) <- 'Hs_insitu'
    raster_swe <- calc(raster_Hs, fun = wet_equation)
    
    writeRaster(raster_swe, paste0('outputs/swe/snYr', as.character(snowyear), '/', folder_name, '/', tif), overwrite = TRUE) #output .tif of swe map

  }
}
