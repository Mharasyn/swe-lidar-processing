# Calculate basin-wide SWE using the average of all density measurements (in situ) of snow across the whole basin, divided by season
# Created by Maddie Harasyn 2023/2024 (madison.harasyn@usask.ca)

# load libraries
library(tidyverse)
library(ggplot2)
library(terra)
library(raster)

# variables ###########################################################
#snow year to use for density calculation (the year that has Jan/Feb/Mar in the snow year)
snowyear <- 2023

#folder name to output swe maps to (in Z:\lidar-processing-basin\outputs\swe)
folder_name <- 'Avg_Basinwide'


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

# plot depth vs density to see spread of densities #########
survey |>
  filter(is.na(Density) == F) |>
  ggplot(aes(x = Hs_insitu, y = Density))+
  geom_point(aes(colour = Site)) +
  facet_wrap(~ Season) +
  theme_bw()

ggsave(paste0('outputs/swe/snYr', as.character(snowyear), '/', folder_name, '/AveragePlot.png'), width = 6, height = 3)

#### Calculate avg density for each season ######
#make a nest to store the average densities for each Season
nest <- survey %>%
  group_by(Season)%>% 
  nest () %>%
  mutate(mean_density = map_dbl(data, ~ mean(.x$Density, na.rm=TRUE)),
         stdv_density = map_dbl(data, ~ sd(.x$Density, na.rm=TRUE)))%>% 
  nest()

##create table to categorize dates as spring or winter
seasonTable <- full_survey[c('Identifier', 'Season')] %>%
  group_by(Identifier) %>%
  slice(1)

###Sort tifs by season, apply avg density from that season to the lidar depth raster to get density raster ####
all_tifs <- list.files(path = "data/Hs/", pattern = '.tif')
output_statement <- c()
for (tif in all_tifs){
  if (seasonTable[[2]][[which(seasonTable$Identifier == substr(tif, 1,6))]] == 'Winter'){
    raster_Hs <- raster(paste("data/Hs/", tif, sep=""))
    names(raster_Hs) <- 'Hs_insitu'
    avg_dens <- nest$data[[which(nest$Season == 'Winter')]][[2]]
    
    raster_swe <- (raster_Hs * avg_dens)

    writeRaster(raster_swe, paste0('outputs/swe/snYr', as.character(snowyear), '/', folder_name, '/', tif), overwrite = TRUE) #output .tif of swe map
    output_statement <- append(output_statement, tif)
    
  }
  else{ ##SPRING DATES
    raster_Hs <- raster(paste("data/Hs/", tif, sep=""))
    names(raster_Hs) <- 'Hs_insitu'
    avg_dens <- nest$data[[which(nest$Season == 'Spring')]][[2]]
    
    raster_swe <- (raster_Hs * avg_dens)
    
    writeRaster(raster_swe, paste0('outputs/swe/snYr', as.character(snowyear), '/', folder_name, '/', tif), overwrite = TRUE) #output .tif of swe map
    output_statement <- append(output_statement, tif)
    
  }
}

#print output statement to tell user which method of SWE calc was applied to which tif
print(paste("SWE calcuated for: ", output_statement))

#output text file stating which files used avg density SWE
avgdens_statement <- paste0('Winter average density: ', as.character(nest[[2]][[1]][[2]]), '  Spring average density: ', as.character(nest[[2]][[2]][[2]]))
cat(sprintf(avgdens_statement), "\n", file = paste0('outputs/swe/snYr', as.character(snowyear), '/', folder_name, '/avgdensity_season.txt'))

