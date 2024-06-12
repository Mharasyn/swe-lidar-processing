# Get area weighted SWE for each landscape unit, also average SWE of manual measurements for each site
# Maddie Harasyn 2024test

# load libraries
library(dplyr)
library(purrr)
library(raster)
library(rgdal)
library(sf)

# variables ###########################################################

#location of field data (output from rover_snow_processing.R) 
survey<-read.csv('data/survey_data/survey_points_seasonSorted.csv') %>%
  filter(!is.na(Density)) %>%
  filter(!Site %in% c('Snow Experiment')) #snow experiment not within basin, not in data 2022 and onwards
survey$insitu_SWE <- survey$Hs_insitu * survey$Density

#load HRU to partition snow depth tif, replace landcover names with survey names
HRU_shp <- st_read(file.path('hru/', "Fortress_HRU.shp"))

#snow year to use for density calculation (the year that has Jan/Feb/Mar in the snow year)
snowyear <- 2023

# functions ##########################################################
#error metric functions
#Root Mean Square Error
RMSE <- function(obs, est) {
  i<-which(!is.na(obs)&!is.na(est))
  sqrt(sum((est[i]-obs[i])^2)/length(est[i]))
}
#bias
bias<-function(obs,est){
  i<-which(!is.na(obs)&!is.na(est))
  sum(est[i]-obs[i])/length(est[i])
}  
#r^2 coefficient of determination
r2fun <- function(obs, pred){
  i<-which(!is.na(obs)&!is.na(pred))
  ((length(pred[i])*(sum(pred[i]*obs[i]))-sum(obs[i])*sum(pred[i]))/
      (((length(pred[i])*sum(obs[i]^2)-sum(obs[i])^2)^0.5)*((length(pred[i])*sum(pred[i]^2)-sum(pred[i])^2)^0.5)))^2
}

### group manual survey data to provide mean SWE for each site for each day
mean_manual_SWE <- survey %>%
  group_by(Identifier, Site) %>%
  summarise(mean_manual_SWE = mean(insitu_SWE, na.rm = TRUE))

# write.table(mean_manual_SWE, file = paste0('outputs/swe/snYr2023/mean_manual_SWE_bySite.txt'), sep = "\t", row.names = FALSE)




# sum SWE values in raster ###########################################################
swe_folders <- list.dirs(paste0('Z:/lidar-processing-basin/outputs/swe/snYr', snowyear, '/'), recursive = FALSE)

for (folder in swe_folders){
  swe_print_sum <- c() #intiate list to append all SWE totals, to be exported to txt file at end of loop
  swe_tifs <- list.files(folder, pattern = '.tif')
  
  for (tif in swe_tifs){
    swe_print_sum <- append(swe_print_sum, substr(tif, 1, 6))
    swe_print_sum <- append(swe_print_sum, "\n")
    
    for (HRU in HRU_shp$Type){
      swe_tif <- raster(paste0(folder, '/', tif))
      HRU_extent <- as(HRU_shp$geometry[[which(HRU_shp$Type == HRU)]], "Spatial") ## load in the multipolygon extent of the HRU, convert it to spatial polygon for mask
      
      raster_crop <- mask(swe_tif, HRU_extent)
      
      swe_sum <- sum(values(raster_crop), na.rm = TRUE) #sum all cell values in landscape unit area
      
      
      #NOTE NCELL NOT COUNTING NUMBER OF CELLS WITH VALUES, COUNTING ALL CELLS
      swe_area_weighted <- (swe_sum / (ncell(raster_crop) - freq(raster_crop, value=NA))) #area in sq m, divide swe by area of HRU areas to get area weighted SWE
      
      swe_print_sum <- append(swe_print_sum, HRU)
      swe_print_sum <- append(swe_print_sum, swe_area_weighted)
      swe_print_sum <- append(swe_print_sum, "\n")
    
    }
  }
  
  #output text file to corresponding folder, does not overwrite file!!
  # cat(sprintf(swe_print_sum), "\n", file = paste0(folder, '/landUnit_swe_by_day_sum.txt'))
}


#plot raster for viewing
plot(raster_crop)
