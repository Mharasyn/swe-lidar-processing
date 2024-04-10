# Process snow DSM .tifs into snow depth .tifs
# Original script by Phillip Harder January  27, 2023
# Updated by Maddie Harasyn 2023

# load libraries
library(dplyr)
library(purrr)
library(raster)
library(rgdal)
library(sf)

# variables ###########################################################

#process year
proc_year <- '2021' #have to process tifs one year at a time to preserve RAM space

#bare ground file name
bare_ground_tif = "22_292.tif"

####create lists and indices needed for script####
tiffs <- list.files(paste("data/dsm/", proc_year, "/", sep=""), pattern="*.tif$")
bare_index = which(tiffs == bare_ground_tif)
DSM_temp<-raster(paste("data/dsm/", proc_year, "/",tiffs[1], sep="")) #this line initiates the DSM_temp variable with the first raster in the list to keep indexing consistent
DSM_crs <- crs(DSM_temp)

####extent of the bare ground flight to use to extent the extents of all other dsms####
bare_extent <- extent(624659, 626840, 5630875, 5633573) #extent of 2022 bare ground flight


####create raster stack of DEMs####
#loop converted to apply function to make processing more efficient
#stacked raster created for each survey area
#some rasters do not include certain surveys, have to increase all raster extents to be the same
#fill 'empty space' with NA, so areas with no lidar data will return NA for snow depth
#add bare ground as first raster in DSM_stack, which will change the indexing of the tifs
DSM_extnt<-raster::extend(DSM_temp, bare_extent, value = NA)
DSM_stack<-DSM_extnt #this line initiates the DSM_stack with the tif at index 1
crs(DSM_stack) <- DSM_crs

for(i in 2:length(tiffs)) {
  DSM_temp <- raster(paste("data/dsm/", proc_year, "/", tiffs[[i]], sep=""))
  DSM_extnt <- raster::extend(DSM_temp, bare_extent, value = NA)
  crs(DSM_extnt) <- DSM_crs
  DSM_stack <- addLayer(DSM_stack, DSM_extnt)
}


####Output Hs tifs####
snow_index<-1:length(tiffs)
snow_index<-snow_index[snow_index!=bare_index]

for(i in snow_index){
    SD_temp <- DSM_stack[[i]] - DSM_stack[[bare_index]]
    SD_temp[SD_temp < 0] <- 0 #set -ve snow depths to 0 (-ve values are predominantly within 5 cm, can be labeled as noise/frost affected)
    names(SD_temp) <- names(DSM_stack[[i]])
    raster::writeRaster(SD_temp, paste0('data/Hs/', gsub("X", "", names(DSM_stack[[i]])), '.tif'), overwrite = TRUE)
}

