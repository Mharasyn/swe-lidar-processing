## Code to sum SWE cell values to get total SWE for full basin
## Created by Maddie Harasyn 2023/2024 (madison.harasyn@usask.ca)

# load libraries
library(raster)
library(sf)

# variables ###########################################################
#snow year to use for density calculation (the year that has Jan/Feb/Mar in the snow year)
snowyear <- '2023'


# code ###########################################################
swe_folders <- list.dirs(paste0('Z:/lidar-processing-basin/outputs/swe/snYr', snowyear, '/'), recursive = FALSE)

for (folder in swe_folders){
  swe_print_sum <- c() #intiate list to append all SWE totals, to be exported to txt file at end of loop
  swe_tifs <- list.files(folder, pattern = '.tif')
  
  for (tif in swe_tifs){
    swe_print_sum <- append(swe_print_sum, substr(tif, 1, 6))
    swe_tif <- raster(paste0(folder, '/', tif))
    swe_sum <- sum(values(swe_tif), na.rm = TRUE) #sum all cell values in swe rasters
    
    swe_area_weighted <- (swe_sum) #/3142932.428) #area in sq m, divide swe by area of CRHM HRU areas to get area weighted SWE
    
    swe_print_sum <- append(swe_print_sum, swe_area_weighted)
    swe_print_sum <- append(swe_print_sum, "\n")
  }
  
  #output text file to corresponding folder, does not overwrite file!!
  cat(sprintf(swe_print_sum), "\n", file = paste0(folder, '/totalBasin_swe_by_day_sum.txt'))
}
