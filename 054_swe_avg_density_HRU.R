# Calculate basin-wide SWE using the average of density measurements for each HRU (in situ) of snow across the whole basin, divided by season
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
folder_name <- 'Avg_Sitewide'

#what to group the data by to calculate regression
group_by <- 'Season'

#load HRU to partition snow depth tif, replace landcover names with survey names
HRU_shp <- st_read(file.path('hru/', "Fortress_HRU.shp"))

#location of field data 
survey <- read.csv('Z:/lidar-processing-basin/data/survey_data/survey_points_seasonSorted.csv')%>%
  filter(!Site %in% c('Snow Experiment'))%>% #snow experiment not within basin, not in data 2022 and onwards
  mutate(Date=as.POSIXct(Date, "%m/%d/%Y", tz="Etc/GMT+6"))%>% ##If survey data isnt being read in properly, try changing the date format  %Y/%m-%d
  filter(Date >= as.Date(paste(as.character(snowyear - 1), "-09-01", sep="")) & Date <= as.Date(paste(as.character(snowyear),"-07-01", sep=""))) #filter survey data to only use snow year for regression calculation

full_survey <- read.csv('Z:/lidar-processing-basin/data/survey_data/survey_points_seasonSorted.csv')%>%
  filter(!Site %in% c('Snow Experiment')) #snow experiment not within basin, not in data 2022 and onwards

###############################################################
#convert snow depth to mm in table
survey$Depth <- survey$Hs_insitu *1000

#change HRU classes to match snow survey names
colnames(HRU_shp)[2] <- 'Site'
HRU_shp[[2]][[1]] <- 'Powerline' #Conifer Forest HRU
HRU_shp[[2]][[2]] <- 'Canadian Ridge' #Slope forest north facing HRU
HRU_shp[[2]][[3]] <- 'Bonsai' #Valley clearing/water HRU
HRU_shp[[2]][[4]] <- 'Fortress Ridge' #Shrubland/ridge HRU
HRU_shp[[2]][[7]] <- 'Fortress Ridge South' #Slope forest south facing HRU

######## PLOTS ######################
#plot hru shapefile
par(mar=c(0,0,0,0))
plot(st_geometry(HRU_shp), col = sf.colors(7, categorical = TRUE), lwd=0.25, border='black')

# plot depth vs density to check relationships #########
survey |>
  filter(is.na(Density) == F) |>
  ggplot(aes(x = Hs_insitu, y = Density))+
  
  geom_point(aes(colour = Season)) +
  facet_grid(Season ~ Site) +
  labs(x = 'Snow Depth (m)')+
  theme_bw()

ggsave(paste0('outputs/swe/snYr', as.character(snowyear), '/', folder_name, '/HRUavgPlot.png'), width = 8, height = 4)


#make a nest to store the average densities for each date + site
nest_winter <- survey %>%
  filter(Season == 'Winter')%>% 
  group_by(Site, Season)%>% 
  nest () %>%
  mutate(mean_density = map_dbl(data, ~ mean(.x$Density, na.rm=TRUE)),
         stdv_density = map_dbl(data, ~ sd(.x$Density, na.rm=TRUE)))%>% 
  nest()

nest_spring <- survey %>%
  filter(Season == 'Spring')%>%
  group_by(Site, Season)%>% 
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
  example_rast <- raster(paste("data/Hs/", all_tifs[1], sep="")) #####initiate empty raster with correct bounds
  raster_dens <-raster()
  crs(raster_dens) <- crs(example_rast) 
  extent(raster_dens) <- extent(624659, 626840, 5630875, 5633573) #extent of 2022 bare ground flight
  res(raster_dens) <- res(example_rast)
  
  if (seasonTable[[2]][[which(seasonTable$Identifier == substr(tif, 1,6))]] == 'Winter'){
    raster_Hs <- raster(paste("data/Hs/", tif, sep=""))
    names(raster_Hs) <- 'Hs_insitu'
    
    HRU_temp <- HRU_shp
    HRU_factor <- merge(HRU_temp, nest_winter, by=c('Site') ,all=F) #merge shapefile and site regression
    HRU_reg <- HRU_factor[, c(1, 6)] #create HRU_dens that only includes relevant info to be multiplied by lidar snow depth
    
    ##iterate through HRUs and apply corresponding model to each HRU extent, then merge rasters together
    for (HRU in HRU_reg[[1]]){
      HRU_avg <- HRU_reg$data[[which(HRU_reg$Site == HRU)]][[2]] #sets HRU_model to the corresponding HRU
      HRU_extent <- as(HRU_reg$geometry[[which(HRU_reg$Site == HRU)]], "Spatial") ## load in the multipolygon extent of the HRU, convert it to spatial polygon for mask
      raster_crop <- mask(raster_Hs, HRU_extent)
      
      HRU_density <- (raster_crop * HRU_avg)
      raster_dens <- mosaic(HRU_density, raster_dens, fun = max) #mosaic HRU density rasters together to create full picture of basin
    }
    
    raster_swe <- raster_dens 
    writeRaster(raster_swe, paste0('outputs/swe/snYr', as.character(snowyear), '/', folder_name, '/', tif), overwrite = TRUE) #output .tif of swe map
    output_statement <- append(output_statement, tif)
    
  }
  else{ ##SPRING DATES
    raster_Hs <- raster(paste("data/Hs/", tif, sep=""))
    names(raster_Hs) <- 'Hs_insitu'
    
    HRU_temp <- HRU_shp
    HRU_factor <- merge(HRU_temp, nest_spring, by=c('Site') ,all=F) #merge shapefile and site regression
    HRU_reg <- HRU_factor[, c(1, 6)] #create HRU_dens that only includes relevant info to be multiplied by lidar snow depth
    
    ##iterate through HRUs and apply corresponding model to each HRU extent, then mosaic rasters together
    for (HRU in HRU_reg[[1]]){
      HRU_avg <- HRU_reg$data[[which(HRU_reg$Site == HRU)]][[2]] #sets HRU_model to the corresponding HRU
      HRU_extent <- as(HRU_reg$geometry[[which(HRU_reg$Site == HRU)]], "Spatial") ## load in the multipolygon extent of the HRU, convert it to spatial polygon for mask
      raster_crop <- mask(raster_Hs, HRU_extent)
      
      HRU_density <- (raster_crop * HRU_avg)
      raster_dens <- mosaic(HRU_density, raster_dens, fun = max) #mosaic HRU density rasters together to create full picture of basin
    }
    
    raster_swe <- raster_dens
    writeRaster(raster_swe, paste0('outputs/swe/snYr', as.character(snowyear), '/', folder_name, '/', tif), overwrite = TRUE) #output .tif of swe map
    output_statement <- append(output_statement, tif)
    
  }
}

#print output statement to tell user which method of SWE calc was applied to which tif
print(paste("SWE regression applied to: ", output_statement))


#output text file stating average density used
avgdens_statement <- c('Winter Avg Densities:\n')
for (HRU in nest_winter[[2]]){
  HRU_dens <- nest_winter$data[[which(HRU_reg$Site == HRU)]][[2]]
  avgdens_statement <- append(avgdens_statement, paste0(as.character(HRU), ':', as.character(HRU_dens), '\n'))
}
avgdens_statement <- append(avgdens_statement, c('Spring Avg Densities:\n'))
for (HRU in nest_spring[[2]]){
  HRU_dens <- nest_spring$data[[which(HRU_reg$Site == HRU)]][[2]]
  avgdens_statement <- append(avgdens_statement, paste0(as.character(HRU), ':', as.character(HRU_dens), '\n'))
}

cat(sprintf(avgdens_statement), "\n", file = paste0('outputs/swe/snYr', as.character(snowyear), '/', folder_name, '/avgdensity_season_HRU.txt'))

