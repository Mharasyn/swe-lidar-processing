# Compare in situ snow depth and lidar snow depth using Hs .tifs and field survey data
# Original script by Phillip Harder January  27, 2023
# Updated by Maddie Harasyn 2023

# load libraries
library(dplyr)
library(purrr)
library(raster)
library(rgdal)
library(sf)

# variables ###########################################################

#location of field data (output from rover_snow_processing.R) 
survey<-read.csv('data/survey_data/survey_points_2019_2023.csv') %>%
  filter(Identifier != '22_047') #Bad drone data!

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

#convert long lat to UTM function
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}


####Create snow depth raster stack SD####
SD_tiffs <- list.files(paste("data/Hs/", sep=""), pattern="*.tif$")
SD<-raster(paste("data/HS/", SD_tiffs[1], sep="")) #this line initiates the DSM_temp variable with the first raster in the list to keep indexing consistent

for(i in 2:length(SD_tiffs)) {
  SD_temp <- raster(paste("data/Hs/", SD_tiffs[[i]], sep=""))
  SD <- addLayer(SD, SD_temp)
}


####Convert lat long to UTM####
survey$X_utm<-NA
survey$Y_utm<-NA
survey$zone<-11
for(i in 1:length(survey$Identifier)){
  survey[i,c('X_utm', 'Y_utm')]<-LongLatToUTM(survey$Longitude[i],survey$Latitude[i],survey$zone[i])[2:3]
}

####compute snow surface and ground surface elevations####
survey$z_type<-'g'
survey$z_type[which(!is.na(survey$Hs_insitu)|survey$Hs_insitu!=0)]<-'s'
survey$gnss_z_snow<-survey$z
survey$z_soil<-survey$z
survey$z_soil[which(survey$z_type=='s')]<-survey$z[which(survey$z_type=='s')]-survey$Hs_insitu[which(survey$z_type=='s')] #z_soil = gnss snow height - in situ snow depth


####Snow depth assessment####
survey$Hs_lidar<-NA
SD_names <- names(SD[[1:length(names(SD))]])
for(i in SD_names){
  survey$Hs_lidar[which(survey$Identifier==gsub("X", "",i))]<- raster::extract(SD[[i]],SpatialPoints(cbind(survey$X_utm[which(survey$Identifier==gsub("X", "",i))],
                                                                                                               survey$Y_utm[which(survey$Identifier==gsub("X", "",i))])))
}

####In situ Hs - lidar Hs for each point, added as a new column in survey table####
survey$lidar_minus_insitu <- survey$Hs_lidar - survey$Hs_insitu


####Statistics calculations####
surv_point_error <- survey %>%
  dplyr::group_by(Site) %>% dplyr::summarise(
    lidar_insitu_Hs_RMSE_points=RMSE(Hs_insitu,Hs_lidar), #RMSE of survey vs lidar snow depth
    lidar_insitu_Hs_Bias_points=bias(Hs_insitu,Hs_lidar), #Bias of survey vs lidar snow depth
    lidar_insitu_Hs_r2_points=r2fun(Hs_insitu,Hs_lidar) #R2 of survey vs lidar snow depth
  )

surv_point_error_date <- survey %>%
  dplyr::group_by(Identifier) %>% dplyr::summarise(
    lidar_insitu_Hs_RMSE_points=RMSE(Hs_insitu,Hs_lidar), #RMSE of survey vs lidar snow depth
    lidar_insitu_Hs_Bias_points=bias(Hs_insitu,Hs_lidar), #Bias of survey vs lidar snow depth
    lidar_insitu_Hs_r2_points=r2fun(Hs_insitu,Hs_lidar) #R2 of survey vs lidar snow depth
  )

surv_point_error_all <- survey %>%
 dplyr::summarise(
    lidar_insitu_Hs_RMSE_points=RMSE(Hs_insitu,Hs_lidar), #RMSE of survey vs lidar snow depth
    lidar_insitu_Hs_Bias_points=bias(Hs_insitu,Hs_lidar), #Bias of survey vs lidar snow depth
    lidar_insitu_Hs_r2_points=r2fun(Hs_insitu,Hs_lidar) #R2 of survey vs lidar snow depth
  )

write.csv(survey, 'data/error_summary/Fortress_survey_data.csv')
write.csv(surv_point_error, 'data/error_summary/Fortress_point_error.csv')
write.csv(surv_point_error_date, 'data/error_summary/Fortress_error_byDate.csv')
write.csv(surv_point_error_all, 'data/error_summary/Fortress_error_basinwide.csv')

####export shape file of all survey points to be read into arcgis####
####remove points with lidar Hs as nan - not supported in arcgis ###
survey_point <- subset(survey, !is.na(survey$Hs_lidar))
points_df <- data.frame(y = survey_point$Latitude, x = survey_point$Longitude, id = survey_point$Identifier, site = survey_point$Site, 
                        Hs_insitu = survey_point$Hs_insitu, Hs_lidar = survey_point$Hs_lidar, lidar_minus_insitu = survey_point$lidar_minus_insitu)
points_sf <- st_as_sf(points_df, coords = c("x", "y"))
st_crs(points_sf) <- 4326
st_write(points_sf, paste("data/error_summary/survey_points.shp",sep = ""), append=FALSE)
