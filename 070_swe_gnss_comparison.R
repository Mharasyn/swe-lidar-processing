# Extract SWE values for all 5 methods for GNSS points
# Maddie Harasyn 2024

# load libraries
library(dplyr)
library(purrr)
library(raster)
library(rgdal)
library(sf)
library(ggplot2)

# variables ###########################################################

#location of field data (output from rover_snow_processing.R) 
survey<-read.csv('data/survey_data/survey_points_seasonSorted.csv') %>%
  filter(Site != 'Powerline') %>% 
  filter(!is.na(Density)) %>%
  filter(!Site %in% c('Snow Experiment')) #snow experiment not within basin, not in data 2022 and onwards

survey$insitu_SWE <- survey$Hs_insitu * survey$Density

#snow year that SWE was calculated for
snow_year <- "2023"

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



####Convert lat long to UTM####
survey$X_utm<-NA
survey$Y_utm<-NA
survey$zone<-11
for(i in 1:length(survey$Identifier)){
  survey[i,c('X_utm', 'Y_utm')]<-LongLatToUTM(survey$Longitude[i],survey$Latitude[i],survey$zone[i])[2:3]
}



####Create SWE raster stack SD####
## Loop that creates a stack of SWE rasters, then extracts SWE values at each GNSS point into 'survey' table (coordinated to 'method'/folder name)
swe_methods <- list.dirs(paste("outputs/swe/snYr", snow_year, "/", sep=""), recursive = FALSE, full.names = FALSE)

for (method in swe_methods){
  
  SWE_tiffs <- list.files(paste("outputs/swe/snYr", snow_year, "/", method, '/', sep=""), pattern="*.tif$")
  SWE<-raster(paste("outputs/swe/snYr", snow_year, "/", method, '/', SWE_tiffs[1], sep="")) #this line initiates the DSM_temp variable with the first raster in the list to keep indexing consistent
  
  for(i in 2:length(SWE_tiffs)) {
    SWE_temp <- raster(paste("outputs/swe/snYr", snow_year, "/", method, '/', SWE_tiffs[[i]], sep=""))
    SWE <- addLayer(SWE, SWE_temp)
  }
  
  ####Add column to survey data for each method of SWE####
  survey[[method]]<-NA
  SWE_names <- names(SWE[[1:length(names(SWE))]])
  
  for(i in SWE_names){
    survey[[method]][which(survey$Identifier==gsub("X", "",i))] <- raster::extract(SWE[[i]],SpatialPoints(cbind(survey$X_utm[which(survey$Identifier==gsub("X", "",i))],
                                                                                                             survey$Y_utm[which(survey$Identifier==gsub("X", "",i))])))
  }
}

# write.csv(survey, paste0("outputs/swe/snYr", snow_year, "/", 'SWE_GNSSpoints_all.csv'))

###Plot manual SWE vs each method SWE
SWE_method <- "DeepSnow_Equation"

# plot depth vs density to see spread of densities #########
survey |>
  filter(is.na(.data[[SWE_method]]) == F) |>
  ggplot(aes(x = insitu_SWE, y = .data[[SWE_method]]))+
  geom_point(aes(color = Season)) +
  scale_color_manual(values = c("Winter" = "#5b9ad5", "Spring" = "#ed7c31")) +
  xlim(0, 1250) +
  ylim(0, 1250) +
  xlab("Manual SWE (mm)") +
  ylab("Pomeroy & Gray (1995) SWE (mm)") + #Pomeroy & Gray (1995) / Avg. Lumped Basin / Avg. Landscape Strat.
  geom_abline(slope = 1, intercept = 0) + # Add 1:1 line
  theme_bw()

# ggsave(paste0('figures/manualVSdrone_swe/bySeason_ManualVSDrone_', SWE_method, '.png'), width = 4, height = 3)

# calculate 1:1 residuals and RMSE of residuals - does not output to a file
filtered_survey <- survey %>%
  filter(!is.na(.data[[SWE_method]])) %>%
  filter(Season == 'Spring')
filtered_survey$residuals <- filtered_survey[[SWE_method]] - filtered_survey$insitu_SWE
rmse <- sqrt(mean(filtered_survey$residuals^2))


####Statistics calculations####
survey_stats <- survey %>%
  filter(!is.na(Avg_Basinwide))

for (method in swe_methods){
  surv_point_error <- survey_stats %>%
    dplyr::group_by(Site) %>% dplyr::summarise(
      lidar_insitu_Hs_RMSE_points = RMSE(insitu_SWE, .data[[method]]), # RMSE of survey vs lidar snow depth
      lidar_insitu_Hs_Bias_points = bias(insitu_SWE, .data[[method]]), # Bias of survey vs lidar snow depth
      lidar_insitu_Hs_r2_points = r2fun(insitu_SWE, .data[[method]])    # R2 of survey vs lidar snow depth
    )
  
  surv_point_error_season <- survey_stats %>%
    dplyr::group_by(Season, Identifier) %>% dplyr::summarise(
      lidar_insitu_Hs_RMSE_points = RMSE(insitu_SWE, .data[[method]]), # RMSE of survey vs lidar snow depth
      lidar_insitu_Hs_Bias_points = bias(insitu_SWE, .data[[method]]), # Bias of survey vs lidar snow depth
      lidar_insitu_Hs_r2_points = r2fun(insitu_SWE, .data[[method]])    # R2 of survey vs lidar snow depth
    )
  
  surv_point_error_date <- survey_stats %>%
    dplyr::group_by(Identifier) %>% dplyr::summarise(
      lidar_insitu_Hs_RMSE_points=RMSE(insitu_SWE,.data[[method]]), #RMSE of survey vs lidar snow depth
      lidar_insitu_Hs_Bias_points=bias(insitu_SWE,.data[[method]]), #Bias of survey vs lidar snow depth
      lidar_insitu_Hs_r2_points=r2fun(insitu_SWE,.data[[method]]) #R2 of survey vs lidar snow depth
    )
  
  surv_point_error_all <- survey_stats %>%
    dplyr::summarise(
      lidar_insitu_Hs_RMSE_points=RMSE(insitu_SWE,.data[[method]]), #RMSE of survey vs lidar snow depth
      lidar_insitu_Hs_Bias_points=bias(insitu_SWE,.data[[method]]), #Bias of survey vs lidar snow depth
      lidar_insitu_Hs_r2_points=r2fun(insitu_SWE,.data[[method]]) #R2 of survey vs lidar snow depth
    )

  # write.csv(surv_point_error, paste0("outputs/swe/snYr", snow_year, "/", method, "/", 'point_error.csv'))
  # write.csv(surv_point_error_season, paste0("outputs/swe/snYr", snow_year, "/", method, "/", 'point_error_byseason.csv'))
  # write.csv(surv_point_error_date, paste0("outputs/swe/snYr", snow_year, "/", method, "/", 'error_by_date.csv'))
  # write.csv(surv_point_error_all, paste0("outputs/swe/snYr", snow_year, "/", method, "/", 'error_basinwide.csv'))
  #  
}

####export shape file of all survey points to be read into arcgis####
####remove points with lidar Hs as nan - not supported in arcgis ###
survey_point <- survey_stats
survey_point$Avg_basin_dens_bias <- NA
survey_point$Avg_basin_dens_bias <- survey_point$insitu_SWE - survey_point$Avg_Basinwide

points_df <- data.frame(y = survey_point$Latitude, x = survey_point$Longitude, id = survey_point$Identifier, site = survey_point$Site, 
                        Hs_insitu = survey_point$Hs_insitu, SWE_avg_bias = survey_point$Avg_basin_dens_bias)
points_sf <- st_as_sf(points_df, coords = c("x", "y"))
st_crs(points_sf) <- 4326
st_write(points_sf, paste0("outputs/swe/snYr", snow_year, "/survey_points.shp",sep = ""), append=FALSE)

