# combining GNSS rover txt files over all survey days
rm(list = ls())

library(dplyr)

# variables #######################################################

# UNCOMMENT PROPER DATASET/OUTPUT LOCATION TO USE

# paths for bare ground survey data
snow_survey_path <- 'data/survey_data/2019_2023_Drone_Survey_points.csv'
survey_data_out_path <- 'data/survey_data/survey_points_2019_2023.csv'

#will use all files in Point Cloud folder to determine which days to process
surv_days <- list.files("F:/PointClouds/Fortress/Intensity") %>% 
  substr(start = 0, 6)

#factor to convert in situ snow depth to meters (lidar snow depth unit)
Hs_conv_fact = 0.01

# functions #######################################################
angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

# leicas terminology for good gnss solution .. 
fixed_solution <- 'GNSSPhaseMeasuredRTK' #filter out GNSSCodeMeasuredRTK

bad_gnss_ids <- c('W')

high_lim_gnss_id <- 9999

surv_dirs <- paste0("F:/Processing/20", substr(surv_days, start = 0, 2),"/", surv_days, "_processing/", surv_days, "_c/", surv_days, "/Exported Data/", surv_days, ".txt")

no_data_days <- c(
  # no rov pts on this day as was bare ground
  "F:/Processing/2022/22_292_processing/22_292_c/22_292/Exported Data/22_292.txt", 
  # no rov pts on this day
  "F:/Processing/2021/21_230_processing/21_230_c/21_230/Exported Data/21_230.txt")

surv_dirs <- surv_dirs[!surv_dirs %in% no_data_days]

rover_pts_list <- lapply(surv_dirs, read.delim, header = F)

rover_pts_df <- do.call(rbind, rover_pts_list) %>% 
  dplyr::select(c(1:5, 10)) %>% 
  dplyr::rename(GPS_ID = V1,
         point_type = V2,
         lat_dms = V3,
         lon_dms = V4,
         ele_m = V5,
         datetime = V10) %>%
  dplyr::filter(point_type == fixed_solution,
         !GPS_ID %in% bad_gnss_ids) %>% 
  dplyr::mutate(GPS_ID = gsub('GS', x = GPS_ID, ''),
                GPS_ID = floor(as.numeric(GPS_ID)),
         datetime = as.POSIXct(datetime, format = '%m/%d/%Y %H:%M:%OS'),
         yy_ddd = paste0(format(datetime, "%y"), "_", format(datetime, "%j"))) %>% 
  # need to remove bad pt ids 
  dplyr::filter(GPS_ID < high_lim_gnss_id)

snow_data <- read.csv(snow_survey_path) %>%
  mutate(Date = as.POSIXct(Date, format = '%m/%d/%Y')) %>% 
  dplyr::select(Date,
        Site,
        GPS_ID,
        Point,
        Depth,
        Tare,
        SWEpretare,
        Density) %>% 
  dplyr::filter(is.na(GPS_ID) != T) %>% 
  dplyr::mutate(
    yy_ddd = paste0(format(Date, "%y"), "_", format(Date, "%j"))
  )

options(max.print=999999)

survey_data <- dplyr::left_join(rover_pts_df, 
                         snow_data, 
                         by = c("yy_ddd", "GPS_ID"),
                         multiple = "all")

survey_data$lat_dd = angle2dec(survey_data$lat_dms)
survey_data$lon_dd = angle2dec(survey_data$lon_dms)*-1

survey_data_out <- survey_data %>% 
  dplyr::filter(is.na(Depth) == F) %>% 
  dplyr::select(Site,
    Identifier = yy_ddd,
    Date,
    GPS_ID,
    Latitude = lat_dd,
    Longitude = lon_dd,
    Point,
    Hs_insitu = Depth,
    Tare,
    Density
  ) %>% 
  mutate(Hs_insitu = floor(as.numeric(Hs_insitu)),
         Hs_insitu = Hs_insitu * Hs_conv_fact)


write.csv(survey_data_out, survey_data_out_path, row.names = F) 

