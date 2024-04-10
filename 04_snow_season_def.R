# Determine the winter/spring dry/wet snow cutoff for each year based on snow thermocouple data 
# Melting snow is defined as the daily mean snow temp (at 20 cm) being above -0.5C (approved by John, 10 April 2024)
# Created by Maddie Harasyn 2024 (madison.harasyn@usask.ca)
# For now, Fortress Ridge is removed (no thermocouple data) and added back at end of code before data export (line 110)

# load libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)

# variables ###########################################################
# temperature to define 'melting' snowpack (will search for defined time period where snow is above this temp)
temp_thresh <- -0.5 #(in celcius)
search_timeframe <- 24 #(in hours)

#cutoff date to define the start of the new snow year
new_year_date <- "-09-01 MDT" #September 1st, year will get added in the code

#location of field data 
survey <- read.csv('Z:/lidar-processing-basin/data/survey_data/survey_points_2019_2023.csv')%>%  
  filter(Site != 'Fortress Ridge')%>% 
  filter(Site != 'Snow Experiment')%>% #filter out sites we do not have thermocouple data for
  mutate(Date=as.POSIXct(Date, "%Y-%m-%d", tz="Etc/GMT+6"))%>% ##If survey data isnt being read in properly, try changing the date format %m/%d/%Y
  filter()

#location of snow thermocouple data used for dividing snow by 'season'
snowT_files <- list.files(path = "Z:/lidar-processing-basin/data/snowtemp/", pattern = ".txt$", ignore.case = TRUE, full.names = TRUE, recursive = FALSE)
snowT_list <- list()

for (path in snowT_files){
  snowT_file <- read.table(path, sep = "\t")%>%
    mutate(V1=as.POSIXct(V1, "%Y-%m-%dT%H:%M:%S", tz="Etc/GMT+6"), #converts to local time
           year = format(V1, '%Y'), 
           month = format(V1, '%m'),
           date = format(V1, '%j')) %>% 
    filter(month >= "03") %>% #remove Jan and Feb to avoid days where snow melted early
    filter(year >= "2021") %>% #remove years before 2021 <- when drone data starts
    group_by(year, date)%>% #group thermocouple data by year

    
    ## Search for first day where daily avg temperature is above temp threshold (temp_thresh)
    summarize(mean_V2 = mean(V2)) %>%
    filter(mean_V2 >= temp_thresh) %>%
    group_by(year) %>%
    
    
    ## These lines are used if defining a 'search' time period to search for when temp is above a threshold (0C)
    # mutate(above_thresh = V2 > temp_thresh,  # Create a logical column indicating if temperature is above 0
    #        count_above_thresh = ave(above_thresh, cumsum(!above_thresh), FUN = cumsum)) %>%
    # filter(count_above_thresh >= (search_timeframe * 4)) %>% #multiply search_timeframe by 4, data is in 15 min incraments
    
    slice(1)  # Take the first instance
  
  #rename sites in snow temp tibbles to match name convention in survey csv table
  if (substr(path, start = 41, stop = 43) == 'Bon'){ 
    site_name <- 'Bonsai'
  } else if (substr(path, start = 41, stop = 43) == 'CRN'){
    site_name <- 'Canadian Ridge'
  } else if (substr(path, start = 41, stop = 43) == 'FRS'){
    site_name <- 'Fortress Ridge South'
  } else if (substr(path, start = 41, stop = 43) == 'Pow'){
    site_name <- 'Powerline'
  }

  out_path <- paste0("Z:/lidar-processing-basin/data/snowtemp/thresholds/", site_name, '_MeltThreshold.txt')
  write.table(snowT_file, out_path)
  
  snowT_list[[site_name]] <- snowT_file
}

#add column in survey data to classify as 'winter' or 'spring'
survey$Season<-NA

#group survey data by year to sort winter/spring
survey_bySite <- survey%>%
  mutate(ID = Site)%>%
  group_by(Site)%>%
  nest()

survey_season <- data.frame()

for (site in survey_bySite[[1]]){ #iterate through each site, apply site specific temp threshold
  site_data <- survey_bySite[[2]][[which(survey_bySite$Site == site)]]
  site_byYear <- site_data%>%
    group_by(year = year(Date)) %>%
    nest()
  
  tempT_data <- snowT_list[[site]]
  
  for (year in site_byYear[[1]]){ #iterate through each year to apply threshold for each year at each site
    year_survey <- site_byYear[[2]][[which(site_byYear$year == year)]]
    
    for (i in 1:nrow(year_survey)){ #if row date before threshold OR after new_year_date (Sept), then classified as 'Winter'
      if((format(year_survey$Date[i], '%j') < tempT_data[[2]][[which(tempT_data$year == year)]]) | year_survey$Date[i] > paste0(year, new_year_date)) {
          year_survey$Season[i] <- 'Winter'}
      else{
        year_survey$Season[i] <- 'Spring'}
    }
    survey_season <- rbind(survey_season, year_survey)
  }
}

#change table headers to match swe code + add back in fortress ridge
colnames(survey_season)[which(names(survey_season) == "ID")] <- "Site"
survey_FR <- read.csv('Z:/lidar-processing-basin/data/survey_data/survey_points_2019_2023.csv')%>% 
  mutate(Date=as.POSIXct(Date, "%m/%d/%Y", tz="Etc/GMT+6"))%>%
  filter(Site == 'Fortress Ridge')%>% 
  mutate(Season = NA)

survey_season <- rbind(survey_season, survey_FR)

#export survey table to pull into SWE codes with 'Season' column
write.csv(survey_season, 'data/survey_data/survey_points_seasonSorted.csv')
