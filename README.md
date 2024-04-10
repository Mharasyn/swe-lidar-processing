# swe-lidar-processing
R code developed to process aerial lidar point clouds into snow depth maps, and to interpolate snow water equivalent over the area based on in situ density measurements.


Goal

Input .las files of Fortress Basin point clouds and output .tif files of snow depth and SWE.

Input Data
1.	Processed .las files of drone based lidar over Fortress Basin
      F:\PointClouds\Fortress\Intensity
2.	.txt files of GNSS rover points, output from Leica software
      F:\Processing\JULDATE_processing\JULDATE_c\JULDATE\Exported Data
3.	Manual snow depth/density measurements in a .csv, with a colum pointing to GPS point # (coordinating with above .txt file)
      Z:\lidar-processing-basin\data\survey_data

Scripts

00_rover_snow_processing.R

Purpose: Combines GNSS rover information from Leica .txt files with manual snow survey data (.csv) in order to add GPS location to each manual measurement, and filter points by GPS quality. 

Input: manual snow survey data in .csv (all dates combined into one), and Leica .txt files containing GPS locations of snow measurements, located in respective F:\Processing\JULDATE_processing\JULDATE_c\JULDATE\Exported Data folder.

Output: .csv file containing all manual snow survey data and GPS points, located in folder specified in ‘survey_data_out_path’ variable (line 12).

01_LAS_tools_generate_DSM.R

Purpose: Processes .las drone lidar into .tif files using LAStools (LAStools_process.bat). Extent of .tif is clipped to the shape file specified in the ‘shp_name’ variable on line 8. Resolution of output .tif files is specified by the ‘RES_STEP’ variable in line 11 (set to 1 meter on default). All point clouds will be processed in the folder specified as the ‘files’ variable in line 14.

Input: .las files of drone lidar data.

Output folder: data/dsm/

Output: .tif files of surface elevation derived from lidar data.

02_generate_Hs_tifs.R

Purpose: Calculate snow depth from a specified bare ground .tif (variable on line 18) and a list of snow height .tifs. Snow height rasters are iterated through, and the specified bare ground raster is subtracted from snow height. 

Considerations: Due to hardware restrictions (RAM), each year of data has to be processed individually. Therefore, .tifs are divided by year within the \dsm\ folder, and the year to be processed must be specified in line 15. As well, extent of all rasters needs to be consistent for subtraction, therefore each raster is extended (filled with NA) to match the extent of the bare ground flight.

Notes: Phillip Harder recommended that the same bare ground flight be used for all years, so 22_292 has been used consistently for all dates thus far.

Input: List of surface elevation .tifs located in Z:\lidar-processing-basin\data\dsm\YEAR.

Output folder: data/Hs/

Output: List of snow depth .tifs 

03_snow_depth_stats.R

Purpose: Compare lidar-derived snow depth and manual measurements of snow depth. Statistics such as bias and R2 are output.

Input: .csv file from 00_rover_snow_processing.R (line 15) and .tifs generated from 02_generate_Hs_tifs.R.

Output folder: data/error_summary/

Output: Fortress_survey_data.csv containing all survey points and the discrepancy between lidar and manual snow depth, Fortress_point_error.csv containing the overall statistics describing the discrepancy between lidar and manual snow depths, and survey_points.shp containing all point statistics for all survey points for use in a GIS.

04_snow_season_def.R

Purpose: Determines the cutoff date between dry/non-melting snow and wet/melting snow, using snow temperature thermocouple data (20 cm) located at each met station. Definition of snow melt used: mean daily snow temperature being above -0.5C.

Notes: Fortress Ridge does not have thermocouple data, since the snow is always shallow here. Thinking of using lowest altitude air temperature at this site.

Input: .txt files containing thermocouple data (from https://giws1.usask.ca/applications/public.html?publicuser=public#waterdatapublic/stationoverview) and .csv snow survey data

Output folder: data/snowtemp/ and data/survey_data/

Outputs: .txt files containing threshold dates (julian) for each site and year (data/snowtemp/thresholds/) and a new snow survey .csv file containing a 'Season' column

051_swe_regression_basinwide.R

Purpose: Calculates the regression between in situ snow depth and density to all input dates, across all sites. Dates that meet the specified regression R2 cutoff (line 18) will use the regression for SWE calculation, those that do not meet the cutoff will use the average density (05_swe_basin…). Regression is applied to snow depth tifs meeting this criteria.

Notes: If a day has less than 10 snow survey measurements, it is disqualified from the regression analysis and average density will be used. 

Input: .csv file containing manual survey data, snow depth .tifs

Output folder: data/outputs/

Outputs: Density .tifs and SWE .tifs calculated from depth/density regression, and a graph showing regression plots for each site/season.

052_swe_regression_sitewide.R

Purpose: Calculates the regression between in situ snow depth and density to all input dates, for each HRU site. Dates that meet the specified regression R2 cutoff (line 18) will use the regression for SWE calculation, those that do not meet the cutoff will use the average density (05_swe_basin…). Regression is applied to snow depth tifs meeting this criteria.

Input: .csv file containing manual survey data, snow depth .tifs

Output folder: outputs/swe and data/variables/

Outputs: Density .tifs and SWE .tifs calculated from depth/density regression, and a graph showing regression plots for each site/season.

053_swe_avg_density_basinwide.R

Purpose: Calculate SWE using the average densities for the whole basin, corresponding to manual snow depth/density measurements.

Input: List of dates to apply average density SWE to (from 04_swe_basin…) and snow depth .tif files.

Output folder: outputs/swe/

Outputs: SWE .tifs calculated using the average snow density within each land cover type.

054_swe_avg_density_HRU.R

Purpose: Calculate SWE using the average densities within each HRU, corresponding to manual snow depth/density measurements in each land cover type. If a land cover type does not have manual measurements of density, that area is skipped. 

Notes: HRU shape file was manually created by referencing the HRUs designated by the Province of Alberta and an optical imagery base layer. 

Input: List of dates to apply average density SWE to (from 04_swe_basin…), HRU shape file (specified in line 25) and snow depth .tif files.

Output folder: outputs/swe/

Outputs: SWE .tifs calculated using the average snow density within each land cover type.

055_swe_deepsnoweq.R
Purpose: calculate SWE using equations derived by Pomeroy and Gray (1995). There are two equations to calculate SWE for deep snow (>60cm), split by season. This code will apply the appropriate equation to each snow depth map, on grid cells where snow is deeper than 60cm.
