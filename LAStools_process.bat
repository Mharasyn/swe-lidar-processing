:: a batch script for converting photogrammetry points into a
:: number of products with a tile-based multi-core batch pipeline
:: include LAStools in PATH to allow running script from anywhere
@echo off
set PATH=%PATH%;C:\LAStools\bin
set LAStools=C:\LAStools\bin

set list= 24_176_Fortress
set working_path= Z:\lidar-processing-basin
set shp_name=fortress_extent
set STEP=1

lastile -version

:: lastools variables
set n_cores=3
:: drop pts below/above this elev (for full basin)
set z_min=1960 
set z_max=2460 
:: use a [n]x[n]x[n] uniform grid for finding isolated points  
set rm_noise_step=3 
:: points are isolated when there is a total of less than [n] points in all neighbour cells  
:: AC had n_pts_isolate = 10
set n_pts_isolated=5
set ground_offset=0.1 
:: sensitivity analysis by AC 2024-03-12
:: AC had ground_step = 2, ground_thin_step=0.05, ground_spike=0.1, 
:: ultra_fine - removed from lasground_new 
set ground_step=3 
set ground_spike=0.5
set ground_thin_step=0.2
set ground_thin_perc=50 
set tile_size=50
set buffer=5


echo entering lidar processing for-loop through the following LAS files %list%.
pause

FOR %%A IN (%list%) DO (
rmdir 0_clip /s /q
mkdir 0_clip
lasclip64 -i F:\PointClouds\Fortress\BasinFlights\Unprocessed\%%A.las ^
         -drop_z_above %z_max% ^
         -drop_z_below %z_min% ^
         -poly %working_path%\data\shp\%shp_name%.shp ^
         -o 0_clip\%%A_clip.las -v
			
rmdir 0_opt /s /q
mkdir 0_opt

lasoptimize64 -i 0_clip\%%A_clip.las ^
         -o 0_opt\%%A_opt.las -olas

rmdir 0_clip /s /q
:: create temp1orary tile directory
rmdir 1_tiles /s /q
mkdir 1_tiles

:: create a temp1orary tiling with %tile_size% and %buffer%
lastile64 -i 0_opt\%%A_opt.las ^
         -set_classification 0 ^
         -tile_size %tile_size% ^
		 -buffer %buffer% ^
		 -flag_as_withheld ^
         -o 1_tiles\tile.las
		 
:: create temp1orary lasnoise directory
rmdir 0_opt /s /q
rmdir 2_tiles_denoised /s /q
mkdir 2_tiles_denoised


lasnoise64 -i 1_tiles\*.las ^
         -step %rm_noise_step% ^
		 -isolated %n_pts_isolated% ^
         -classify_as 31 ^
         -odir 2_tiles_denoised ^
		 -olas ^
         -remove_noise ^
         -cores %n_cores%

rmdir 1_tiles /s /q
:: create temp1orary lassort directory
rmdir 3_tiles_sorted /s /q
mkdir 3_tiles_sorted


lassort64 -i 2_tiles_denoised\*.las ^
         -odir 3_tiles_sorted -olas ^
         -cores %n_cores%

rmdir 2_tiles_denoised /s /q			
rmdir 4_tiles_ground /s /q
mkdir 4_tiles_ground


lasground_new -i 3_tiles_sorted\tile*.las ^
         -step %ground_step% ^
         -offset %ground_offset% ^
         -spike %ground_spike% ^
         -spike_down %ground_spike% ^
         -ground_class 2 ^
         -odir 4_tiles_ground -olas ^
         -cores %n_cores%

rmdir 3_tiles_sorted /s /q
rmdir 5_tiles_ground_thin /s /q
mkdir 5_tiles_ground_thin	


lasthin64 -i 4_tiles_ground\tile*.las ^
         -keep_class 2 ^
         -step %ground_thin_step% ^
         -percentile %ground_thin_perc% ^
         -odir 5_tiles_ground_thin -olas ^
         -cores %n_cores%  

rmdir 4_tiles_ground /s /q

rmdir 6_class /s /q
mkdir 6_class

echo starting lasmerge64 on file: %%A.

lasmerge64 -i 5_tiles_ground_thin\tile*.las ^
         -drop_withheld ^
		 -keep_class 2 ^
         -o 6_class\%%A_class.las -olas 

rmdir 5_tiles_ground_thin /s /q

blast2dem -i 6_class\%%A_class.las ^
         -step %STEP% ^
         -keep_class 2 ^
		 -utm 11U ^
		 -o %working_path%\data\dsm\%%A.tif 
		  
rmdir 6_class /s /q

pause
)

