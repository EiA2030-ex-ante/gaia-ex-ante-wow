
# ------------------------------------------------------------------------------

# directories
input_path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/scripts-ex-ante/input-data/'
output_path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/scripts-ex-ante/output-data/'

# ------------------------------------------------------------------------------

# soil-grids
sprops_cropland <- terra::rast(paste0(input_path, 'soilgrids_properties_cropland.tif'))
sprops_cropland <- sprops_cropland[[c(3,4,5,6,7,1)]]
names(sprops_cropland) <- c('exch_ac', 'exch_k', 'exch_Ca', 'exch_mg', 'exch_na', 'SBD')  

# acidity saturation
hp_sat <- terra::rast(paste0(input_path, 'soilgrids_properties_cropland.tif'))
hp_sat <- hp_sat[[c(10)]]
hp_sat_acid <- terra::classify(hp_sat, rcl=cbind(-1, 10, 0))
hp_sat_acid <- terra::ifel(hp_sat_acid != 0, 1, hp_sat_acid)

# ------------------------------------------------------------------------------

# kamprath: year 1
caco3_kamprath <- limer::limeRate(sprops_cropland, method='ka', check_Ca=F, unit='t/ha', SD=20) 
caco3_kamprath_filter <- caco3_kamprath * hp_sat_acid # for soils with exch acidity saturation > 10% only
terra::writeRaster(caco3_kamprath_filter, paste0(input_path, 'caco3_kamprath.tif'), overwrite=T)

# ------------------------------------------------------------------------------

# cochrane: year 1
tas <- c(0, 5, 10, 15, 20, 25, 30, 35, 40)
caco3_cochrane <- lapply(tas, function(t){
  cochrane <- limer::limeRate(sprops_cropland, method='co', check_Ca=F, unit='t/ha', SD=20, TAS=t)
  names(cochrane) <- paste0('cochrane_', t)
  cochrane})
caco3_cochrane <- terra::rast(caco3_cochrane) 
terra::writeRaster(caco3_cochrane, paste0(input_path, 'caco3_cochrane.tif'), overwrite=T)

# ------------------------------------------------------------------------------

# aramburu-merlos: year 1
tas <- c(0, 5, 10, 15, 20, 25, 30, 35, 40)
caco3_merlos <- lapply(tas, function(t){
  merlos <- limer::limeRate(sprops_cropland, method='my', check_Ca=F, unit='t/ha', SD=20, TAS=t)
  names(merlos) <- paste0('merlos_', t)
  merlos})
caco3_merlos <- terra::rast(caco3_merlos) 
terra::writeRaster(caco3_merlos, paste0(input_path, 'caco3_merlos.tif'), overwrite=T)

# ------------------------------------------------------------------------------

# aramburu-merlos: maintenance
sprops_maintenance <- terra::rast(paste0(input_path, 'soilgrids_properties_cropland.tif'))
sprops_maintenance <- sprops_maintenance[[c(3,4,5,6,7,1,9)]]
names(sprops_maintenance) <- c('exch_ac', 'exch_k', 'exch_Ca', 'exch_mg', 'exch_na', 'SBD', 'ecec')  
acidification=0; decay=0.22; tas <- c(0, 5, 10, 15, 20, 25, 30, 35, 40)
caco3_merlos_maintenance <- lapply(tas, function(t){
  maint_lime <- sprops_maintenance
  exal <- (maint_lime$ecec * t) / 100 # corresponding ex_ac for a given tas
  maint_lime$exch_ac <- min(maint_lime$exch_ac, exal + decay + acidification)
  merlos <- limer::limeRate(maint_lime[[1:6]], method='my', check_Ca=F, unit='t/ha', SD=20, TAS=t)
  names(merlos) <- paste0('merlos_', t)
  merlos})
caco3_merlos_maintenance <- terra::rast(caco3_merlos_maintenance) 
terra::writeRaster(caco3_merlos_maintenance, paste0(input_path, 'caco3_merlos_maintenance.tif'), overwrite=T)

# ------------------------------------------------------------------------------
