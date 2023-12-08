
# ------------------------------------------------------------------------------

# directories
input_path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/scripts-ex-ante/input-data/'

# ------------------------------------------------------------------------------

# sub-saharan africa 
country <- geodata::world(path=input_path, resolution=5, level=0)
isocodes <- geodata::country_codes()
isocodes_ssa <- subset(isocodes, NAME=='Sudan' | UNREGION1=='Middle Africa' | UNREGION1=='Western Africa' | UNREGION1=='Southern Africa' | UNREGION1=='Eastern Africa')
isocodes_ssa <- subset(isocodes_ssa, NAME!='Cabo Verde' & NAME!='Comoros' & NAME!='Mauritius' & NAME!='Mayotte' & NAME!='Réunion' & NAME!='Saint Helena' & NAME!='São Tomé and Príncipe' & NAME!='Seychelles')
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)
terra::writeVector(ssa, paste0(input_path, 'gadm_ssa.gpkg'), overwrite=T)

# ------------------------------------------------------------------------------ 

# geosurvey
geosurvey <- geodata::cropland(source='QED', path=paste0(input_path))
geosurvey <- terra::crop(x=geosurvey, y=ssa, mask=T)
m <- c(0, 0.1, NA, 0.1, 1, 1)   
rclmat <- matrix(m, ncol=3, byrow=TRUE)
geosurvey <- terra::classify(geosurvey, rclmat)
terra::writeRaster(geosurvey, paste0(input_path, 'geosurvey_processed.tif'), overwrite=T)

# soil properties
props_d <- lapply(c('BLKD', 'pH', 'acid-exch'), function(sv) {
  prop5  <- geodata::soil_af(var=sv, depth=5 , path=paste0(input_path, '/soilgrids'))
  prop15 <- geodata::soil_af(var=sv, depth=15, path=paste0(input_path, '/soilgrids'))
  prop30 <- geodata::soil_af(var=sv, depth=30, path=paste0(input_path, '/soilgrids'))
  prop <- (5 * prop5 + 10 * prop15 + 15 * prop30) / 30 
  props <- terra::crop(x=prop, y=ssa, mask=T)
  props})
props <- terra::rast(props_d)
names(props) <- c('SBD', 'ph', 'hp')
props$SBD <- props$SBD / 1000

# exchangeable bases
bases_d <- lapply(c('K-exch', 'Ca-exch', 'Mg-exch', 'Na-exch'), function(sv) {
	bases <- geodata::soil_af(var=sv, depth=20 , path=paste0(input_path, '/soilgrids'))
	bases <- terra::crop(x=bases, y=ssa, mask=T)
	bases})
bases <- terra::rast(bases_d)
names(bases) <- c('k', 'ca', 'mg', 'na')
bases$bases <- sum(bases) 

# ------------------------------------------------------------------------------

# layers
p <- c(props, bases) 
p$ecec <- p$hp + p$bases
p$hp_sat <- 100 * p$hp / p$ecec
terra::writeRaster(p, paste0(input_path, 'soilgrids_properties_all.tif'), overwrite=T)

# layers cropland
p_cropland <- p * geosurvey
terra::writeRaster(p_cropland, paste0(input_path, 'soilgrids_properties_cropland.tif'), overwrite=T)

# ------------------------------------------------------------------------------
