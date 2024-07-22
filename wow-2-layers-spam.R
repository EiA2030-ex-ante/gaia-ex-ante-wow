
# ------------------------------------------------------------------------------

# directories
input_path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/1-ex-ante-analysis/input-data/'

# ------------------------------------------------------------------------------

# regions
ssa <- terra::vect(paste0(input_path, 'gadm_ssa.gpkg'))

# soils (to correct extent)
ref <- terra::rast(paste0(input_path, 'soilgrids_properties_all.tif'))[[1]]
ref <- terra::aggregate(ref, 10, 'mean', na.rm=T)

# crops
spam <- c('MAIZ', 'SORG', 'BEAN', 'CHIC', 'LENT', 'WHEA', 'BARL', 'ACOF', 'RCOF', 'PMIL', 'SMIL', 'POTA', 
          'SWPO', 'CASS', 'COWP', 'PIGE', 'SOYB', 'GROU', 'SUGC', 'COTT', 'COCO', 'TEAS', 'TOBA')	

# spam
for(sv in c('harv_area', 'prod', 'yield')){   # val_prod
  print(sv)
  variable <- lapply(spam, function(crop){geodata::crop_spam(crop, sv, path=input_path)[[1]]})   # africa=T,
  variable <- terra::rast(variable)
  names(variable) <- spam
  variable <- terra::crop(variable, ssa, mask=T)
  variable <- terra::resample(variable, ref)
  terra::writeRaster(variable, paste0(input_path, 'spam_', sv, '_processed.tif'), overwrite=T)
  }

# ------------------------------------------------------------------------------
