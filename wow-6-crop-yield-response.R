
# ------------------------------------------------------------------------------

# directories
input_path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/scripts-ex-ante/input-data/'
output_path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/scripts-ex-ante/output-data/'

# ------------------------------------------------------------------------------

# yield-spam
crop_yield <- terra::rast(paste0(input_path, "spam_yield_processed.tif")) / 1000

# yield-response
resp <- c('hp', 'ph')
x <- lapply(resp, function(response){
  loss <- terra::rast(Sys.glob(paste0(input_path, 'ecocrop_f/', response, '_crop_suitability_*_0.tif')))
  names(loss) <- gsub('_..$', "", names(loss))
  loss_agg <- terra::aggregate(loss, 10, fun='mean', na.rm=T)
  resp_tha <- (crop_yield / loss_agg) - crop_yield 
  terra::writeRaster(resp_tha, paste0(input_path, '/extra_production_', response,'.tif'), overwrite=T)
})

# ------------------------------------------------------------------------------
