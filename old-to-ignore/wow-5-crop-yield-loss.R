
# ------------------------------------------------------------------------------

# directories
input_path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/scripts-ex-ante/input-data/'
output_path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/scripts-ex-ante/output-data/'

# ------------------------------------------------------------------------------

# soil-grids
sprops_cropland <- terra::rast(paste0(input_path, 'soilgrids_properties_cropland.tif'))
sprops_cropland <- sprops_cropland[[c(2, 10)]]
names(sprops_cropland) <- c("ph", "ac_sat")

# crops
crops_df <- read.csv(paste0(input_path, 'ecocrop_parameters_hp.csv'))

# ------------------------------------------------------------------------------

# run
for(crop in unique(crops_df$spam)){
  print(crop)
    # define response
    for(response in c('hp', 'ph')){
      print(response)
      if(response == 'hp'){
        for(parameter in seq(0, 30, 10)){
          # define crop 
          crops_df <- read.csv(paste0(input_path, 'ecocrop_parameters_hp.csv'))
          subset_crops <- subset(crops_df, spam == crop)
          subset_crops$max_ac_sat <- subset_crops$max_ac_sat - parameter
          print(subset_crops$max_ac_sat)
          ecocrop_crop <- Recocrop::ecocropPars(subset_crops$ecocrop)
          ecocrop_crop$parameters <- cbind(ecocrop_crop$parameters, ac_sat=c(-Inf, -Inf, subset_crops$ac_sat, subset_crops$max_ac_sat))
          # model for hp
          m_hp <- Recocrop::ecocrop(ecocrop_crop)
          v_hp <- terra::values(sprops_cropland[['ac_sat']])
          Recocrop::staticPredictors(m_hp) <- v_hp
          Recocrop::control(m_hp, get_max = T)
          y_hp <- Recocrop::run(m_hp)
          r_hp <- terra::rast(sprops_cropland[['ac_sat']])
          r_hp <- terra::setValues(r_hp, y_hp)
          r_hp <- terra::ifel(r_hp == 0, NA, r_hp)
          names(r_hp) <- 'ecocrop_hp'  
          terra::writeRaster(r_hp, paste0(input_path, '/ecocrop/hp_crop_suitability_', crop, '_', parameter, '.tif'), overwrite=T)
        }
      } else{
        # select parameters
        for(parameter in seq(0, 0.3, 0.1)){
          # define crop 
          crops_df <- read.csv(paste0(input_path, 'ecocrop_parameters_ph.csv'))
          subset_crops <- subset(crops_df, spam == crop)
          subset_crops$min_ph <- subset_crops$min_ph + parameter
          print(subset_crops$min_ph)
          ecocrop_crop <- Recocrop::ecocropPars(subset_crops$ecocrop)
          ecocrop_crop$parameters <- cbind(ecocrop_crop$parameters, ph=c(subset_crops$min_ph, subset_crops$max_ph, Inf, Inf))
          m_ph <- Recocrop::ecocrop(ecocrop_crop)
          v_ph <- terra::values(sprops_cropland[['ph']])
          Recocrop::staticPredictors(m_ph) <- v_ph
          Recocrop::control(m_ph, get_max = T)
          y_ph <- Recocrop::run(m_ph)
          r_ph <- terra::rast(sprops_cropland[['ph']])
          r_ph <- terra::setValues(r_ph, y_ph)
          r_ph <- terra::ifel(r_ph == 0, NA, r_ph)
          names(r_ph) <- 'ecocrop_ph' 
          terra::writeRaster(r_ph, paste0(input_path, '/ecocrop/ph_crop_suitability_', crop, '_', parameter, '.tif'), overwrite=T)
        }
      }
    }
  }
  
# ------------------------------------------------------------------------------
