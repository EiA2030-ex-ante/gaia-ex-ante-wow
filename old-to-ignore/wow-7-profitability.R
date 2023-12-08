
# ------------------------------------------------------------------------------

# directories
input_path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/scripts-ex-ante/input-data/'
output_path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/scripts-ex-ante/output-data/'

# ------------------------------------------------------------------------------

# crop keys 
spam <- c("MAIZ", "SORG", "BEAN", "CHIC", 'LENT', "WHEA", "BARL", "ACOF", "RCOF", 'RICE', 'PMIL', 'SMIL', 'POTA', 'SWPO', 'CASS', 'COWP', 'PIGE', 'SOYB', 'GROU', 'SUGC', 'COTT', 'COCO', 'TEAS', 'TOBA', 'BANA', 'PLNT')
fao <- c('Maize (corn)', "Sorghum", "Beans, dry", "Chick peas, dry", 'Lentils, dry', "Wheat", "Barley", "Coffee, green", "Coffee, green", 'Rice', 'Millet', 'Millet', 'Potatoes', 'Sweet potatoes', 'Cassava, fresh', 'Cow peas, dry', 'Pigeon peas, dry', 'Soya beans', 'Groundnuts, excluding shelled', 'Sugar cane', 'Cotton seed', 'Cocoa beans', 'Tea leaves', 'Unmanufactured tobacco', 'Bananas', 'Plantains')
ac_par <- c(20, 20, 10, 10, 10, 20, 20, 30, 30, 30,20, 20, 30, 30, 30, 10, 10, 10, 10, 30, 30, 30, 30, 30, 30, 30)	 
crops_df <- data.frame(spam=spam, Item=fao, ac_sat=ac_par)
types_df <- read.csv(paste0(input_path, 'crop_types.csv'))

# country keys 
iso3 <- c('AGO', 'BEN', 'BWA', 'BFA', 'BDI', 'CMR', 'CAF', 'TCD', 'COG', 'COD', 'GNQ', 'ERI', 'SWZ', 'ETH', 'GAB', 'GMB', 'GHA', 'GIN', 'GNB', 'CIV', 'KEN', 'LSO', 'LBR', 'MDG', 'MWI', 'MLI', 'MRT', 'MOZ', 'NAM', 'NER', 'NGA', 'RWA', 'SEN', 'SLE', 'SOM', 'SSD', 'SDN', 'SWZ', 'TZA', 'TGO', 'UGA', 'ZAF', 'ZMB', 'ZWE')
name <- c('Angola', 'Benin', 'Botswana', 'Burkina Faso', 'Burundi', 'Cameroon', 'Central African Republic', 'Chad', 'Congo', 'DRC', 'Equatorial Guinea', 'Eritrea', 'Swaziland', 'Ethiopia', 'Gabon', 'Gambia', 'Ghana', 'Guinea', 'Guinea-Bissau', 'Côte d\'Ivoire', 'Kenya', 'Lesotho', 'Liberia', 'Madagascar', 'Malawi', 'Mali', 'Mauritania', 'Mozambique', 'Namibia', 'Niger', 'Nigeria', 'Rwanda', 'Senegal', 'Sierra Leone', 'Somalia', 'Sudan (former)', 'Sudan (former)', 'Swaziland', 'United Republic of Tanzania', 'Togo', 'Uganda', 'South Africa', 'Zambia', 'Zimbabwe')
country <- data.frame(iso3=iso3, Area=name)

# ------------------------------------------------------------------------------

# yield loss to hp
resp_hp <- terra::rast(Sys.glob(paste0(input_path, 'ecocrop/hp_crop_suitability_*_0.tif')))
names(resp_hp) <- gsub("\\_0.tif$", "", basename(terra::sources(resp_hp)))
names(resp_hp) <- gsub("hp_crop_suitability_", "", names(resp_hp))
resp_hp <- terra::aggregate(resp_hp, 10, fun='mean', na.rm=T)

# yield loss to ph 
resp_ph <- terra::rast(Sys.glob(paste0(input_path, 'ecocrop/ph_crop_suitability_*_0.tif')))
names(resp_ph) <- gsub("\\_0.tif$", "", basename(terra::sources(resp_ph)))
names(resp_ph) <- gsub("ph_extra_production_", "", names(resp_ph))
resp_ph <- terra::aggregate(resp_ph, 10, fun='mean', na.rm=T)

# crop yield
yield_all <- terra::rast(paste0(input_path, "spam_yield_processed.tif")) / 1000
yield_all <- terra::resample(yield_all, resp_hp)

# crop prices
fao_price <- read.csv(paste0(input_path, 'FAOSTAT_data_en_4-24-2023.csv'))
fao_price <- subset(fao_price, Year > 2015 & Year <= 2020)
fao_price <- subset(fao_price, Item %in% fao)
fao_price <- subset(fao_price, Area %in% name)
fao_price <- merge(fao_price, crops_df, by='Item', all.y=T)
fao_price <- merge(fao_price, country, by='Area', all.x=T)
fao_price <- fao_price[c(1, 2, 10, 12, 13, 14, 17, 18, 19)]
boxplot(fao_price$Value~fao_price$spam)
write.csv(fao_price, paste0(input_path, 'crop-prices.csv'))
crop_price <- aggregate(fao_price$Value, by=list('crop'=fao_price$spam), FUN=median, na.rm=T) # median price

# lime rate
kamprath <- terra::rast(paste0(input_path, 'caco3_kamprath.tif'))
kamprath <- terra::aggregate(kamprath, 10, mean, na.rm=T)
cochrane <- terra::rast(paste0(input_path, 'caco3_cochrane.tif'))
cochrane <- terra::aggregate(cochrane, 10, mean, na.rm=T)
merlos <- terra::rast(paste0(input_path, 'caco3_merlos.tif'))
merlos <- terra::aggregate(merlos, 10, mean, na.rm=T)

# lime price
lime_price = 100  # USD/t

# ------------------------------------------------------------------------------

# economic returns
residual_effect = c(1, 0.5, 0.25, 0)
epv_dir <- paste0(input_path, "economics/")
for(c in unique(spam)){
  print(c)
  c_subset <- subset(crops_df, spam == c)
  price_ust <- subset(crop_price, crop_price$crop == c)
  actual_yield <- yield_all[c(c),]
  # select response model
  for(r in c('hp', 'ph')){
    print(r)
    response = r
    if(response == 'hp'){
      yield_loss <- resp_hp[c(c),]
    } else{
      yield_loss <- resp_ph[c(c),]
      }
    # select yield level
    for(actual_yield_f in seq(1, 2, 0.25)){
      # select crop price
      for(crop_price_f in seq(0, 2, 0.25)){
        yield_level <- actual_yield * actual_yield_f
        yield_resp_tha <- (yield_level / yield_loss) - yield_level 
        yield_resp_tha <- terra::ifel(yield_resp_tha == 0, NA, yield_resp_tha)
        if(crop_price_f == 0){
          terra::writeRaster(yield_resp_tha, filename=paste0(epv_dir, response, '_', c, '_ayf_', actual_yield_f, '_yieldresp_tha.tif'), overwrite=T)
        }
        return_usha <- yield_resp_tha * price_ust$x * crop_price_f
        names(return_usha) <- c
        return_usha_resid <- return_usha * residual_effect[1] + return_usha * residual_effect[2] + return_usha * residual_effect[3] + return_usha * residual_effect[4]
        terra::writeRaster(return_usha, filename=paste0(epv_dir, response, '_', c, '_ayf_', actual_yield_f, '_cpf_', crop_price_f, '_returns_usha.tif'), overwrite=T)
        terra::writeRaster(return_usha_resid, filename=paste0(epv_dir, response, '_', c, '_ayf_', actual_yield_f, '_cpf_', crop_price_f, '_returns_usha_resid.tif'), overwrite=T)
      }
    }
  }
}

# ------------------------------------------------------------------------------

# lime costs
epv_dir<- paste0(input_path, "economics/")
for(c in unique(spam)){
  print(c)
  c_subset <- subset(crops_df, spam == c)
  for(m in c('merlos', 'cochrane')){
    print(m)
    method = m
    if(method == 'merlos'){
      lime_tha <- merlos[paste0('merlos_', c_subset$ac_sat)]
      names(lime_tha) <- c
      for(lime_price_f in seq(0, 1, 0.2)){
        lime_usha <- lime_tha * lime_price * lime_price_f
        names(lime_usha) <- c
        terra::writeRaster(lime_tha, filename=paste0(epv_dir, 'merlos_', c, '_lpf_', lime_price_f, '_lr_merlos_tha.tif'), overwrite=T)
        terra::writeRaster(lime_usha, filename=paste0(epv_dir, 'merlos_', c, '_lpf_', lime_price_f, '_lime_usha.tif'), overwrite=T)
      }
    } else{
      lime_tha <- cochrane[paste0('cochrane_', c_subset$ac_sat)]
      names(lime_tha) <- c
      for(lime_price_f in seq(0, 1, 0.2)){
        lime_usha <- lime_tha * lime_price * lime_price_f
        names(lime_usha) <- c
        terra::writeRaster(lime_tha, filename=paste0(epv_dir, 'cochrane_', c, '_lpf_', lime_price_f, '_lr_cochrane_tha.tif'), overwrite=T)
        terra::writeRaster(lime_usha, filename=paste0(epv_dir, 'cochrane_', c, '_lpf_', lime_price_f, '_lime_usha.tif'), overwrite=T)
      }
    }
  }
}

# ------------------------------------------------------------------------------

# area
area_ha <- terra::rast(Sys.glob(paste0(input_path, "spam_harv_area_processed.tif")))
'%!in%' <- function(x,y)!('%in%'(x,y))
area_ha <- area_ha[[names(area_ha) %!in% c('BANA', 'PLNT', 'RICE')]]
area_ha <- terra::resample(area_ha, merlos)

# lime rate kamprath
lime_tha <- terra::rast(paste0(input_path, 'caco3_kamprath.tif'))
lime_tha <- terra::aggregate(lime_tha, 10, mean, na.rm=T)
cereals <- subset(types_df, type=='Cereal')
cereal_ha  <- area_ha[[names(area_ha) %in% unique(cereals$crop)]]
lime_t_cereals <- cereal_ha * lime_tha
legumes <- subset(types_df, type=='Legume')
legume_ha  <- area_ha[[names(area_ha) %in% unique(legumes$crop)]]
lime_t_legumes <- legume_ha * lime_tha
lime_t <- c(lime_t_cereals, lime_t_legumes)
lime_t <- sum(lime_t)
terra::global(lime_t, sum, na.rm = TRUE)/1000000

# lime rate cochrane
lime_tha <- terra::rast(Sys.glob(paste0(input_path, 'economics/cochrane_*_lpf_1_lr_cochrane_tha.tif')))
lime_tha <- lime_tha[[names(lime_tha) %!in% c('BANA', 'PLNT', 'RICE')]]

# lime rate merlos
lime_tha <- terra::rast(Sys.glob(paste0(input_path, 'economics/merlos_*_lpf_1_lr_merlos_tha.tif')))
'%!in%' <- function(x,y)!('%in%'(x,y))
lime_tha <- lime_tha[[names(lime_tha) %!in% c('BANA', 'PLNT', 'RICE')]]

# lime total merlos
lime_t <- lime_tha * area_ha
lime_t <- sum(lime_t, na.rm=T)
terra::global(lime_t, sum, na.rm=T)/1000000 # all

# ------------------------------------------------------------------------------

# economic layers
returns_usha <- terra::rast(Sys.glob(paste0(input_path, 'economics/hp_*_ayf_1_cpf_1_returns_usha.tif')))
returns_resid_usha <- terra::rast(Sys.glob(paste0(input_path, 'economics/hp_*_ayf_1_cpf_1_returns_usha_resid.tif')))
costs_usha <- terra::rast(Sys.glob(paste0(input_path, 'economics/merlos_*_lpf_1_lime_usha.tif')))

# gross margin
gm <- returns_usha - costs_usha
gm_resid <- returns_resid_usha - costs_usha
terra::writeRaster(gm, filename=paste0(input_path, 'baseline_hp_grossmargin_usha.tif'), overwrite=T)
terra::writeRaster(gm_resid, filename=paste0(input_path, 'baseline_hp_grossmargin_resid_usha.tif'), overwrite=T)

# roi
roi <- returns_usha / costs_usha
roi_resid <- returns_resid_usha / costs_usha
terra::writeRaster(roi, filename=paste0(input_path, 'baseline_hp_roi_usdusd.tif'), overwrite=T)
terra::writeRaster(roi_resid, filename=paste0(input_path, 'baseline_hp_roi_resid_usdusd.tif'), overwrite=T)

# ------------------------------------------------------------------------------
