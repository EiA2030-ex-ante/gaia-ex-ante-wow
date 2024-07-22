
# ------------------------------------------------------------------------------

# directories
input_path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/scripts-ex-ante/input-data/'
output_path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/scripts-ex-ante/output-data/'

# ------------------------------------------------------------------------------

# keys 
fao <- data.frame(spam = c("MAIZ", "SORG", "BEAN", "CHIC", 'LENT', "WHEA", "BARL", "ACOF", "RCOF", 'PMIL', 'SMIL', 'POTA', 'SWPO', 'CASS', 'COWP', 'PIGE', 'SOYB', 'GROU', 'SUGC', 'COTT', 'COCO', 'TEAS', 'TOBA'), 
                  Item = c('Maize (corn)', "Sorghum", "Beans, dry", "Chick peas, dry", 'Lentils, dry', "Wheat", "Barley", "Coffee, green", "Coffee, green", 'Millet', 'Millet', 'Potatoes', 'Sweet potatoes', 'Cassava, fresh', 'Cow peas, dry', 'Pigeon peas, dry', 'Soya beans', 'Groundnuts, excluding shelled', 'Sugar cane', 'Cotton seed', 'Cocoa beans', 'Tea leaves', 'Unmanufactured tobacco'))
crops_df <- read.csv(paste0(input_path, 'ecocrop_parameters_hp.csv'))[-c(1)]
ctype_df <- read.csv(paste0(input_path, 'crop_types.csv'))[-c(1)]
crops_df <- merge(crops_df, ctype_df, by.x='spam', by.y='crop')
crops_df <- merge(crops_df, fao, by='spam')
country <- data.frame(iso3 = c('AGO', 'BEN', 'BWA', 'BFA', 'BDI', 'CMR', 'CAF', 'TCD', 'COG', 'COD', 'GNQ', 'ERI', 'SWZ', 'ETH', 'GAB', 'GMB', 'GHA', 'GIN', 'GNB', 'CIV', 'KEN', 'LSO', 'LBR', 'MDG', 'MWI', 'MLI', 'MRT', 'MOZ', 'NAM', 'NER', 'NGA', 'RWA', 'SEN', 'SLE', 'SOM', 'SSD', 'SDN', 'SWZ', 'TZA', 'TGO', 'UGA', 'ZAF', 'ZMB', 'ZWE'), 
                      Area =  c('Angola', 'Benin', 'Botswana', 'Burkina Faso', 'Burundi', 'Cameroon', 'Central African Republic', 'Chad', 'Congo', 'DRC', 'Equatorial Guinea', 'Eritrea', 'Swaziland', 'Ethiopia', 'Gabon', 'Gambia', 'Ghana', 'Guinea', 'Guinea-Bissau', 'Côte d\'Ivoire', 'Kenya', 'Lesotho', 'Liberia', 'Madagascar', 'Malawi', 'Mali', 'Mauritania', 'Mozambique', 'Namibia', 'Niger', 'Nigeria', 'Rwanda', 'Senegal', 'Sierra Leone', 'Somalia', 'Sudan (former)', 'Sudan (former)', 'Swaziland', 'United Republic of Tanzania', 'Togo', 'Uganda', 'South Africa', 'Zambia', 'Zimbabwe'))

# ------------------------------------------------------------------------------

# loss
resp_hp <- terra::rast(Sys.glob(paste0(input_path, 'ecocrop_f/hp_crop_suitability_*_0.tif'))) 
names(resp_hp) <- gsub("\\_0.tif$", "", basename(terra::sources(resp_hp)))
names(resp_hp) <- gsub("hp_crop_suitability_", "", names(resp_hp))
resp_hp <- terra::aggregate(resp_hp, 10, fun='mean', na.rm=T)
resp_ph <- terra::rast(Sys.glob(paste0(input_path, 'ecocrop_f/ph_crop_suitability_*_0.tif')))
names(resp_ph) <- gsub("\\_0.tif$", "", basename(terra::sources(resp_ph)))
names(resp_ph) <- gsub("ph_extra_production_", "", names(resp_ph))
resp_ph <- terra::aggregate(resp_ph, 10, fun='mean', na.rm=T)

# yield
crop_yield <- terra::rast(paste0(input_path, "spam_yield_processed.tif")) / 1000
crop_area <- terra::rast(paste0(input_path, "spam_harv_area_processed.tif"))
names(crop_area) <- paste0(names(crop_area), '_ha')

# price
fao_price <- read.csv(paste0(input_path, 'FAOSTAT_data_en_4-24-2023.csv'))
fao_price <- subset(fao_price, Year > 2015 & Year <= 2020)
fao_price <- subset(fao_price, Item %in% crops_df$Item & Area %in% country$Area)
fao_price <- merge(fao_price, crops_df, by='Item', all.y=T)
fao_price <- merge(fao_price, country, by='Area', all.x=T)
fao_price <- fao_price[c(1, 2, 10, 12, 13, 14, 17, 18, 19)]
write.csv(fao_price, paste0(input_path, 'crop-prices.csv'))
crop_price <- aggregate(fao_price$Value, by=list('crop'=fao_price$spam), FUN=median, na.rm=T) # median price

# lime per tas
merlos <- terra::aggregate(terra::rast(paste0(input_path, 'caco3_merlos.tif')), 10, mean, na.rm=T)
merlos_m <- terra::aggregate(terra::rast(paste0(input_path, 'caco3_merlos_maintenance.tif')), 10, mean, na.rm=T)

# lime per crop
soil <- terra::aggregate(terra::rast(paste0(input_path, 'soilgrids_properties_all.tif')), 10, 'mean', na.rm=T)
lr <- function(crop, lime_method) {
  c_subset <- crops_df[crops_df$spam == crop,]
  lime_tha <- lime_method[[grep(paste0("_", c_subset$ac_sat), names(lime_method))]]
  area <- crop_area[[paste0(crop, '_ha')]]
  area <- terra::ifel(area > 0, 1, NA)
  soil_hp <- terra::ifel(soil$hp_sat > c_subset$ac_sat, 1, NA)
  lime_tha <- lime_tha * soil_hp * area
  names(lime_tha) <- crop
  return(lime_tha)}
lr_crops <- terra::rast(lapply(crops_df$spam, function(crp){lr(crop=crp, lime_method=merlos)}))
lr_m_crops <- terra::rast(lapply(crops_df$spam, function(crp){lr(crop=crp, lime_method=merlos_m)}))

# ------------------------------------------------------------------------------

# returns
returns <- function(crop, yield_resp, crop_price, yield_f){
  c_subset <- crops_df[crops_df$spam == crop,]
  actual_yield <- crop_yield[c(crop),]
  names(actual_yield) <- paste0(crop, '_ya')
  yield_loss <- yield_resp[[crop]]
  names(yield_loss) <- paste0(crop, '_loss')
  yield_level <- actual_yield * yield_f
  yield_resp_tha <- (yield_level / yield_loss) - yield_level 
  yield_resp_tha <- terra::subst(yield_resp_tha, 0, NA)
  names(yield_resp_tha) <- paste0(crop, '_yresp_tha')
  return_usha <- yield_resp_tha * crop_price
  names(return_usha) <- paste0(crop, '_return_usha')
  return(c(actual_yield, yield_loss, yield_resp_tha, return_usha))
  }

# costs (year 1)
costs <- function(crop, lime_method, lime_price) {
  # c_subset <- subset(crops_df, spam == crop) 
  # lime_tha <- lime_method[[grep(paste0("_", c_subset$ac_sat), names(lime_method))]]
  lime_tha <- lime_method[[crop]]
  names(lime_tha) <- paste0(crop, '_lr_tha')
  lime_usha <- lime_tha * lime_price
  names(lime_usha) <- paste0(crop, '_cost_usha')
  return(lime_usha)
  }

# number of years
nyears_f <- function(crop, lime_year1, lime_maint){
  c_subset <- subset(crops_df, spam == crop) 
  # lime_1 <- lime_year1[[grep(paste0("_", c_subset$ac_sat), names(lime_year1))]]
  # lime_m <- lime_maint[[grep(paste0("_", c_subset$ac_sat), names(lime_maint))]]
  lime_1 <- lime_year1[[crop]]
  lime_m <- lime_maint[[crop]]
  nyears <- round(lime_1/lime_m, 0); names(nyears) <- 'nyears'
  return(nyears)
  }

# profitability
profit <- function(crop, yield_resp, yf, crop_price, returns_f=c('year1', 'npv', 'equilibrium'), lime_method, lime_m_method, lime_price, nyrs){
  if(returns_f == 'year1'){                                         
    # year 1 return & year 1 cost
    return <- returns(crop, yield_resp, yield_f=yf, crop_price)  
    cost <- costs(crop, lime_method, lime_price)           
  } else if(returns_f == 'npv'){                                    
    # npv return & year 1 cost
    return_1 <- returns(crop, yield_resp, yield_f=yf, crop_price) # output not as per returns() function
    return <- limer::NPV_lime(return_1[[paste0(crop, "_return_usha")]], nyears=nyrs, discount_rate=10)   
    names(return) <- paste0(crop, "_return_usha")
    cost <- costs(crop, lime_method, lime_price)           
  } else if(returns_f == 'equilibrium'){                             
    # year 1 return & maintenance cost
    return <- returns(crop, yield_resp, yield_f=yf, crop_price)
    cost <- costs(crop, lime_m_method, lime_price)           
  }
  gm <- return[[paste0(crop, "_return_usha")]] - cost; names(gm) <- paste0(crop, '_gm_usha')
  roi <- return[[paste0(crop, "_return_usha")]] / cost; names(roi) <- paste0(crop, '_roi_usha')
  return(c(return, cost, gm, roi))
  }

# ------------------------------------------------------------------------------

# sensitivity to prices
prices <- expand.grid(c_price=seq(0.1, 2, 0.1), l_price=seq(0, 2, 0.1))
output <- data.frame(rel_price=rep(NA, nrow(crops_df)*nrow(prices)), crop=NA, gm_sum=NA, ha_sum=NA)
for(r_f in c('year1', 'npv', 'equilibrium')){
  print(r_f)
  j <- 1
  for(crop in unique(crops_df$spam)){
    print(crop)
    area_ha <- crop_area[[paste0(crop, '_ha')]]
    price_ust <- crop_price[crop_price$crop==crop,]$x
    for(i in 1:nrow(prices)){
      print(i)
      p <- prices[i,]    
      # economics
      c_price <- price_ust * p$c_price
      l_price <- 100 * p$l_price
      c_nyrs <- nyears_f(crop, lr_crops, lr_m_crops)
      if(r_f == 'npv'){                                         
        crop1 <- profit(returns_f=r_f, crop, yield_resp=resp_hp, yf=1, crop_price=c_price, lime_method=lr_crops, nyrs=c_nyrs, lime_price=l_price) 
      } else{
        crop1 <- profit(returns_f=r_f, crop, yield_resp=resp_hp, yf=1, crop_price=c_price, lime_method=lr_crops, lime_m_method=lr_m_crops, lime_price=l_price) 
      }
      crop2 <- c(area_ha, crop1)
      # summary gm
      gm <- crop2[[c(grep('_ha', names(crop2)), grep('_gm_usha', names(crop2)))]]
      gm[[2]] <- terra::ifel(gm[[2]] <= 0, NA, gm[[2]])
      gm_sum <- terra::global(gm[[2]], sum, na.rm=T)$sum
      # summary Ha
      gm_mask <- terra::ifel(gm[[2]] <= 0, NA, 1) 
      ha <- gm[[1]] * gm_mask
      ha_sum <- terra::global(ha, sum, na.rm=T)$sum
      # output
      output[j,] <- cbind(rel_price=i, crop=crop, gm_sum=round(gm_sum,2), ha_sum=round(ha_sum,2))
      j <- j + 1
      }
    }
  write.csv(output, paste0(output_path, '/output-sensitivity-analysis-prices-', r_f, '.csv'))
  }

# ------------------------------------------------------------------------------

# sensitivity to yields
yld <- seq(1, 2.5, 0.1)
output <- data.frame(yield_f=rep(NA, length(yld)), crop=NA, gm_sum=NA, ha_sum=NA)
for(r_f in c('year1', 'npv', 'equilibrium')){
  print(r_f)
  j <- 1
  for(crop in unique(crops_df$spam)){
    print(crop)
    area_ha <- crop_area[[paste0(crop, '_ha')]]
    price_ust <- crop_price[crop_price$crop==crop,]$x
    for(i in unique(yld)){
      print(i)
      # economics
      c_price <- price_ust
      l_price <- 100
      c_nyrs <- nyears_f(crop, lr_crops, lr_m_crops)
      if(r_f == 'npv'){                                         
        crop1 <- profit(returns_f=r_f, crop, yield_resp=resp_hp, yf=i, crop_price=c_price, lime_method=lr_crops, nyrs=c_nyrs, lime_price=l_price) 
      } else{
        crop1 <- profit(returns_f=r_f, crop, yield_resp=resp_hp, yf=i, crop_price=c_price, lime_method=lr_crops, lime_m_method=lr_m_crops, lime_price=l_price) 
      }
      crop2 <- c(area_ha, crop1)
      # summary gm
      gm <- crop2[[c(grep('_ha', names(crop2)), grep('_gm_usha', names(crop2)))]]
      gm[[2]] <- terra::ifel(gm[[2]] <= 0, NA, gm[[2]]) 
      gm_sum <- terra::global(gm[[2]], sum, na.rm=T)$sum
      # summary area
      gm_mask <- terra::ifel(gm[[2]] <= 0, NA, 1) 
      ha <- gm[[1]] * gm_mask
      ha_sum <- terra::global(ha, sum, na.rm=T)$sum
      # output
      output[j,] <- cbind(yield_f=i, crop=crop, gm_sum=round(gm_sum,2), ha_sum=round(ha_sum,2))
      j <- j + 1
      }
    }
  write.csv(output, paste0(output_path, '/output-sensitivity-analysis-yields-', r_f, '.csv'))
  }

# ------------------------------------------------------------------------------
