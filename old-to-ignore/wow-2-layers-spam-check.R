
# ------------------------------------------------------------------------------

# directories
input_path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/scripts-ex-ante/input-data/'

# ------------------------------------------------------------------------------

# regions
ssa <- terra::vect(paste0(input_path, 'gadm_ssa.gpkg'))

# crops
spam <- c('RICE', 'MAIZ', 'SORG', 'BEAN', 'CHIC', 'LENT', 'WHEA', 'BARL', 'ACOF', 'RCOF', 'PMIL', 'SMIL', 'POTA', 
          'SWPO', 'CASS', 'COWP', 'PIGE', 'SOYB', 'GROU', 'SUGC', 'COTT', 'COCO', 'TEAS', 'TOBA')	

# 2010
spam2010 <- lapply(spam, function(crop){geodata::crop_spam(crop, 'harv_area', path=input_path)[[1]]})
spam2010 <- terra::rast(spam2010)
names(spam2010) <- gsub("_harv_area_all", "", names(spam2010))
spam2010 <- spam2010[[names(spam2010) %in% spam]]
spam2010 <- spam2010[[sort(names(spam2010))]]
spam2010 <- terra::crop(spam2010, ssa, mask=T)
spam2010 <- terra::subst(spam2010, 0, NA)

# 2017
spam2017 <- lapply(spam, function(crop){geodata::crop_spam(crop, 'harv_area', africa=T, path=input_path)[[1]]})
spam2017 <- terra::rast(spam2017)
names(spam2017) <- gsub("_harv_area_all", "", names(spam2017))
spam2017 <- spam2017[[names(spam2017) %in% spam]]
spam2017 <- spam2017[[sort(names(spam2017))]]
spam2017 <- terra::crop(spam2017, ssa, mask=T)
spam2017 <- terra::subst(spam2017, 0, NA)

# 2020
spam2020 <- terra::rast(Sys.glob(paste0(input_path, 'spam-2020/spam2020V0r1_global_harvested_area/spam2020_v0r1_global_H_*_A.tif')))
names(spam2020) <- gsub("\\_A.tif$", "", basename(terra::sources(spam2020)))
names(spam2020) <- gsub("spam2020_v0r1_global_H_", "", names(spam2020))
names(spam2020) <- ifelse(names(spam2020) == 'COFF', 'ACOF', names(spam2020))
names(spam2020) <- ifelse(names(spam2020) == 'MILL', 'SMIL', names(spam2020))
spam2020 <- spam2020[[names(spam2020) %in% spam]]
spam2020 <- spam2020[[sort(names(spam2020))]]
spam2020 <- terra::crop(spam2020, ssa, mask=T)
spam2020 <- terra::crop(spam2020, spam2017)
spam2020 <- terra::subst(spam2020, 0, NA)

# plot
brk = c(0,10,500,50000)
cols = rainbow(5, start=0.1, end=0.9)
par(ask=T, mfrow=c(1,3))
for (crop in spam) {
  print(crop) 
  terra::plot(spam2010[[crop]], col=cols, breaks=brk); terra::lines(ssa)
  text(-10, -25, crop)
  text(-10, -28, "2010")
  terra::plot(spam2017[[crop]], col=cols, breaks=brk); terra::lines(ssa)
  text(-10, -25, crop)
  text(-10, -28, "2017")
  terra::plot(spam2020[[crop]], col=cols, breaks=brk); terra::lines(ssa)
  text(-10, -25, crop)
  text(-10, -28, "2020")}

# ------------------------------------------------------------------------------
