library(terra)
library(limer)

# toy example of raster with different soils exch_ac, ecec, and SBD values
sprops_maintenance <- c(
                        # soils:   a  b  c  d  e  f
  rast(name = "exch_ac", vals = c(.2,.4, 4, 8,16,16), ncol = 2, nrow = 3),
  rast(name = "ECEC",    vals = c( 1, 1,10,10,20,20), ncol = 2, nrow = 3),
  rast(name = "SBD",     vals = c( 1, 1, 1, 1, 1,10), ncol = 2, nrow = 3)
)

# constants
decay = 0.25
acidification = 0
tas = 10

# Joao's code (I only changed maint_lime$ecec for maint_lime$ECEC)
caco3_merlos_maintenance <- lapply(tas, function(t){
  
  maint_lime <- sprops_maintenance  # raster with all soil properties to run litas
  
  exal <- (maint_lime$ECEC * t) / 100 # corresponding ex_ac for a given tas
  
  maint_lime$exch_ac <- min(maint_lime$exch_ac, exal + decay + acidification) # acidity saturation of the soil prior to liming 
  
  merlos <- limer::limeRate(maint_lime[[1:6]], method='my', check_Ca=F, unit='t/ha', SD=20, TAS=t)
  
  names(merlos) <- paste0('merlos_', t)
  
  merlos})

values(caco3_merlos_maintenance[[1]])
