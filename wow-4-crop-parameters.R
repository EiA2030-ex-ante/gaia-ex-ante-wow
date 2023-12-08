
# ------------------------------------------------------------------------------

# directories
input_path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/scripts-ex-ante/input-data/'
output_path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/scripts-ex-ante/output-data/'

# ------------------------------------------------------------------------------

# crop types
crop_types <- data.frame(crop=c("MAIZ", "SORG", "BEAN", "CHIC", 'LENT', "WHEA", "BARL", "ACOF", "RCOF", 'PMIL', 'SMIL', 'POTA', 'SWPO', 'CASS', 'COWP', 'PIGE', 'SOYB', 'GROU', 'SUGC', 'COTT', 'COCO', 'TEAS', 'TOBA'), 
                         type=c('Cereal', "Cereal", "Legume", "Legume", 'Legume', "Cereal", "Cereal", "Commodity", "Commodity", 'Cereal', 'Cereal', 'RTBs', 'RTBs', 'RTBs', 'Legume', 'Legume', 'Legume', 'Legume', 'Commodity', 'Commodity', 'Commodity', 'Commodity', 'Commodity'))
write.csv(crop_types, paste0(input_path, 'crop_types.csv'))

# ------------------------------------------------------------------------------

# figure
png(paste0(output_path, "crop-parameters-1.png"), units="in", width=11.3, height=5.5, res=1000)
par(mfrow=c(1,2), mar=c(4.5,5,1,1), xaxs='i', yaxs='i', las=1, cex.main=1.5, cex.lab=1.4, cex.axis=1.3)

# acidity
crops_df <- readxl::read_excel(paste0(input_path, '# ecocrop_parameters_hp_final.xlsx'))
crops_df <- crops_df[c(1,2,9,10)]
names(crops_df)[3:4] <- c('ac_sat', 'max_ac_sat')
write.csv(crops_df, paste0(input_path, 'ecocrop_parameters_hp.csv'))
crops_df <- merge(crops_df, crop_types, by.x='spam', by.y='crop')
crops_df <- unique(crops_df[c(3:5)])
crops_df$crop <- c('coffee', 'maize', 'bean', 'cassava', 'sugarcane', 'cowpea', 'potato', 'sweet potato')
plot(NULL, xlim=c(0,100), ylim=c(0,1.05), xlab='Acidity saturation (% ECEC)', ylab='Relative yield (-)', main='')
grid(nx=10, ny=10)
j <- 1
txt <- c()
col <- c()
for(tp in unique(crops_df$crop)){
  col_crop <- viridis::viridis(nrow(crops_df)*2)[j]
  croptype <- subset(crops_df, crop == tp)
  croptype <- reshape2::melt(croptype, id.vars=c('type', 'crop'))
  croptype$yield <- ifelse(croptype$variable == 'ac_sat', 1, 0)
  lines(x=c(0, croptype$value), y=c(1, croptype$yield), lwd=2.5, col=col_crop)
  j <- j + 2
  txt <- c(txt, tp)
  col <- c(col, col_crop)}
legend('topright', cex=0.9, legend=txt, lty=1, col=col, lwd=2.5)

# pH
crops_df <- data.frame(spam=c("MAIZ", "SORG", "BEAN", "CHIC", 'LENT', "WHEA", "BARL", "ACOF", "RCOF", 'PMIL', 'SMIL', 'POTA', 'SWPO', 'CASS', 'COWP', 'PIGE', 'SOYB', 'GROU', 'SUGC', 'COTT', 'COCO', 'TEAS', 'TOBA'), 
                       ecocrop=c('Maize', "Sorghum (med. altitude)", "Bean, Common", "Chick pea", 'Lentil', "Wheat, common", "Barley", "Coffee arabica", "Coffee robusta", 'Pearl millet', 'Finger millet', 'Potato', 'Sweet potato', 'Cassava', 'Cowpea', 'Pigeon Pea', 'Soyabean', 'Groundnut', 'Saccharum officinarum L.', 'Cotton, American upland', 'Cacao', 'Tea', 'Tobacco'), 
                       min_ph=c(4.5, 4.5, 5.0, 5.0, 5.0, 4.5, 4.5, 4.0, 4.0, 4.5, 4.5, 4.0, 4.0, 4.0, 5.0, 5.0, 5.0, 5.0, 4.0, 4.5, 4.0, 4.0, 4.5), 
                       max_ph=5.5)
write.csv(crops_df, paste0(input_path, 'ecocrop_parameters_ph.csv'))
crops_df <- merge(crops_df, crop_types, by.x='spam', by.y='crop')
crops_df <- unique(crops_df[c(3:5)])
crops_df$crop1 <- c('coffee', 'maize', 'bean', 'cassava', 'tobacco')
crops_df$crop <- c('coffee+cassava', 'maize+tobacco', 'bean', NA, NA)
crops_df <- na.omit(crops_df[-c(4)])
plot(NULL, xlim=c(3.5,6), ylim=c(0,1.05), xlab='Soil pH in water', ylab='Relative yield (-)', main='')
grid(nx=10, ny=10)
j <- 1
txt <- c()
col <- c()
for(tp in unique(crops_df$crop)){
  col_crop <- viridis::viridis(nrow(crops_df)*2)[j]
  croptype <- subset(crops_df, crop == tp)
  croptype <- reshape2::melt(croptype, id.vars=c('type', 'crop'))
  croptype$yield <- ifelse(croptype$variable == 'max_ph', 1, 0)
  lines(x=c(0, croptype$value, 7), y=c(0, croptype$yield, 1), lwd=2.5, col=col_crop)
  j <- j + 2
  txt <- c(txt, tp)
  col <- c(col, col_crop)}
legend('topleft', cex=0.9, legend=txt, lty=1, col=col, lwd=2.5)

dev.off()

# ------------------------------------------------------------------------------
