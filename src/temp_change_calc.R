

library("terra")


# explore change: 2001 vs 2011
r1 <- rast("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_mosaic/visualization/NEE_annual_withoutgbm.tif")
r11 <- rast("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_mosaic/visualization/NEE_gC_m2_1km_rf_2011_sum_loocv.tif")

change <- r11-r1
writeRaster(change, "/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_mosaic/visualization/2011_2001_change.tif", overwrite=TRUE)





# 2001 vs 2011 vs 2018

r18 <- rast("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_mosaic/visualization/NEE_gC_m2_1km_rf_2018_sum_loocv.tif")
r1a <- crop(r1, r18)
change2 <- r18-r1a
writeRaster(change2, "/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_mosaic/visualization/2018_2001_change.tif", overwrite=TRUE)

r1a <- crop(r1, r18)
r11a <- crop(r11, r18)

change2 <- r18-r1a
writeRaster(change2, "/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_mosaic/visualization/2018_2001_change.tif", overwrite=TRUE)

change3 <- r18-r11a
writeRaster(change2, "/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_mosaic/visualization/2018_2011_change.tif", overwrite=TRUE)

st <- c(r1a, r11a, r18)
print(st)

df <- as.data.frame(st, xy=TRUE)
str(df)

df <- na.omit(df)
str(df)

# library("zyp")
# 
# t <- zyp.trend.dataframe(df, 2, method=c("yuepilon"),
#                     conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
# 
# 
# df2 <- pivot_longer(df, c(3:5))
# 
# df2$year <- ifelse()

print("calculating slope")

df$slope <- apply(df[3:5], 1, function(x) coef(lm(x ~ seq(x)))[2])

write.csv(df, "/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_mosaic/visualization/regression_change_2001_2011_2018.csv")