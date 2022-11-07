#0.1

files <- list.files("/home/master/local_outputs/predictions_8km/csv/0.1", pattern="1991")
files <-files[grepl("NEE", files) & grepl("larval", files)]
setwd("/home/master/local_outputs/predictions_8km/csv/0.1")


s1 <- rast()
for (f in files) {
  
  # f <- "NEE_gC_m2_20km_qrf_1991_01_train_loocv_full_model_without_larvaloutbreak.csv"
  dd <- read.csv(f)
  
  pred_rast_static_m  <- as.matrix(dd[]) 
  r <- rast(pred_rast_static_m[], type="xyz")/1000
  plot(r)
  
  s1 <- c(s1, r)
  
}



#0.9

files <- list.files("/home/master/local_outputs/predictions_8km/csv/0.9", pattern="1991")
files <-files[grepl("NEE", files) & grepl("larval", files)]
setwd("/home/master/local_outputs/predictions_8km/csv/0.9")


s9 <- rast()
for (f in files) {
  
  # f <- "NEE_gC_m2_20km_qrf_1991_01_train_loocv_full_model_without_larvaloutbreak.csv"
  dd <- read.csv(f)
  
  pred_rast_static_m  <- as.matrix(dd[]) 
  r <- rast(pred_rast_static_m[], type="xyz")/1000
  plot(r)
  
  s9 <- c(s9, r)
  
}



ranges <- rast()
for (i in 1:12) {
  
  r <- c(s1[[i]], s9[[i]])
  r2 <- r[[1]] - r[[2]]
  
  ranges <- c(ranges, r2)
  
}

ranges_mean <- mean(ranges)

writeRaster(ranges_mean, "/home/master/temp/ranges_mean.tif")





# add mean tif

nee <- rast("/home/master/local_outputs/predictions_8km/raster/0.5/NEE_gC_m2_1992_sum.tif")/1000
plot(nee)

nee_mean <- nee/12
plot(nee_mean)

crs(ranges_mean) <- crs(nee)
rel_range <- ranges_mean/nee_mean
plot(rel_range)
