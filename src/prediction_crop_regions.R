library("raster")

## REMEMBER TO CHANGE THIS FROM OLD
esa <- raster("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_northpolelambert1km_tundraboreal_attfix.tif")


# first crop to a smaller domain
esa <- raster::crop(esa, raster::extent(-5e+06, 5e+06, -4e+06, 4e+06))

r <- SplitRas(raster=esa,ppside=5,save=TRUE,plot=FALSE)

# get extents
for (i in 1:25) {
  
  print(i)
  # i <- 1
  
  if (i==1) {
    crops <- cbind(extent(r[[i]])@xmin, extent(r[[i]])@xmax, extent(r[[i]])@ymin, extent(r[[i]])@ymax)
  } else {
    crops2 <- cbind(extent(r[[i]])@xmin, extent(r[[i]])@xmax, extent(r[[i]])@ymin, extent(r[[i]])@ymax)
    crops <- rbind(crops, crops2)
  }
}


write.csv(crops, "/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/prediction_crop_regions.csv", row.names=FALSE)
