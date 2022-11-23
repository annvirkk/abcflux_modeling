


# download terraclimate dataset
library("terra")
r <- rast("/home/master/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_aggregate_northpolelambert8km_tundraboreal_attfix.tif")


# extract coordinates
r2 <-  xyFromCell(r, 1:ncell(r))


r3 <- data.frame(cbind(r2, r2[,1]))
lon <- rast(r3, type="xyz", crs=crs(r))

r3 <- data.frame(cbind(r2, r2[,2]))
lat <- rast(r3, type="xyz", crs=crs(r))

lat <- mask(lat, r) # this is probably better for the biases!!
lon <- mask(lon, r) # this separates between NA and Eurasia

plot(lat)
plot(lon)


writeRaster(lat, "/home/master/predictors_8km/latitude.tif")

writeRaster(lon, "/home/master/predictors_8km/longitude.tif")





# Terra settings
terraOptions(memfrac=0.9, tempdir = "/home/master/temp/") 
options(digits=20) # this is important, otherwise we will lose some digits with the cropped extents


library(googleCloudStorageR)
# Google cloud settings
my_project_id <- "top-operand-328213"
gcs_list_buckets(my_project_id)
gcs_global_bucket("abcflux_modeling_files")
contents <- gcs_list_objects()
gcs_upload_set_limit(50000000L) # increasing data size limit for transferring data to google cloud





filename_in="/home/master/predictors_8km/latitude.tif"
filename_out="gs://abcflux_modeling_files/predictors_8km/latitude.tif"
system(paste("gsutil cp", filename_in, filename_out, sep=" ")) # unix commands. use gsutils
file.remove(filename_in)



filename_in="/home/master/predictors_8km/longitude.tif"
filename_out="gs://abcflux_modeling_files/predictors_8km/longitude.tif"
system(paste("gsutil cp", filename_in, filename_out, sep=" ")) # unix commands. use gsutils
file.remove(filename_in)
