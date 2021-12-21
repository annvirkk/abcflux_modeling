

#install.packages('terra', repos='https://rspatial.r-universe.dev', lib="/mnt/data1/boreal/avirkkala/packages")
library("terra", lib.loc="/mnt/data1/boreal/avirkkala/packages")

setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_1km") ### REMEMBER TO CHANGE THIS!!!
Percent_Tree_Cover_MOD44B_sites <- rast(paste0("Percent_Tree_Cover_mod44b2001", ".tif")) # THE FIRST LAYER!!!!
print(Percent_Tree_Cover_MOD44B_sites)
Percent_Tree_Cover_MOD44B_sites <- Percent_Tree_Cover_MOD44B_sites[[1]]



esa <- rast("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_northpolelambert1km_tundraboreal_attfix.tif")
print(esa)


library("raster")
esa <- raster("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_northpolelambert1km_tundraboreal_attfix.tif")
print(esa)
esa2 <- as(esa)
print(esa2)