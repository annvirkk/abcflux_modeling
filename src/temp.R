# mosaicking files in R using terra has memory issues, so using gdal instead: https://stackoverflow.com/questions/15876591/merging-multiple-rasters-in-r
library(gdalUtils)
library(rgdal)
library("stringi")


rasters <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/prep", pattern=".tif$", full.names=TRUE)

# y <- 2001
i <- "NEE_gC_m2"
km <- "1km"
m <- "gbm"

library("stringi")
# annual
r1 <- rasters[stri_detect_fixed(rasters,"annual_sum")]
mosaic_rasters(gdalfile=r1,dst_dataset=paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/", paste(i,"annual_sum",  km, m,   "loocv", sep="_"), ".tif"),of="GTiff")


# ngs
r1 <- rasters[stri_detect_fixed(rasters,"ngs_sum")]
mosaic_rasters(gdalfile=r1,dst_dataset=paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/", paste(i,"ngs_sum",  km, m,  "loocv", sep="_"), ".tif"),of="GTiff")

# gs
r1 <- rasters[stri_detect_fixed(rasters,"_gs_sum")]
mosaic_rasters(gdalfile=r1,dst_dataset=paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/", paste(i,"gs_sum",  km, m,  "loocv", sep="_"), ".tif"),of="GTiff")

  

  
