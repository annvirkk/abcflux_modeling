library(stringr)
library(stringi)
library(terra)
library(dplyr)
library(zyp)
library(googleCloudStorageR)
setwd("/home/master/snap")

my_project_id <- "top-operand-328213"
gcs_list_buckets(my_project_id)
gcs_global_bucket("abcflux_modeling_files")

gcs_get_object("testdata.csv", saveToDisk = "mtcars_duplicate.csv")

setwd("")
f <- list.files(, pattern='tmean_')
f <- f[1:756] 

#make sure to focus on 1982-2016

library('terra')
f <- list.files(, pattern='tmean_')
f2 <- f[str_detect(f, "_1.tif")]
f2 <- f2[25:59]# this is for temp
raster <- rast(f2)
rasterdf <-as.data.frame(raster, xy=TRUE)
trend <- zyp.trend.dataframe(rasterdf, metadata.cols=2, method="yuepilon",
                             conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)

trendr <- rast(trend, type="xyz")

# aggregate to vegetation types
v <- rast("X:/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_aggregate_northpolelambert8km_tundraboreal_attfix.tif")

crs(trendr) <- crs(v)
v2 <- crop(v, trendr)

test <- terra::zonal(trendr, v2, fun=mean, na.rm = TRUE) 

# drivers of the trend

# varimp at each pixel, july maps
# temp
f <- list.files(, pattern='tmean_')
f <- f[1:756]
f2 <- f[str_detect(f, "_7.tif")]
f2 <- f2[25:59]# this is for temp

# ndvi
n <- list.files(, pattern='ndvi_')
n <- n[7:426]
n2 <- n[str_detect(f, "_7.tif")]


# flux pred
u <-  list.files("X:/boreal/avirkkala/New folder", pattern='NEE_', recursive=TRUE, full.names=TRUE)
u2 <- u[str_detect(u, "07_1_train_loocv.csv")]
u2 <- u2[!(str_detect(u2, "csv:Zone"))]

test <- read.csv(u2[3:5])
flux <- do.call(rbind,lapply(u2,read.csv))
for (i in 1:length(u2)) assign(u2[i], read.csv(u2[i]))
list2env(
  lapply(setNames(u2, make.names(gsub("*.csv$", "", u2))), 
         read.csv), envir = .GlobalEnv)
rasterdf  <- as.matrix(flux[]) 
r <- rast(rasterdf[], type="xyz")
# plot(r)
crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "

