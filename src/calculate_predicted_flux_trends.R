

.libPaths("/home/master/R/x86_64-pc-linux-gnu-library/4.2")

#install.packages("terra", dependencies=TRUE)
library(terra)
#install.packages("zyp", dependencies=TRUE)
library(Kendall)

library(zyp)
#install.packages("dplyr", dependencies=TRUE)
library(dplyr)
#install.packages("stringi", dependencies=TRUE)
library(stringi)
#install.packages("stringr", dependencies=TRUE)
library(stringr)



### Focus on 1990-2020 for now


### Flux trends

# function to go through months, years, seasons
setwd("/home/master/predictions_8km/raster/0.5") # path

terraOptions(memfrac=0.9, tempdir = "/home/master/temp/")
trends <- function(flux) {

  months <- c("_01", "_02", "_03", "_04", "_05", "_06",
              "_07", "_08", "_09", "_10", "_11", "_12")

  files <- list.files("/home/master/predictions_8km/raster/0.5", pattern=flux)
  files <- files[!str_detect(files, "198")]
  files2 <- files[nchar(files)<25 & nchar(files)>20]
## TEMMPORARILY!!!
# 
#   for (m in months) {
# 
#     files3 <- files2[str_detect(files2, m)]
#     print(files3)
# 
#     r <- rast(files3)
#     df <- as.data.frame(r, xy=TRUE)
# 
#     trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
#                                  conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
# 
#     trendr <- rast(trend, type="xyz")
# 
#     writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", flux,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
# 
# 
#     print(paste(m, "done"))
# 
# 
#   }
# 
#   file.remove(list.files("/home/master/temp/", full.names=TRUE))
# 
# 
#   # seasons: spring #
#   m <- c("_03_04_05")
# 
# 
#   files3 <- files[str_detect(files, m)]
#   print(files3)
# 
# 
#   r <- rast(files3)
#   df <- as.data.frame(r, xy=TRUE)
# 
#   trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
#                                conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
# 
#   trendr <- rast(trend, type="xyz")
# 
#   writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", flux,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
# 
# 
# 
#   # seasons: summer #
#   m <- c("_06_07_08")
# 
# 
#   files3 <- files[str_detect(files, m)]
#   print(files3)
# 
# 
#   r <- rast(files3)
#   df <- as.data.frame(r, xy=TRUE)
# 
#   trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
#                                conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
# 
#   trendr <- rast(trend, type="xyz")
# 
#   writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", flux,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
# 
# 
#   # seasons: autumn #
#   m <- c("_09_10_11")
# 
# 
#   files3 <- files[str_detect(files, m)]
#   print(files3)
# 
# 
#   r <- rast(files3)
#   df <- as.data.frame(r, xy=TRUE)
# 
#   trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
#                                conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
# 
#   trendr <- rast(trend, type="xyz")
# 
#   writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", flux,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
# 
# 
#   # seasons: winter #
#   m <- c("_01_02_12")
# 
# 
#   files3 <- files[str_detect(files, m)]
#   print(files3)
# 
# 
#   r <- rast(files3)
#   df <- as.data.frame(r, xy=TRUE)
# 
#   trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
#                                conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
# 
#   trendr <- rast(trend, type="xyz")
# 
#   writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", flux,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
# 
# 
# 
#   # seasons: ngs #
#   m <- c("_01_02_03_04_10_11_12")
# 
# 
#   files3 <- files[str_detect(files, m)]
#   print(files3)
# 
# 
#   r <- rast(files3)
#   df <- as.data.frame(r, xy=TRUE)
# 
#   trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
#                                conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
# 
#   trendr <- rast(trend, type="xyz")
# 
#   writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", flux,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
# 
#   file.remove(list.files("/home/master/temp/", full.names=TRUE))
# 
# 
#   # seasons: gs #
#   m <- c("_05_06_07_08_09")
# 
# 
#   files3 <- files[str_detect(files, m)]
#   print(files3)
# 
# 
#   r <- rast(files3)
#   df <- as.data.frame(r, xy=TRUE)
# 
#   trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
#                                conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
# 
#   trendr <- rast(trend, type="xyz")
# 
#   writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", flux,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")


  # annual #


  files3 <- files[nchar(files)==22]
  print(files3)


  r <- rast(files3)
  df <- as.data.frame(r, xy=TRUE)

  trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)

  trendr <- rast(trend, type="xyz")

  writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", flux,  "_annual","_trend", ".tif"), overwrite=TRUE, datatype="INT4S")


  rm(trendr);rm(trend);rm(r)
  gc()

  file.remove(list.files("/home/master/temp/", full.names=TRUE))



}


trends("NEE_gC_m2")
#trends("Reco_gC_m2")








############# uncommented because done???


# ### calculate pixel-wise environmental trends using zyp package 
# setwd("/home/master/cloud/predictors_8km")
# years <- seq(1990, 2016, 1) %>% as.character()
# vector.is.empty <- function(x) return(length(x) ==0 )
# trends_env <- function(env) {
#   
#   print(env)
#   
#   months <- c("_01", "_02", "_03", "_04", "_05", "_06",
#               "_07", "_08", "_09", "_10", "_11", "_12")
#   
#   files <- list.files("/home/master/cloud/predictors_8km", pattern=env)
#   files <- files[!str_detect(files, "198")] 
#   files <- files[!str_detect(files, "2021")] 
#   files <- files[!str_detect(files, "2022")] 
#   files <- files[!str_detect(files, "trend")] 
#   files <- files[!str_detect(files, "average")] 
#   files <- files[!str_detect(files, "2017")] 
#   files <- files[!str_detect(files, "2018")] 
#   files <- files[!str_detect(files, "2019")] 
#   files <- files[!str_detect(files, "2020")] 
#   files <- files[endsWith(files, '*.tif$')]
#   
#   # # TEMPORARY
#   # files <- files[str_detect(files, "199")] 
#   
#   
#   print(files)
#   
#   
#   for (m in months) {
#     
#     # add point after m - this needs to be done because _1. somehow is not seen by the program
#     m_orig <- m
#     m <- paste(m, ".tif", sep="")
#     # if _01 -> _1
#     m2 <- sub("_0+", "_", m)
#     
#    
#     files3 <- files[str_detect(files, m) | str_detect(files, m2)] 
#     print(files3)
#     
# 
#     r <- rast(files3)
#     df <- as.data.frame(r, xy=TRUE)
#     
#     trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
#                                  conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
#     
#     trendr <- rast(trend, type="xyz")
#     
#     writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", env,  m_orig, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
#     
#     
#     print(paste(m, "done"))
#     
#     
#   }
#   
#   
#   # seasons: spring #
#   m <- c("_03_04_05")
#   
#   files3 <- files[str_detect(files, "_03") | str_detect(files, "_04") | str_detect(files, "_05") | str_detect(files, "_3") | str_detect(files, "_4") | str_detect(files, "_5")] 
#   print(files3)
#   
#   
#   rs <- rast()
#   
#   for (y in years) {
#     
#     print(y)
#     files4 <- files3[str_detect(files3, y) ]
#     r <- rast(files4)
#     r2 <- mean(r)
#     rs <- c(rs, r2)
#     
#   }
# 
#   df <- as.data.frame(rs, xy=TRUE)
#   
#   trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
#                                conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
#   
#   trendr <- rast(trend, type="xyz")
#   
#   writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", env,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
#   
#   
#   # seasons: summer #
#   m <- c("_06_07_08")
#   
#   files3 <- files[str_detect(files, "_06") | str_detect(files, "_07") | str_detect(files, "_08") | str_detect(files, "_6") | str_detect(files, "_7") | str_detect(files, "_8")] 
#   print(files3)
#   
#   
#   rs <- rast()
#   
#   for (y in years) {
#     
#     print(y)
#     files4 <- files3[str_detect(files3, y) ]
#     r <- rast(files4)
#     r2 <- mean(r)
#     rs <- c(rs, r2)
#     
#   }
#   
#   df <- as.data.frame(rs, xy=TRUE)
#   
#   trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
#                                conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
#   
#   trendr <- rast(trend, type="xyz")
#   
#   writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", env,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
#   
#   print("spring and summer done")
#   file.remove(list.files("/home/master/temp/", full.names=TRUE))
#   
#   # seasons: autumn #
#   m <- c("_09_10_11")
#   
#   files3 <- files[str_detect(files, "_09") | str_detect(files, "_10") | str_detect(files, "_11") | str_detect(files, "_9") | str_detect(files, "_10") | str_detect(files, "_11")] 
#   print(files3)
#   
#   
#   rs <- rast()
#   
#   for (y in years) {
#     
#     print(y)
#     files4 <- files3[str_detect(files3, y) ]
#     r <- rast(files4)
#     r2 <- mean(r)
#     rs <- c(rs, r2)
#     
#   }
#   
#   df <- as.data.frame(rs, xy=TRUE)
#   
#   trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
#                                conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
#   
#   trendr <- rast(trend, type="xyz")
#   
#   writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", env,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
#   
#   
#   # seasons: winter #
#   m <- c("_01_02_12")
#   
#   # need to take a different file subset so that 1989 for 1990 can be included
#   files2 <- list.files("/home/master/cloud/predictors_8km", pattern=env)
#   files2 <- files2[!str_detect(files2, "1988")] 
#   files2 <- files2[!str_detect(files2, "1987")] 
#   files2 <- files2[!str_detect(files2, "1986")] 
#   files2 <- files2[!str_detect(files2, "1985")] 
#   files2 <- files2[!str_detect(files2, "1984")] 
#   files2 <- files2[!str_detect(files2, "1983")] 
#   files2 <- files2[!str_detect(files2, "1982")] 
#   files2 <- files2[!str_detect(files2, "1981")] 
#   files2 <- files2[!str_detect(files2, "1980")] 
#   files2 <- files2[!str_detect(files2, "2021")] 
#   files2 <- files2[!str_detect(files2, "2022")] 
#   files2 <- files2[!str_detect(files2, "trend")] 
#   files2 <- files2[!str_detect(files2, "average")] 
#   files2 <- files2[!str_detect(files2, "2017")] 
#   files2 <- files2[!str_detect(files2, "2018")] 
#   files2 <- files2[!str_detect(files2, "2019")] 
#   files2 <- files2[!str_detect(files2, "2020")] 
#   files2 <- files2[endsWith(files2, '*.tif$')]
#   
#   
#   files3 <- files2[str_detect(files2, "_01") | str_detect(files2, "_02") | str_detect(files2, "_12") | str_detect(files2, "_1") | str_detect(files2, "_2") | str_detect(files2, "_12")] 
#   print(files3)
#   
#   
#   rs <- rast()
#   
#   years2 <- seq(1989, 2016, 1) %>% as.character()
#   
#   for (y in years2) {
#     
#     print(y)
#     files4 <- files3[str_detect(files3, y) ]
#     files4 <- str_replace_all(files4, paste0(y, "_12"), paste0((as.numeric(y)-1) %>% as.character(), "_12"))
#     r <- rast(files4)
#     r2 <- mean(r)
#     rs <- c(rs, r2)
#     
#   }
#   
#   df <- as.data.frame(rs, xy=TRUE)
#   
#   trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
#                                conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
#   
#   trendr <- rast(trend, type="xyz")
#   
#   writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", env,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
#   
#   
#   
#   # seasons: ngs #
#   m <- c("_01_02_03_04_10_11_12")
#   
#   files3 <- files2[str_detect(files2, "_01.tif") | str_detect(files2, "_02") |str_detect(files2, "_03") |str_detect(files2, "_04") |str_detect(files2, "_10") |str_detect(files2, "_11") | str_detect(files2, "_12") | str_detect(files2, "_1.tif") | str_detect(files2, "_2") |str_detect(files2, "_3") |str_detect(files2, "_4")] 
#   print(files3)
#   
#   rs <- rast()
#   
#   years2 <- seq(1989, 2016, 1) %>% as.character()
#   
#   for (y in years2) {
#     
#     print(y)
#     files4 <- files3[str_detect(files3, y) ]
#     files4 <- str_replace_all(files4, paste0(y, "_12"), paste0((as.numeric(y)-1) %>% as.character(), "_12"))
#     files4 <- str_replace_all(files4, paste0(y, "_11"), paste0((as.numeric(y)-1) %>% as.character(), "_11"))
#     files4 <- str_replace_all(files4, paste0(y, "_10"), paste0((as.numeric(y)-1) %>% as.character(), "_10"))
#     
#     r <- rast(files4)
#     r2 <- mean(r)
#     rs <- c(rs, r2)
#     
#   }
#   
#   df <- as.data.frame(rs, xy=TRUE)
#   
#   trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
#                                conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
#   
#   trendr <- rast(trend, type="xyz")
#   
#   writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", env,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
#   
#   
#   # seasons: gs #
#   m <- c("_05_06_07_08_09")
#   
#   
#   files3 <- files[str_detect(files, "_05") | str_detect(files, "_06") | str_detect(files, "_07") | str_detect(files, "_08") | str_detect(files, "_09") | str_detect(files, "_5") | str_detect(files, "_6") | str_detect(files, "_7") | str_detect(files, "_8") | str_detect(files, "_9")] 
#   print(files3)
#   
#   rs <- rast()
#   
#   for (y in years) {
#     
#     print(y)
#     files4 <- files3[str_detect(files3, y) ]
#     r <- rast(files4)
#     r2 <- mean(r)
#     rs <- c(rs, r2)
#     
#   }
#   
#   df <- as.data.frame(rs, xy=TRUE)
#   
#   trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
#                                conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
#   
#   trendr <- rast(trend, type="xyz")
#   
#   writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", env,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
#   
#   
#   # annual #
#   
#   
#   files3 <- files
#   print(files3)
#   
#   for (y in years) {
#     
#     print(y)
#     files4 <- files3[str_detect(files3, y) ]
#     r <- rast(files4)
#     r2 <- mean(r)
#     rs <- c(rs, r2)
#     
#   }
#   
#   df <- as.data.frame(rs, xy=TRUE)
#   
#   trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
#                                conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
#   
#   trendr <- rast(trend, type="xyz")
#   
#   writeRaster(trendr, paste0("/home/master/trends_drivers_8km/", env,  "_annual","_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
#   
#   
#   rm(trendr);rm(trend);rm(r)
#   gc()
#   file.remove(list.files("/home/master/temp/", full.names=TRUE))
#   
#   
#   
# }
# 
# 
# # trends_env("tmean")
# # trends_env("ndvi")
# # trends_env("srad")
# # trends_env("soilmoist")
# trends_env("soiltemp")
# trends_env("vpd")


# # ndvi
# n <- list.files(, pattern='ndvi_')
# n <- n[7:426]
# n2 <- n[str_detect(n, "_07.tif")]
# 
# raster2 <- rast(n2)
# rasterdf <-as.data.frame(raster2, xy=TRUE)
# trend <- zyp.trend.dataframe(rasterdf, metadata.cols=2, method="yuepilon",
#                              conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
# 
# trendndvir <- rast(trend, type="xyz")
# 
# 
# trend <- zyp.trend.dataframe(rasterdf, metadata.cols=2, method="yuepilon",
#                              conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
# 
# 
# ### do the same calculations for the key study domains with zonal statistics
# 
# # biomes: tundra vs. boreal
# s <- vect("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/Ecoregions2017_tundraboreal.shp")
# s2 <- terra::project(s, "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")
# 
# # vegetation types:
# # 1 km raster:
# vegtype <- rast("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_northpolelambert1km_tundraboreal_attfix.tif")
# # 8 km raster:
# vegtype <- rast("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_aggregate_northpolelambert8km_tundraboreal_attfix.tif")
# 
# 
# # countries/regions: Alaska, Canada, Greenland+Svalbard+Finland+Sweden+Norway, Russia
# countries <- vect("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ne_10m_admin_0_countries.shp")
# countries <- crop(countries, extent(-180, 180, 40, 90))
# countries$region <- countries$NAME
# countries$region <- ifelse(countries$region=="Finland" | countries$region=="Sweden" |countries$region=="Norway" |countries$region=="Svalbard", "Northern Europe", countries$region)
# countries$region <- ifelse(countries$region=="United States of America", "Alaska", countries$region)
# countries$region <- ifelse(countries$region=="Canada"| countries$region=="Greenland" , "Canada & Greenland", countries$region)
# 
# countries2 <- subset(countries, countries$region=="Northern Europe" | countries$region=="Alaska" | countries$region=="Canada & Greenland" | countries$region=="Russia")
# 
# # permafrost zones:
# perma <- vect("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/permaice.shp")


# and then a combination of biomes and countries/regions
