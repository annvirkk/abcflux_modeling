
#install.packages("terra", dependencies=TRUE)
library(terra, lib.loc="R/x86_64-pc-linux-gnu-library/4.2")
#install.packages("zyp", dependencies=TRUE)
library(Kendall, lib.loc="R/x86_64-pc-linux-gnu-library/4.2")

library(zyp, lib.loc="R/x86_64-pc-linux-gnu-library/4.2")
#install.packages("dplyr", dependencies=TRUE)
library(dplyr, lib.loc="R/x86_64-pc-linux-gnu-library/4.2")
#install.packages("stringi", dependencies=TRUE)
library(stringi, lib.loc="R/x86_64-pc-linux-gnu-library/4.2")
#install.packages("stringr", dependencies=TRUE)
library(stringr, lib.loc="R/x86_64-pc-linux-gnu-library/4.2")

### calculate budgets for each year
# separately for each month, climatological season, and full year
# note that for the climatological season (DJF, MAm, "_trend", JJA, SON) and full year budgets, we'll need to aggregate the monthly predictions to cumulative fluxes for each pixel first (i.e. sum of pixel values from the same year)
# winter season always includes the December value from the past year and Jan & Feb from the current year
# budget will be calculated by summing all the pixels in the output together

### calculate average and standard deviations of fluxes
# calculated for the same temporal periods as the budgets, but this time the final step just calculates an average flux and its st deve, not the sum of the fluxes


### calculate pixel-wise flux trends using zyp package 
# calculated for the same temporal periods as above (so separate trends for each month, season, and annual flux)

# function to go through months, years, seasons
setwd("/home/master/local_outputs/predictions_8km/raster/0.5")
terraOptions(memfrac=0.9, tempdir = "/home/master/temp/") 
trends <- function(flux) {
  
  months <- c("_01", "_02", "_03", "_04", "_05", "_06",
              "_07", "_08", "_09", "_10", "_11", "_12")
  
  files <- list.files("/home/master/local_outputs/predictions_8km/raster/0.5", pattern=flux)
  files <- files[!str_detect(files, "198")] 
  files2 <- files[nchar(files)<25 & nchar(files)>20]

  
  for (m in months) {
    
    files3 <- files2[str_detect(files2, m)] 
    
    r <- rast(files3)
    df <- as.data.frame(r, xy=TRUE)
    
    trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                                 conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
    
    trendr <- rast(trend, type="xyz")
    
    writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", flux,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
    
    
    print(paste(m, "done"))
    
    
  }
  
  file.remove(list.files("/home/master/temp/", full.names=TRUE))
  
  
  # seasons: spring #
  m <- c("_03_04_05")
  
  
  files3 <- files[str_detect(files, m)] 
  
  r <- rast(files3)
  df <- as.data.frame(r, xy=TRUE)
  
  trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  trendr <- rast(trend, type="xyz")
  
  writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", flux,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
  
  
  
  # seasons: summer #
  m <- c("_06_07_08")
  
  
  files3 <- files[str_detect(files, m)] 
  
  r <- rast(files3)
  df <- as.data.frame(r, xy=TRUE)
  
  trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  trendr <- rast(trend, type="xyz")
  
  writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", flux,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
  
  
  # seasons: autumn #
  m <- c("_09_10_11")
  
  
  files3 <- files[str_detect(files, m)] 
  
  r <- rast(files3)
  df <- as.data.frame(r, xy=TRUE)
  
  trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  trendr <- rast(trend, type="xyz")
  
  writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", flux,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
 
  
  # seasons: winter #
  m <- c("_01_02_12")
  
  
  files3 <- files[str_detect(files, m)] 
  
  r <- rast(files3)
  df <- as.data.frame(r, xy=TRUE)
  
  trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  trendr <- rast(trend, type="xyz")
  
  writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", flux,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
  
  
  
  # seasons: ngs #
  m <- c("_01_02_03_04_10_11_12")
  
  
  files3 <- files[str_detect(files, m)] 
  
  r <- rast(files3)
  df <- as.data.frame(r, xy=TRUE)
  
  trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  trendr <- rast(trend, type="xyz")
  
  writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", flux,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
  
  file.remove(list.files("/home/master/temp/", full.names=TRUE))
  
  
  # seasons: gs #
  m <- c("_05_06_07_08_09")
  
  
  files3 <- files[str_detect(files, m)] 
  
  r <- rast(files3)
  df <- as.data.frame(r, xy=TRUE)
  
  trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  trendr <- rast(trend, type="xyz")
  
  writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", flux,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
  
  
  # annual #


  files3 <- files[nchar(files)==18] 
  
  r <- rast(files3)
  df <- as.data.frame(r, xy=TRUE)
  
  trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  trendr <- rast(trend, type="xyz")
  
  writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", flux,  "_annual","_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
   

  rm(trendr);rm(trend);rm(r)
  gc()
  
  file.remove(list.files("/home/master/temp/", full.names=TRUE))
  
  
  
}


trends("NEE_gC_m2")
#trends("Reco_gC_m2")











### calculate pixel-wise environmental trends using zyp package 
setwd("/home/master/cloud/predictors_8km")
years <- seq(1990, 2016, 1) %>% as.character()
vector.is.empty <- function(x) return(length(x) ==0 )
trends_env <- function(env) {
  
  print(env)
  
  months <- c("_01", "_02", "_03", "_04", "_05", "_06",
              "_07", "_08", "_09", "_10", "_11", "_12")
  
  files <- list.files("/home/master/cloud/predictors_8km", pattern=env)
  files <- files[!str_detect(files, "198")] 
  files <- files[!str_detect(files, "2021")] 
  files <- files[!str_detect(files, "2022")] 
  files <- files[!str_detect(files, "trend")] 
  
  
  print(files)
  
  
  for (m in months) {
    
    m2 <- sub("0+", "", m)
    files3 <- files[str_detect(files, m) | str_detect(files, m2)] 

    r <- rast(files3)
    df <- as.data.frame(r, xy=TRUE)
    
    trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                                 conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
    
    trendr <- rast(trend, type="xyz")
    
    writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", env,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
    
    
    print(paste(m, "done"))
    
    
  }
  
  
  # seasons: spring #
  m <- c("_03_04_05")
  
  files3 <- files[str_detect(files, "_03") | str_detect(files, "_04") | str_detect(files, "_05") | str_detect(files, "_3") | str_detect(files, "_4") | str_detect(files, "_5")] 
  
  rs <- rast()
  
  for (y in years) {
    
    print(y)
    files4 <- files3[str_detect(files3, y) ]
    r <- rast(files4)
    r2 <- mean(r)
    rs <- c(rs, r2)
    
  }

  df <- as.data.frame(rs, xy=TRUE)
  
  trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  trendr <- rast(trend, type="xyz")
  
  writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", env,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
  
  
  
  # seasons: summer #
  m <- c("_06_07_08")
  
  files3 <- files[str_detect(files, "_06") | str_detect(files, "_07") | str_detect(files, "_08") | str_detect(files, "_6") | str_detect(files, "_7") | str_detect(files, "_8")] 
  
  rs <- rast()
  
  for (y in years) {
    
    print(y)
    files4 <- files3[str_detect(files3, y) ]
    r <- rast(files4)
    r2 <- mean(r)
    rs <- c(rs, r2)
    
  }
  
  df <- as.data.frame(rs, xy=TRUE)
  
  trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  trendr <- rast(trend, type="xyz")
  
  writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", env,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
  
  print("spring and summer done")
  file.remove(list.files("/home/master/temp/", full.names=TRUE))
  
  # seasons: autumn #
  m <- c("_09_10_11")
  
  files3 <- files[str_detect(files, "_09") | str_detect(files, "_10") | str_detect(files, "_11") | str_detect(files, "_9") | str_detect(files, "_10") | str_detect(files, "_11")] 
  
  rs <- rast()
  
  for (y in years) {
    
    print(y)
    files4 <- files3[str_detect(files3, y) ]
    r <- rast(files4)
    r2 <- mean(r)
    rs <- c(rs, r2)
    
  }
  
  df <- as.data.frame(rs, xy=TRUE)
  
  trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  trendr <- rast(trend, type="xyz")
  
  writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", env,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
  
  
  # seasons: winter #
  m <- c("_01_02_12")
  
  files3 <- files[str_detect(files, "_01") | str_detect(files, "_02") | str_detect(files, "_12") | str_detect(files, "_1") | str_detect(files, "_2") | str_detect(files, "_12")] 
  
  rs <- rast()
  
  years2 <- seq(1989, 2016, 1) %>% as.character()
  
  for (y in years2) {
    
    print(y)
    files4 <- files3[str_detect(files3, y) ]
    files4 <- str_replace_all(files4, paste0(y, "_12"), paste0((as.numeric(y)-1) %>% as.character(), "_12"))
    r <- rast(files4)
    r2 <- mean(r)
    rs <- c(rs, r2)
    
  }
  
  df <- as.data.frame(rs, xy=TRUE)
  
  trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  trendr <- rast(trend, type="xyz")
  
  writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", env,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
  
  
  
  # seasons: ngs #
  m <- c("_01_02_03_04_10_11_12")
  
  files3 <- files[str_detect(files, "_01") | str_detect(files, "_02") |str_detect(files, "_03") |str_detect(files, "_04") |str_detect(files, "_10") |str_detect(files, "_11") | str_detect(files, "_12") | str_detect(files, "_1") | str_detect(files, "_2") |str_detect(files, "_3") |str_detect(files, "_4")] 
  
  rs <- rast()
  
  years2 <- seq(1989, 2016, 1) %>% as.character()
  
  for (y in years2) {
    
    print(y)
    files4 <- files3[str_detect(files3, y) ]
    files4 <- str_replace_all(files4, paste0(y, "_12"), paste0((as.numeric(y)-1) %>% as.character(), "_12"))
    files4 <- str_replace_all(files4, paste0(y, "_11"), paste0((as.numeric(y)-1) %>% as.character(), "_11"))
    files4 <- str_replace_all(files4, paste0(y, "_10"), paste0((as.numeric(y)-1) %>% as.character(), "_10"))
    
    r <- rast(files4)
    r2 <- mean(r)
    rs <- c(rs, r2)
    
  }
  
  df <- as.data.frame(rs, xy=TRUE)
  
  trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  trendr <- rast(trend, type="xyz")
  
  writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", env,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
  
  
  # seasons: gs #
  m <- c("_05_06_07_08_09")
  
  
  files3 <- files[str_detect(files, "_05") | str_detect(files, "_06") | str_detect(files, "_07") | str_detect(files, "_08") | str_detect(files, "_09") | str_detect(files, "_5") | str_detect(files, "_6") | str_detect(files, "_7") | str_detect(files, "_8") | str_detect(files, "_9")] 
  
  rs <- rast()
  
  for (y in years) {
    
    print(y)
    files4 <- files3[str_detect(files3, y) ]
    r <- rast(files4)
    r2 <- mean(r)
    rs <- c(rs, r2)
    
  }
  
  df <- as.data.frame(rs, xy=TRUE)
  
  trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  trendr <- rast(trend, type="xyz")
  
  writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", env,  m, "_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
  
  
  # annual #
  
  
  files3 <- files
  
  for (y in years) {
    
    print(y)
    files4 <- files3[str_detect(files3, y) ]
    r <- rast(files4)
    r2 <- mean(r)
    rs <- c(rs, r2)
    
  }
  
  df <- as.data.frame(rs, xy=TRUE)
  
  trend <- zyp.trend.dataframe(df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  trendr <- rast(trend, type="xyz")
  
  writeRaster(trendr, paste0("/home/master/local_outputs/trends_drivers_8km/", env,  "_annual","_trend", ".tif"), overwrite=TRUE, datatype="INT4S")
  
  
  rm(trendr);rm(trend);rm(r)
  gc()
  file.remove(list.files("/home/master/temp/", full.names=TRUE))
  
  
  
}


trends_env("tmean")

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
