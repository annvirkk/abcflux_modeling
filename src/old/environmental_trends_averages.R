


library("zyp", lib.loc="/mnt/data1/boreal/avirkkala/packages") 
library("stringi")
#library("terra", lib.loc="/mnt/data1/boreal/avirkkala/packages")
library("terra")


terraOptions(memfrac=0.9, tempdir = "/mnt/data1/boreal/avirkkala/Temp/change/")


# Functions to estimate annual, seasonal, and monthly trends and average RASTERS across different time periods


annual_function <- function(files, variable, division_factor, output_folder_avg, output_folder_trend, mask_file, km) {
  
  
  # load rasters
  r <- rast(files)
  
  # name rasters
  names(r) <- sources(r)$source
  print(names(r))
  
  # mask rasters
  #r <- mask(r, mask=mask_file)
  
  # # TEMPORARY CROP!!!
  r <- crop(r, ext(-4834843, 0, -4834843, 0))
  
  # divide by the division factor to get back to the original scale
  r1<- r/division_factor
  
  rm(r); gc()
  
  print("starting annual calculation")
  
  # calculate annual averages
  if (km=="1km") {
    
    years <- seq(2001, 2020, by=1)
    
    for (y in years) {
      
      print(y)
      # y <- 2001
      r2 <- r1[[grepl(y, names(r1))]]
      
      if (y==2001) {
        r3 <- r2
      } else {
        
        add(r3) <- r2
        
      }
      
    }
    
    
  }
  
  
  rm(r1); rm(r2); gc()
  

  print("annual means calculated")
  
  # calculate averages to different time splits based on km model outputs
  
  if (km=="1km") {
    
    
    # 2001-2005
    r4 <- mean(r3[[1:5]]) # drop na.rm=TRUE because there shouldn't be any NAs
    writeRaster(r4, paste0(output_folder_avg, variable, "_avg_20012005.tif"), overwrite=TRUE)
    
    # 2006-2010
    r4 <- mean(r3[[6:10]]) # drop na.rm=TRUE because there shouldn't be any NAs
    writeRaster(r4, paste0(output_folder_avg, variable, "_avg_20062010.tif"), overwrite=TRUE)
    
    # 2011-2015
    r4 <- mean(r3[[11:15]]) # drop na.rm=TRUE because there shouldn't be any NAs
    writeRaster(r4, paste0(output_folder_avg, variable, "_avg_20112015.tif"), overwrite=TRUE)
    
    # 2016-2020
    r4 <- mean(r3[[16:20]]) # drop na.rm=TRUE because there shouldn't be any NAs
    writeRaster(r4, paste0(output_folder_avg, variable, "_avg_20162020.tif"), overwrite=TRUE)
    
    
    # 2001-2010
    r4 <- mean(r3[[1:10]]) # drop na.rm=TRUE because there shouldn't be any NAs
    writeRaster(r4, paste0(output_folder_avg, variable, "_avg_20012010.tif"), overwrite=TRUE)
    
    # 2011-2020
    r4 <- mean(r3[[11:20]]) # drop na.rm=TRUE because there shouldn't be any NAs
    writeRaster(r4, paste0(output_folder_avg, variable, "_avg_20112020.tif"), overwrite=TRUE)
    
    rm(r4); gc()
    
  }
  
  print("average rasters writte out")
  
  # convert to data frame
  r1_df <- terra::as.data.frame(r3, xy=TRUE)
  r1_df <- na.omit(r1_df)
  str(r1_df)

  # calculate trend
  print("calculating trend")
  trend <- zyp.trend.dataframe(r1_df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  print("trend calculated")
  
  trendval <- as.matrix(cbind(r1_df$x, r1_df$y, round(trend$trend)))
  pval <- as.matrix(cbind(r1_df$x, r1_df$y, round(trend$sig)))
  
  # convert to raster and write out
  trendval <- as.data.frame(cbind(r1_df$x, r1_df$y, trend$trend))
  rtrend <- terra::rast(trendval, crs=crs(r3), type="xyz")
  # trend the Sen’s slope (trend) per unit time. Note that there's another column: trendp the Sen’s slope (trend) over the time period.
  # crs="+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
  writeRaster(rtrend, paste0(output_folder_trend, variable, "_annual_trend_20012010.tif", overwrite=TRUE))
  
  ptrend <- terra::rast(pval, crs=crs(r1), type="xyz")
  writeRaster(ptrend, paste0(output_folder_trend, variable, "_annual_p_20012010.tif", overwrite=TRUE))

  tmpFiles(remove=TRUE)
  
}

# run function
files <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_1km", pattern="tmean_.*.tif$", full.names=T) # check pattern here
#1980, 1990, 2000 and 2021 cannot be considered!!!
files <- files[!grepl("2000", files) & !grepl("2021", files) & !grepl("199", files) & !grepl("198", files) & !grepl("197", files) & !grepl("196", files) & !grepl("195", files)]
output_folder_trend <- ("/mnt/data1/boreal/avirkkala/abcflux_modeling/environmental_trend/")
output_folder_avg <- ("/mnt/data1/boreal/avirkkala/abcflux_modeling/environmental_avg/")
division_factor <- 100
variable <- "tmean"
mask_file <- rast("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/NEE_gC_m2_annual_sum_1km_gbm_loocv.tif") # this file will be the same as the average flux raster to make sure that there's a same amount of data each year and month
km <- "1km"
annual_function(files, "tmean", 100, 
                ("/mnt/data1/boreal/avirkkala/abcflux_modeling/environment_avg/"), ("/mnt/data1/boreal/avirkkala/abcflux_modeling/environment_trend/"),
                mask_file, "1km")







### 2. seasonal

## winter season




## spring season




## summer season



## autumn season
  

  
# Function to estimate annual, seasonal, and monthly trends and average VALUES across different time periods and regions