

library("stringr")
library("stringi")
library("terra")
library(dplyr)
terraOptions(memfrac=0.9, tempdir = "/mnt/data1/boreal/avirkkala/Temp") 

### 8 km predictions ###




### calculate average rasters ###


avg_rasters <- function(flux, pattern1, minmaxyears) {
  
  
  # list and select files
  files <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_8km", pattern=paste0(pattern1, collapse="|"))
  files <- files[str_detect(files, flux)]
  setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_8km")
  
  # read to a data frame
  df <- do.call(cbind,lapply(files,read.csv)) # can do this because the number of rows is same across the predictions!
  
  # rename
  columns <- NA
  for (f in files) {
    column <- c("x", "y", f)
    columns <- c(columns, column)
    
  }
  
  columns <- columns[2:length(columns)]
  names(df) <- columns
  
  ## temporal splits
  
  
  # months #
  months <- c("_01", "_02", "_03", "_04", "_05", "_06",
              "_07", "_08", "_09", "_10", "_11", "_12")
  
  for (m in months) {
    # m <- "_01"
    selected_columns <- str_detect(columns, m)
    
    selected_columns[1:2] <- TRUE # add coords
    df2 <- df[, selected_columns]
    print(colnames(df2))
    
    df3 <- df2  %>% mutate(mean = rowMeans(.[, 3:ncol(df2)]))
    df4 <- df3[, c(1:2, ncol(df3))]
    
    df4[, 3] <- unname(df4[, 3])/1000
    
    rasterdf  <- as.matrix(df4[]) 
    r <- rast(rasterdf[], type="xyz")
    # plot(r)
    crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
    writeRaster(r, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_8km_rasters/", flux, m, "_", minmaxyears, ".tif"), overwrite=TRUE)
    
  }
  
  print("months done")
  
  # seasons: spring #
  months <- c("_03", "_04", "_05")
  
  
  # loop over years and calculate rowwise sum, then out of those several sums take the mean
  yearly_sums <- NA
  
  for (y in pattern1) {
    
    # y <- "1982"
    selected_columns <- str_detect(columns, paste(months, collapse = "|")) 
    
    selected_columns2 <- str_detect(columns, y)
    
    selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE, TRUE, FALSE)
    
    selected_columns[1:2] <- TRUE # add coords
    df2 <- df[, selected_columns]
    print(colnames(df2))
    
    df3 <- df2  %>% mutate(sum = rowSums(.[, 3:ncol(df2)]))
    
    yearly_sums <- data.frame(cbind(yearly_sums, df3$sum/1000))
    
    
    
    
  }
  
  
  yearly_sums2 <- rowMeans(yearly_sums[, 2:ncol(yearly_sums)])
  df4 <- cbind(df3[, 1:2], yearly_sums2)
  
  #df4[, 3] <- unname(df4[, 3])/1000
  
  rasterdf  <- as.matrix(df4[]) 
  r <- rast(rasterdf[], type="xyz")
  # plot(r)
  crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
  writeRaster(r, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_8km_rasters/", flux, stri_paste(months, collapse=''), "_", minmaxyears, ".tif"), overwrite=TRUE)
  
  
  
  # seasons: summer #
  months <- c("_06", "_07", "_08")
  
  
  # loop over years and calculate rowwise sum, then out of those several sums take the mean
  yearly_sums <- NA
  
  for (y in pattern1) {
    
    # y <- "1982"
    selected_columns <- str_detect(columns, paste(months, collapse = "|")) 
    
    selected_columns2 <- str_detect(columns, y)
    
    selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE, TRUE, FALSE)
    
    selected_columns[1:2] <- TRUE # add coords
    df2 <- df[, selected_columns]
    print(colnames(df2))
    
    df3 <- df2  %>% mutate(sum = rowSums(.[, 3:ncol(df2)]))
    
    yearly_sums <- data.frame(cbind(yearly_sums, df3$sum/1000))
    
    
    
    
  }
  
  
  yearly_sums2 <- rowMeans(yearly_sums[, 2:ncol(yearly_sums)])
  df4 <- cbind(df3[, 1:2], yearly_sums2)
  
  #df4[, 3] <- unname(df4[, 3])/1000
  
  rasterdf  <- as.matrix(df4[]) 
  r <- rast(rasterdf[], type="xyz")
  # plot(r)
  crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
  writeRaster(r, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_8km_rasters/", flux, stri_paste(months, collapse=''), "_", minmaxyears, ".tif"), overwrite=TRUE)
  
  
  
  
  # seasons: autumn #
  months <- c("_09", "_10", "_11")
  
  
  # loop over years and calculate rowwise sum, then out of those several sums take the mean
  yearly_sums <- NA
  
  for (y in pattern1) {
    
    # y <- "1982"
    selected_columns <- str_detect(columns, paste(months, collapse = "|")) 
    
    selected_columns2 <- str_detect(columns, y)
    
    selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE, TRUE, FALSE)
    
    selected_columns[1:2] <- TRUE # add coords
    df2 <- df[, selected_columns]
    print(colnames(df2))
    
    df3 <- df2  %>% mutate(sum = rowSums(.[, 3:ncol(df2)]))
    
    yearly_sums <- data.frame(cbind(yearly_sums, df3$sum/1000))
    
    
    
    
  }
  
  
  yearly_sums2 <- rowMeans(yearly_sums[, 2:ncol(yearly_sums)])
  df4 <- cbind(df3[, 1:2], yearly_sums2)
  
  #df4[, 3] <- unname(df4[, 3])/1000
  
  rasterdf  <- as.matrix(df4[]) 
  r <- rast(rasterdf[], type="xyz")
  # plot(r)
  crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
  writeRaster(r, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_8km_rasters/", flux, stri_paste(months, collapse=''), "_", minmaxyears, ".tif"), overwrite=TRUE)
  
  
  
  
  
  
  
  # seasons: winter #
  months <- c("_01", "_02", "_12")
  
  
  # loop over years and calculate rowwise sum, then out of those several sums take the mean
  yearly_sums <- NA
  
  for (y in pattern1) {
    
    # skip 1982
    if (y!=1982) {
      
      # y <- "1983"
      # months 1-2
      selected_columns <- str_detect(columns, paste(months[1:2], collapse = "|")) 
      
      selected_columns2 <- str_detect(columns, y)
      
      selected_columnsb <- str_detect(columns, paste(months[3], collapse = "|")) 
      
      selected_columnsb2 <- str_detect(columns, as.character(as.numeric(y)-1))
      
      selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE | selected_columnsb==TRUE & selected_columnsb2==TRUE, TRUE, FALSE)
      
      selected_columns[1:2] <- TRUE # add coords
      df2 <- df[, selected_columns]
      print(colnames(df2))
      
      df3 <- df2  %>% mutate(sum = rowSums(.[, 3:ncol(df2)]))
      
      yearly_sums <- data.frame(cbind(yearly_sums, df3$sum/1000))
      
      
      
    }
    
    
    
    
    
  }
  
  
  yearly_sums2 <- rowMeans(yearly_sums[, 2:ncol(yearly_sums)])
  df4 <- cbind(df3[, 1:2], yearly_sums2)
  
  #df4[, 3] <- unname(df4[, 3])/1000
  
  rasterdf  <- as.matrix(df4[]) 
  r <- rast(rasterdf[], type="xyz")
  # plot(r)
  crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
  writeRaster(r, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_8km_rasters/", flux, stri_paste(months, collapse=''), "_", minmaxyears, ".tif"), overwrite=TRUE)
  
  
  print("seasons done")
  
  
  
  # annual 
  months <- c("_01", "_02", "_03", "_04", "_05", "_06",
              "_07", "_08", "_09", "_10", "_11", "_12")
  
  
  
  # loop over years and calculate rowwise sum, then out of those several sums take the mean
  yearly_sums <- NA
  
  for (y in pattern1) {
    
    # y <- "1982"
    selected_columns <- str_detect(columns, paste(months, collapse = "|")) 
    
    selected_columns2 <- str_detect(columns, y)
    
    selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE, TRUE, FALSE)
    
    selected_columns[1:2] <- TRUE # add coords
    df2 <- df[, selected_columns]
    print(colnames(df2))
    
    df3 <- df2  %>% mutate(sum = rowSums(.[, 3:ncol(df2)]))
    
    yearly_sums <- data.frame(cbind(yearly_sums, df3$sum/1000))
    
    
    
    
  }
  
  
  yearly_sums2 <- rowMeans(yearly_sums[, 2:ncol(yearly_sums)])
  df4 <- cbind(df3[, 1:2], yearly_sums2)
  
  #df4[, 3] <- unname(df4[, 3])/1000
  
  rasterdf  <- as.matrix(df4[]) 
  r <- rast(rasterdf[], type="xyz")
  # plot(r)
  crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
  writeRaster(r, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_8km_rasters/", flux, "_", minmaxyears, ".tif"), overwrite=TRUE)
  
  print("annual done")
  
 
  
}






#1982-1984
avg_rasters(flux="NEE_gC_m2", pattern1=seq(1982, 1984, by=1)%>% as.character(), minmaxyears="19821984")
avg_rasters(flux="GPP_gC_m2", pattern1=seq(1982, 1984, by=1)%>% as.character(), minmaxyears="19821984")
avg_rasters(flux="Reco_gC_m2", pattern1=seq(1982, 1984, by=1)%>% as.character(), minmaxyears="19821984")

#1985-1989
avg_rasters(flux="NEE_gC_m2", pattern1=seq(1985, 1989, by=1)%>% as.character(), minmaxyears="19851989")
avg_rasters(flux="GPP_gC_m2", pattern1=seq(1985, 1989, by=1)%>% as.character(), minmaxyears="19851989")
avg_rasters(flux="Reco_gC_m2", pattern1=seq(1985, 1989, by=1)%>% as.character(), minmaxyears="19851989")

#1990-1994
avg_rasters(flux="NEE_gC_m2", pattern1=seq(1990, 1994, by=1)%>% as.character(), minmaxyears="19901994")
avg_rasters(flux="GPP_gC_m2", pattern1=seq(1990, 1994, by=1)%>% as.character(), minmaxyears="19901994")
avg_rasters(flux="Reco_gC_m2", pattern1=seq(1990, 1994, by=1)%>% as.character(), minmaxyears="19901994")

#1995-1999
avg_rasters(flux="NEE_gC_m2", pattern1=seq(1995, 1999, by=1)%>% as.character(), minmaxyears="19951999")
avg_rasters(flux="GPP_gC_m2", pattern1=seq(1995, 1999,by=1)%>% as.character(), minmaxyears="19951999")
avg_rasters(flux="Reco_gC_m2", pattern1=seq(1995, 1999, by=1)%>% as.character(), minmaxyears="19951999")

#2000-2004
avg_rasters(flux="NEE_gC_m2", pattern1=seq(2000, 2004, by=1)%>% as.character(), minmaxyears="20002004")
avg_rasters(flux="GPP_gC_m2", pattern1=seq(2000, 2004, by=1)%>% as.character(), minmaxyears="20002004")
avg_rasters(flux="Reco_gC_m2", pattern1=seq(2000, 2004, by=1)%>% as.character(), minmaxyears="20002004")

#2005-2009
avg_rasters(flux="NEE_gC_m2", pattern1=seq(2005, 2009, by=1)%>% as.character(), minmaxyears="20052009")
avg_rasters(flux="GPP_gC_m2", pattern1=seq(2005, 2009, by=1)%>% as.character(), minmaxyears="20052009")
avg_rasters(flux="Reco_gC_m2", pattern1=seq(2005, 2009, by=1)%>% as.character(), minmaxyears="20052009")

#2010-2014

#2015-2016

#1982-2016





### calculate budgets for each year
# separately for each month, climatological season, and full year
# note that for the climatological season (DJF, MAM, JJA, SON) and full year budgets, we'll need to aggregate the monthly predictions to cumulative fluxes for each pixel first (i.e. sum of pixel values from the same year)
# winter season always includes the December value from the past year and Jan & Feb from the current year
# budget will be calculated by summing all the pixels in the output together

### calculate average and standard deviations of fluxes
# calculated for the same temporal periods as the budgets, but this time the final step just calculates an average flux and its st deve, not the sum of the fluxes


### calculate pixel-wise trends using zyp package 
# calculated for the same temporal periods as above (so separate trends for each month, season, and annual flux)

trend <- zyp.trend.dataframe(rasterdf, metadata.cols=2, method="yuepilon",
                             conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)


### do the same calculations for the key study domains with zonal statistics

# biomes: tundra vs. boreal
s <- vect("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/Ecoregions2017_tundraboreal.shp")
s2 <- terra::project(s, "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")

# vegetation types:
# 1 km raster:
vegtype <- rast("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_northpolelambert1km_tundraboreal_attfix.tif")
# 8 km raster:
vegtype <- rast("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_aggregate_northpolelambert8km_tundraboreal_attfix.tif")


# countries/regions: Alaska, Canada, Greenland+Svalbard+Finland+Sweden+Norway, Russia
countries <- vect("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ne_10m_admin_0_countries.shp")
countries <- crop(countries, extent(-180, 180, 40, 90))
countries$region <- countries$NAME
countries$region <- ifelse(countries$region=="Finland" | countries$region=="Sweden" |countries$region=="Norway" |countries$region=="Svalbard", "Northern Europe", countries$region)
countries$region <- ifelse(countries$region=="United States of America", "Alaska", countries$region)
countries$region <- ifelse(countries$region=="Canada"| countries$region=="Greenland" , "Canada & Greenland", countries$region)

countries2 <- subset(countries, countries$region=="Northern Europe" | countries$region=="Alaska" | countries$region=="Canada & Greenland" | countries$region=="Russia")

# permafrost zones:
perma <- vect("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/permaice.shp")


# and then a combination of biomes and countries/regions



