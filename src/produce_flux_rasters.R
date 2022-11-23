

library("stringr")
library("stringi")
library("terra")
library(dplyr)

setwd("/home/master/")
terraOptions(memfrac=0.9, tempdir = "temp/") 

# Google cloud settings
my_project_id <- "top-operand-328213"
gcs_list_buckets(my_project_id)
gcs_global_bucket("abcflux_modeling_files")
contents <- gcs_list_objects()
gcs_upload_set_limit(50000000L) # increasing data size limit for transferring data to google cloud

### 8 km predictions ###


################ ADD FILENAMES TO THIS SCRIPT!!!

### calculate average rasters ###


avg_rasters <- function(flux, pattern1, filename) {
  
  
  
  setwd("/home/master/")
  files_to_download <- grep("*.csv", contents$name, value = TRUE)
  files_to_download2 <- files_to_download[grepl(flux, files_to_download)]
  files_to_download2 <- files_to_download2[grepl("predictions_8km/csv/0.5/", files_to_download2)]
  files_to_download2 <- files_to_download2[grepl(filename, files_to_download2)]
  
  map(files_to_download2, function(x) gcs_get_object(x, saveToDisk = x, overwrite = TRUE))
  
  
  # read to a data frame
  setwd("/home/master/predictions_8km/csv/0.5")
  files <- list.files(, pattern=flux)
  files <- files[grepl(filename, files)]
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
    print(colnames(df2)[3:length(colnames(df2))])
    
    df3 <- df2  # %>% mutate(mean = rowMeans(.[, 3:ncol(df2)])) # this would do annual averages
    df4 <- df3[, c(1:2, ncol(df3))]
    
    df4[, 3] <- unname(df4[, 3])
    
    rasterdf  <- as.matrix(df4[]) 
    r <- rast(rasterdf[], type="xyz")
    # plot(r)
    crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
    writeRaster(r, paste0("/home/master/predictions_8km/raster/0.5/", flux, m, "_", pattern1, ".tif"), overwrite=TRUE, datatype="INT4S")
    
  }
  
  print("months done")
  
  # seasons: spring #
  months <- c("_03", "_04", "_05")
    
    # y <- "1982"
    selected_columns <- str_detect(columns, paste(months, collapse = "|")) 
    
    selected_columns2 <- str_detect(columns, pattern1)
    
    selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE, TRUE, FALSE)
    
    selected_columns[1:2] <- TRUE # add coords
    df2 <- df[, selected_columns]
    print(colnames(df2)[3:length(colnames(df2))])
    
    df3 <- df2  %>% mutate(sum = rowSums(.[, 3:ncol(df2)]))
    
    df4 <- df3[, c(1, 2, ncol(df3))]
    
    rasterdf  <- as.matrix(df4[]) 
    r <- rast(rasterdf[], type="xyz")
    # plot(r)
    crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
    writeRaster(r, paste0("/home/master/predictions_8km/raster/0.5/", flux, stri_paste(months, collapse=''), "_", pattern1, "_sum.tif"), overwrite=TRUE, datatype="INT4S")
    
    
    # y <- "1982"
    selected_columns <- str_detect(columns, paste(months, collapse = "|")) 
    
    selected_columns2 <- str_detect(columns, pattern1)
    
    selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE, TRUE, FALSE)
    
    selected_columns[1:2] <- TRUE # add coords
    df2 <- df[, selected_columns]
    print(colnames(df2)[3:length(colnames(df2))])
    
    df3 <- df2  %>% mutate(sum = rowMeans(.[, 3:ncol(df2)]))
    
    df4 <- df3[, c(1, 2, ncol(df3))]
    
    rasterdf  <- as.matrix(df4[]) 
    r <- rast(rasterdf[], type="xyz")
    # plot(r)
    crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
    writeRaster(r, paste0("/home/master/predictions_8km/raster/0.5/", flux, stri_paste(months, collapse=''), "_", pattern1, "_mean.tif"), overwrite=TRUE, datatype="INT4S") 
  

  
  # seasons: summer #
  months <- c("_06", "_07", "_08")
  

    # y <- "1982"
    selected_columns <- str_detect(columns, paste(months, collapse = "|")) 
    
    selected_columns2 <- str_detect(columns, pattern1)
    
    selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE, TRUE, FALSE)
    
    selected_columns[1:2] <- TRUE # add coords
    df2 <- df[, selected_columns]
    print(colnames(df2)[3:length(colnames(df2))])
    
    df3 <- df2  %>% mutate(sum = rowSums(.[, 3:ncol(df2)]))
    
    df4 <- df3[, c(1, 2, ncol(df3))]
    
    rasterdf  <- as.matrix(df4[]) 
    r <- rast(rasterdf[], type="xyz")
    # plot(r)
    crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
    writeRaster(r, paste0("/home/master/predictions_8km/raster/0.5/", flux, stri_paste(months, collapse=''), "_", pattern1, "_sum.tif"), overwrite=TRUE, datatype="INT4S")
 

  
    # y <- "1982"
    selected_columns <- str_detect(columns, paste(months, collapse = "|")) 
    
    selected_columns2 <- str_detect(columns, pattern1)
    
    selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE, TRUE, FALSE)
    
    selected_columns[1:2] <- TRUE # add coords
    df2 <- df[, selected_columns]
    print(colnames(df2)[3:length(colnames(df2))])
    
    df3 <- df2  %>% mutate(sum = rowMeans(.[, 3:ncol(df2)]))
    
    df4 <- df3[, c(1, 2, ncol(df3))]
    
    rasterdf  <- as.matrix(df4[]) 
    r <- rast(rasterdf[], type="xyz")
    # plot(r)
    crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
    writeRaster(r, paste0("/home/master/predictions_8km/raster/0.5/", flux, stri_paste(months, collapse=''), "_", pattern1, "_mean.tif"), overwrite=TRUE, datatype="INT4S")
    
  
  
  
  
  # seasons: autumn #
  months <- c("_09", "_10", "_11")
  
    
    # y <- "2000"
    selected_columns <- str_detect(columns, paste(months, collapse = "|")) 
    
    selected_columns2 <- str_detect(columns, pattern1)
    
    selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE, TRUE, FALSE)
    
    selected_columns[1:2] <- TRUE # add coords
    df2 <- df[, selected_columns]
    print(colnames(df2)[3:length(colnames(df2))])
    
    df3 <- df2  %>% mutate(sum = rowSums(.[, 3:ncol(df2)]))
    
    df4 <- df3[, c(1, 2, ncol(df3))]
    
    rasterdf  <- as.matrix(df4[]) 
    r <- rast(rasterdf[], type="xyz")
    # plot(r)
    crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
    writeRaster(r, paste0("/home/master/predictions_8km/raster/0.5/", flux, stri_paste(months, collapse=''), "_", pattern1, "_sum.tif"), overwrite=TRUE, datatype="INT4S")
    
  
    # y <- "2000"
    selected_columns <- str_detect(columns, paste(months, collapse = "|")) 
    
    selected_columns2 <- str_detect(columns, pattern1)
    
    selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE, TRUE, FALSE)
    
    selected_columns[1:2] <- TRUE # add coords
    df2 <- df[, selected_columns]
    print(colnames(df2)[3:length(colnames(df2))])
    
    df3 <- df2  %>% mutate(sum = rowMeans(.[, 3:ncol(df2)]))
    
    df4 <- df3[, c(1, 2, ncol(df3))]
    
    rasterdf  <- as.matrix(df4[]) 
    r <- rast(rasterdf[], type="xyz")
    # plot(r)
    crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
    writeRaster(r, paste0("/home/master/predictions_8km/raster/0.5/", flux, stri_paste(months, collapse=''), "_", pattern1, "_mean.tif"), overwrite=TRUE, datatype="INT4S")
    
    
    
    
  
  print("most of 3-month seasons done")
  
  
  
  # Calculate longer seasons (GS)
  
  # seasons: autumn #
  months <- c("_05","_06", "_07", "_08", "_09")
  
    
    # y <- "1982"
    selected_columns <- str_detect(columns, paste(months, collapse = "|")) 
    
    selected_columns2 <- str_detect(columns, pattern1)
    
    selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE, TRUE, FALSE)
    
    selected_columns[1:2] <- TRUE # add coords
    df2 <- df[, selected_columns]
    print(colnames(df2)[3:length(colnames(df2))])
    
    df3 <- df2  %>% mutate(sum = rowSums(.[, 3:ncol(df2)]))
    
    df4 <- df3[, c(1, 2, ncol(df3))]
    
    rasterdf  <- as.matrix(df4[]) 
    r <- rast(rasterdf[], type="xyz")
    # plot(r)
    crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
    writeRaster(r, paste0("/home/master/predictions_8km/raster/0.5/", flux, stri_paste(months, collapse=''), "_", pattern1, "_sum.tif"), overwrite=TRUE, datatype="INT4S")

    
    # y <- "1982"
    selected_columns <- str_detect(columns, paste(months, collapse = "|")) 
    
    selected_columns2 <- str_detect(columns, pattern1)
    
    selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE, TRUE, FALSE)
    
    selected_columns[1:2] <- TRUE # add coords
    df2 <- df[, selected_columns]
    print(colnames(df2)[3:length(colnames(df2))])
    
    df3 <- df2  %>% mutate(sum = rowMeans(.[, 3:ncol(df2)]))
    
    df4 <- df3[, c(1, 2, ncol(df3))]
    
    rasterdf  <- as.matrix(df4[]) 
    r <- rast(rasterdf[], type="xyz")
    # plot(r)
    crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
    writeRaster(r, paste0("/home/master/predictions_8km/raster/0.5/", flux, stri_paste(months, collapse=''), "_", pattern1, "_mean.tif"), overwrite=TRUE, datatype="INT4S")
  
  print("GS done")
  
  

  
  # annual 
  months <- c("_01", "_02", "_03", "_04", "_05", "_06",
              "_07", "_08", "_09", "_10", "_11", "_12")
  

    
    # y <- "1982"
    selected_columns <- str_detect(columns, paste(months, collapse = "|")) 
    
    selected_columns2 <- str_detect(columns, pattern1)
    
    selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE, TRUE, FALSE)
    
    selected_columns[1:2] <- TRUE # add coords
    df2 <- df[, selected_columns]
    print(colnames(df2)[3:length(colnames(df2))])
    
    df3 <- df2  %>% mutate(sum = rowSums(.[, 3:ncol(df2)]))
    
    df4 <- df3[, c(1, 2, ncol(df3))]
    
    rasterdf  <- as.matrix(df4[]) 
    r <- rast(rasterdf[], type="xyz")
    # plot(r)
    crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
    writeRaster(r, paste0("/home/master/predictions_8km/raster/0.5/", flux, "_", pattern1, "_sum.tif"), overwrite=TRUE, datatype="INT4S")
    
  
  
  print("annual done")
  
  
  
  # seasons: winter #
  # trickier because need to take previous year into account!
  months <- c("_01", "_02", "_12")
  
  
  
  # list and select files
  # files <- list.files("/home/master/predictions_8km/csv/0.5", pattern=paste0(pattern1, collapse="|")) # old way?
  files <- list.files("/home/master/predictions_8km/csv/0.5", pattern=pattern1)
  files <- files[str_detect(files, flux)]
  files <- files[!str_detect(files, "_12")] # remove december from the same year
  files2 <- list.files("/home/master/predictions_8km/csv/0.5", pattern=paste0(as.numeric(pattern1)-1, collapse="|"))
  files2 <- files2[str_detect(files2, "_12")]
  files2 <- files2[str_detect(files2, flux)]
  files <- c(files, files2)
  files <- files[str_detect(files, flux)]
  setwd("/home/master/predictions_8km/csv/0.5")
  
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
    
    # skip 1982
    if (y!="1982") {
      
      # y <- "1983"
      # months 1-2
      selected_columns <- str_detect(columns, paste(months, collapse = "|")) 
      
      #selected_columns2 <- str_detect(columns, y)
      
      
      #selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE | selected_columnsb==TRUE & selected_columnsb2==TRUE, TRUE, FALSE)
      
      selected_columns[1:2] <- TRUE # add coords
      df2 <- df[, selected_columns]
      print(colnames(df2)[3:length(colnames(df2))])
      
      df3 <- df2  %>% mutate(sum = rowSums(.[, 3:ncol(df2)]))
      
      df4 <- df3[, c(1, 2, ncol(df3))]
      
      rasterdf  <- as.matrix(df4[]) 
      r <- rast(rasterdf[], type="xyz")
      # plot(r)
      crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
      writeRaster(r, paste0("/home/master/predictions_8km/raster/0.5/", flux, stri_paste(months, collapse=''), "_", pattern1, "_sum.tif"), overwrite=TRUE, datatype="INT4S")
      
      

      df3 <- df2  %>% mutate(sum = rowMeans(.[, 3:ncol(df2)]))
      
      df4 <- df3[, c(1, 2, ncol(df3))]
      
      rasterdf  <- as.matrix(df4[]) 
      r <- rast(rasterdf[], type="xyz")
      # plot(r)
      crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
      writeRaster(r, paste0("/home/master/predictions_8km/raster/0.5/", flux, stri_paste(months, collapse=''), "_", pattern1, "_mean.tif"), overwrite=TRUE, datatype="INT4S")
    
  }
  

  
  
  # seasons: non-growing season #
  # trickier because need to take previous year into account!
  months <- c("_01", "_02", "_03", "_04","_10","_11","_12")
  
  
  # list and select files
  files <- list.files("/home/master/predictions_8km/csv/0.5", pattern=paste0(pattern1, collapse="|"))
  files <- files[!(str_detect(files, "_12") | str_detect(files, "_11") | str_detect(files, "_10"))]# remove december from the same year
  files2 <- list.files("/home/master/predictions_8km/csv/0.5", pattern=paste0(as.numeric(pattern1)-1, collapse="|"))
  files2 <- files2[str_detect(files2, "_12") | str_detect(files2, "_11") | str_detect(files2, "_10") ]
  files <- c(files, files2)
  files <- files[str_detect(files, flux)]
  setwd("/home/master/predictions_8km/csv/0.5")
  
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
  

  for (y in pattern1) {
    
    # skip 1982
    if (y!=1982) {
      
      # y <- "1983"
      # months 1-2
      selected_columns <- str_detect(columns, paste(months, collapse = "|")) 
      
      #selected_columns2 <- str_detect(columns, y)
      
      
      #selected_columns <- ifelse(selected_columns==TRUE & selected_columns2==TRUE | selected_columnsb==TRUE & selected_columnsb2==TRUE, TRUE, FALSE)
      
      selected_columns[1:2] <- TRUE # add coords
      df2 <- df[, selected_columns]
      print(colnames(df2)[3:length(colnames(df2))])
      
      df3 <- df2  %>% mutate(sum = rowSums(.[, 3:ncol(df2)]))
      
      df4 <- df3[, c(1, 2, ncol(df3))]
      
      rasterdf  <- as.matrix(df4[]) 
      r <- rast(rasterdf[], type="xyz")
      # plot(r)
      crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
      writeRaster(r, paste0("/home/master/predictions_8km/raster/0.5/", flux, stri_paste(months, collapse=''), "_", pattern1, "_sum.tif"), overwrite=TRUE, datatype="INT4S")
      
      
      df3 <- df2  %>% mutate(sum = rowMeans(.[, 3:ncol(df2)]))
      
      df4 <- df3[, c(1, 2, ncol(df3))]
      
      rasterdf  <- as.matrix(df4[]) 
      r <- rast(rasterdf[], type="xyz")
      # plot(r)
      crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
      writeRaster(r, paste0("/home/master/predictions_8km/raster/0.5/", flux, stri_paste(months, collapse=''), "_", pattern1, "_mean.tif"), overwrite=TRUE, datatype="INT4S")
      
    }
    
    
  }
  
  
  
  
  print("all done")
  setwd("/home/master/")
  file.remove(files_to_download2)
  
  
  
  
  
}





### This is to explore the average datasubset runs
filename <- "full_model_simple"
flux <- "NEE_gC_m2"
setwd("/home/master/")
files_to_download <- grep("*.csv", contents$name, value = TRUE)
files_to_download2 <- files_to_download[grepl(flux, files_to_download)]
files_to_download2 <- files_to_download2[grepl("predictions_8km/csv/0.5/", files_to_download2)]
files_to_download2 <- files_to_download2[grepl(filename, files_to_download2)]

map(files_to_download2, function(x) gcs_get_object(x, saveToDisk = x, overwrite = TRUE))


# read to a data frame
setwd("/home/master/predictions_8km/csv/0.5")
files <- list.files(, pattern=flux)
files <- files[grepl(filename, files)]
df <- do.call(cbind,lapply(files,read.csv)) # can do this because the number of rows is same across the predictions!
selected_columns <- c(1, 2, seq(3, 36, by=3))
df2 <- df[, selected_columns]
print(colnames(df2)[3:length(colnames(df2))])
df3 <- df2  %>% mutate(sum = rowSums(.[, 3:ncol(df2)]))
df4 <- df3[, c(1, 2, ncol(df3))]
rasterdf  <- as.matrix(df4[]) 
r <- rast(rasterdf[], type="xyz")
# plot(r)
crs(r) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
writeRaster(r/1000, paste0("/home/master/predictions_8km/raster/0.5/NEE_gC_m2_simple.tif"))



years <- seq(2000, 2011, by=1) # TEMPORARY

for (y in years) {
  
  avg_rasters(flux="NEE_gC_m2", pattern1=seq(y, y, by=1)%>% as.character(), filename="full_model_without_srad")
  
}

for (y in years) {
  
  avg_rasters(flux="GPP_gC_m2", pattern1=seq(y, y, by=1)%>% as.character(), filename="full_model_without_larvaloutbreak")
  
}

for (y in years) {
  
  avg_rasters(flux="Reco_gC_m2", pattern1=seq(y, y, by=1)%>% as.character(), filename="full_model_without_larvaloutbreak")
  
}


# average annual gpp and reco, and the budget from them
files <- list.files("/home/master/predictions_8km/raster/0.5/", pattern="NEE")
files <- files[nchar(files)==22]
setwd("/home/master/predictions_8km/raster/0.5/")
reco <- rast(files)
reco <- mean(reco)/1000
writeRaster(reco, "/home/master/predictions_8km_mean/NEE_gC_m2_2000_2016_sum_mean.tif", overwrite=TRUE)



# average annual gpp and reco, and the budget from them
files <- list.files("/home/master/predictions_8km/raster/0.5/", pattern="Reco")
files <- files[nchar(files)==23]
setwd("/home/master/predictions_8km/raster/0.5/")
reco <- rast(files)
reco <- mean(reco)/1000



files <- list.files("/home/master/predictions_8km/raster/0.5/", pattern="GPP")
files <- files[nchar(files)==22]
setwd("/home/master/predictions_8km/raster/0.5/")
gpp <- rast(files)
gpp <- mean(gpp)/1000


nee=gpp+reco

writeRaster(reco, "/home/master/predictions_8km/raster/0.5/Reco_gC_m2_2000_2016_sum_mean.tif", overwrite=TRUE)
writeRaster(gpp, "/home/master/predictions_8km/raster/0.5/GPP_gC_m2_2000_2016_sum_mean.tif", overwrite=TRUE)
writeRaster(nee, "/home/master/predictions_8km/raster/0.5/NEE_from_GPPReco_m2_2000_2016_sum_mean.tif", overwrite=TRUE)


# average annual, gs, and ngs fluxes of nee
setwd("/home/master/predictions_8km/raster/0.5/")

# virkkala 1990-2015
files <- list.files("/home/master/predictions_8km/raster/0.5/")
files2 <- files[nchar(files)==18]
files3 <- files2[9:34]
setwd("/home/master/predictions_8km/raster/0.5/")
r <- rast(files3)
r2 <- mean(r)
writeRaster(r2, "/home/master/predictions_8km/raster/0.5/NEE_annual_avg_19902015.tif", overwrite=TRUE, datatype="INT4S")

# natali 2003-2016
files <- list.files("/home/master/predictions_8km/raster/0.5/", pattern="01_02_03_04_10_11_12")
files3 <- files[22:35]
r <- rast(files3)
r2 <- mean(r)
writeRaster(r2, "/home/master/predictions_8km/raster/0.5/NEE_ngs_avg_20032016.tif", overwrite=TRUE, datatype="INT4S")

# natali 2003-2016
files <- list.files("/home/master/predictions_8km/raster/0.5/", pattern="01_02_03_04_10_11_12")
files3 <- files[22:35]
r <- rast(files3)
r2 <- mean(r)
writeRaster(r2, "/home/master/predictions_8km/raster/0.5/NEE_ngs_avg_20032016.tif", overwrite=TRUE, datatype="INT4S")


# here 1990-2016
files <- list.files("/home/master/predictions_8km/raster/0.5/")
files3 <- files[nchar(files)==22]
r <- rast(files3)
r2 <- mean(r)
writeRaster(r2, "/home/master/predictions_8km/raster/0.5/NEE_avg_19902016.tif", overwrite=TRUE, datatype="INT4S")

files <- list.files("/home/master/predictions_8km/raster/0.5/", pattern="01_02_03_04_10_11_12")
files3 <- files[str_detect(files, "sum")]
r <- rast(files3)
r2 <- mean(r)
writeRaster(r2, "/home/master/predictions_8km/raster/0.5/NEE_avg_19902016_ngs.tif", overwrite=TRUE, datatype="INT4S")


files <- list.files("/home/master/predictions_8km/raster/0.5/", pattern="05_06_07_08")
files3 <- files[str_detect(files, "sum")]
r <- rast(files3)
r2 <- mean(r)
writeRaster(r2, "/home/master/predictions_8km/raster/0.5/NEE_avg_19902016_gs.tif", overwrite=TRUE, datatype="INT4S")

