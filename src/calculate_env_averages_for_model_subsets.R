
library("terra")
library(googleCloudStorageR)
library(purrr)

###### Accessing data from the cloud
my_project_id <- "top-operand-328213"
gcs_list_buckets(my_project_id)
gcs_global_bucket("abcflux_modeling_files")
gcs_list_objects("abcflux_modeling_files")
contents <- gcs_list_objects()

terraOptions(memfrac=0.9, tempdir = "/home/master/temp/") 

### 8 km predictor averages 2000-2016

# Download data from 2000 and 2016 from GC

avg_function <- function(var, month_format, divider) {
  
  if (month_format=="short") {
    months <- seq(1, 12, by=1)
  } else {
    months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  }
  
  setwd("/home/master/cloud/")
  files_to_download <- grep("*.tif", contents$name, value = TRUE)
  files_to_download2 <- files_to_download[grepl(paste(var, "2", sep="_"), files_to_download)]
  files_to_download2 <- files_to_download2[grepl("predictors_8km", files_to_download2)]
  files_to_download3 <- files_to_download2[!(grepl("predictors_1km", files_to_download2) | grepl("old", files_to_download2))]
  files_to_download3 <- files_to_download3[!(grepl(paste(var, "2017", sep="_"), files_to_download3)) ]
  files_to_download3 <- files_to_download3[!(grepl(paste(var, "2018", sep="_"), files_to_download3)) ]
  files_to_download3 <- files_to_download3[!(grepl(paste(var, "2019", sep="_"), files_to_download3)) ]
  files_to_download3 <- files_to_download3[!(grepl(paste(var, "2020", sep="_"), files_to_download3)) ]
  files_to_download3 <- files_to_download3[!(grepl(paste(var, "2021", sep="_"), files_to_download3)) ]
  files_to_download3 <- files_to_download3[!(grepl(paste(var, "2000", sep="_"), files_to_download3)) ]
  files_to_download3 <- files_to_download3[!(grepl(paste(var, "2001", sep="_"), files_to_download3)) ]
  
  map(files_to_download3, function(x) gcs_get_object(x, saveToDisk = x, overwrite = TRUE))
  
  files_to_read <- list.files("/home/master/cloud/predictors_8km", pattern=var, full.names=TRUE)
  
  for (m in months) {
    print(m)
    files_subset <- files_to_read[(grepl(paste0("_", m, ".tif"), files_to_read)) ]
    print(files_subset)
    r <- rast(files_to_read)
    r <- mean(srad_terraclimate_sites)/divider
    
    writeRaster(r, paste0("/home/master/local_outputs/predictors_8km_mean/", var, "_",  "mean_2002_2016", "_", m,  ".tif"), overwrite=TRUE)
    
  }
  
  
  # if(grepl(var, "VCF5KYR")) {
  #   
  #   r2 <- rast(files_to_read)
  #   
  #   r2 <- mean(r2)/divider
  #   
  #   writeRaster(r2, paste0("/home/master/local_outputs/predictors_8km_mean/", var, "_",  "mean_2002_2016", "_", m,  ".tif"), overwrite=TRUE)
  #   
  #   
  # }
  # 

  file.remove(files_to_download3)
  
}

# var <- "srad"
# month_format <- "short"
# divider=10


# time alt short
avg_function("srad", "short", 10)


avg_function("co2", "short", 1000)

avg_function("ndvi_gimms", "non_short", 10000)

avg_function("soiltemplevel1", "non_short", 100)

avg_function("snowcover", "short", 100)

avg_function("soilmoistlevel1", "nonshort", 100)

avg_function("tmean", "short", 10)

avg_function("vpd", "short", 1)

avg_function("tmean20yrprior_trend", "nonshort", 1000) # unclear

avg_function("snowdepth", "nonshort", 100)

avg_function("ppt", "short", 100)

avg_function("pdsi", "short", 1)

avg_function("Percent_TreeCover_VCF5KYR", "short", 1)

avg_function("Percent_NonTree_Vegetation_VCF5KYR", "short", 1)

avg_function("Percent_NonVegetated_VCF5KYR", "short", 1)

#r <- rast(list.files("/home/master/local_outputs/predictors_8km_mean/", pattern="tmean20yrprior", full.names=TRUE))



file.remove(paste0("/home/master/cloud/predictors_8km/", "vpd_", time_alt[t], ".tif"))
file.remove(paste0("/home/master/cloud/predictors_8km/", "soilmoistlevel1_", time[t], ".tif"))
file.remove(paste0("/home/master/cloud/predictors_8km/", "Percent_TreeCover_VCF5KYR_", time[t], ".tif"))
file.remove(paste0("/home/master/cloud/predictors_8km/", "Percent_NonTree_Vegetation_VCF5KYR_", time[t], ".tif"))
file.remove(paste0("/home/master/cloud/predictors_8km/", "Percent_NonVegetated_VCF5KYR_", time[t], ".tif"))
file.remove(paste0("/home/master/cloud/predictors_8km/", "tmean20yrprior_trend_", substr(time[t],1, 4), ".tif"))
file.remove(paste0("/home/master/cloud/predictors_8km/", "snowdepth_", time[t], ".tif"))
file.remove(paste0("/home/master/cloud/predictors_8km/", "ppt_", time_alt[t], ".tif"))
file.remove(paste0("/home/master/cloud/predictors_8km/", "pdsi_", time_alt[t], ".tif"))





### 1 km predictor averages