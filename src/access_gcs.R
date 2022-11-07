
library(googleCloudStorageR)
library(purrr)

###### Accessing data from the cloud
my_project_id <- "top-operand-328213"
gcs_list_buckets(my_project_id)
gcs_global_bucket("abcflux_modeling_files")
gcs_list_objects("abcflux_modeling_files")
gcs_list_objects("abcflux_modeling_files/predictors_8km") # not working

setwd("/home/master/cloud") 

gcs_get_object("masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_aggregate_northpolelambert8km_tundraboreal_attfix.tif", saveToDisk = "masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_aggregate_northpolelambert8km_tundraboreal_attfix.tif",overwrite=TRUE)
gcs_get_object("predictors_8km/abovegroundbiomass.tif", saveToDisk = "predictors_8km/abovegroundbiomass.tif")
gcs_get_object("predictors_8km/Terraclimate_averages_tmean.tif", saveToDisk = "predictors_8km/Terraclimate_averages_tmean.tif")
gcs_get_object("predictors_8km/soc.tif", saveToDisk = "predictors_8km/soc.tif")
gcs_get_object("predictors_8km/roughscale.tif", saveToDisk = "predictors_8km/roughscale.tif")
gcs_get_object("predictors_8km/belowgroundbiomass.tif", saveToDisk = "predictors_8km/belowgroundbiomass.tif")
gcs_get_object("predictors_8km/bulkdensity.tif", saveToDisk = "predictors_8km/bulkdensity.tif")
gcs_get_object("predictors_8km/cti.tif", saveToDisk = "predictors_8km/cti.tif")
gcs_get_object("predictors_8km/sol_watercontent.tif", saveToDisk = "predictors_8km/sol_watercontent.tif")
gcs_get_object("predictors_8km/Terraclimate_averages_ppt.tif", saveToDisk = "predictors_8km/Terraclimate_averages_ppt.tif")
gcs_get_object("predictors_8km/ndvi_trend_19812010.tif", saveToDisk = "predictors_8km/ndvi_trend_19812010.tif")
gcs_get_object("predictors_8km/ph.tif", saveToDisk = "predictors_8km/ph.tif")
gcs_get_object("predictors_8km/tmean_trend_19601990.tif", saveToDisk = "predictors_8km/tmean_trend_19601990.tif")
gcs_get_object("predictors_8km/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif", saveToDisk = "predictors_8km/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif")
gcs_get_object("predictors_8km/Circumpolar_Thermokarst_Landscapes_TKWP.tif", saveToDisk = "predictors_8km/Circumpolar_Thermokarst_Landscapes_TKWP.tif")
gcs_get_object("predictors_8km/Circumpolar_Thermokarst_Landscapes_TKHP.tif", saveToDisk = "predictors_8km/Circumpolar_Thermokarst_Landscapes_TKHP.tif", overwrite=TRUE)
gcs_get_object("predictors_8km/forest_age.tif", saveToDisk = "predictors_8km/forest_age.tif")



contents <- gcs_list_objects()
files_to_download <- grep("*.tif", contents$name, value = TRUE)
files_to_download2 <- files_to_download[grepl("2012", files_to_download)]
files_to_download2 <- files_to_download2[grepl("predictors_8km", files_to_download2)]
files_to_download3 <- files_to_download2[!(grepl("predictors_1km", files_to_download2) | grepl("old", files_to_download2))]
map(files_to_download3, function(x) gcs_get_object(x, saveToDisk = x, overwrite = TRUE))


files_to_download <- grep("*.tif", contents$name, value = TRUE)
files_to_download2 <- files_to_download[grepl("co2", files_to_download)]
files_to_download2 <- files_to_download2[grepl("predictors_8km", files_to_download2)]
files_to_download3 <- files_to_download2[121:492]
map(files_to_download3, function(x) gcs_get_object(x, saveToDisk = x, overwrite = TRUE))

files_to_download <- grep("*.tif", contents$name, value = TRUE)
files_to_download2 <- files_to_download[grepl("vpd", files_to_download)]
files_to_download2 <- files_to_download2[grepl("predictors_8km", files_to_download2)]
files_to_download3 <- files_to_download2[121:492]
map(files_to_download3, function(x) gcs_get_object(x, saveToDisk = x, overwrite = TRUE))

files_to_download <- grep("*.tif", contents$name, value = TRUE)
files_to_download2 <- files_to_download[grepl("snowcover", files_to_download)]
files_to_download2 <- files_to_download2[grepl("predictors_8km", files_to_download2)]
files_to_download3 <- files_to_download2[479:855]
map(files_to_download3, function(x) gcs_get_object(x, saveToDisk = x, overwrite = TRUE))

files_to_download <- grep("*.csv", contents$name, value = TRUE)
files_to_download2 <- files_to_download[grepl("predictions_8km/0.1", files_to_download)]
files_to_download3 <- files_to_download2[(grepl("NEE", files_to_download2))]
files_to_download3 <- files_to_download3[97:420]
map(files_to_download3, function(x) gcs_get_object(x, saveToDisk = x, overwrite = TRUE))

# files_to_download2 <- files_to_download[grepl("predictions_8km/0.9", files_to_download)] # files not there?
# files_to_download3 <- files_to_download2[(grepl("NEE", files_to_download2))]
# files_to_download3 <- files_to_download3[97:420]
# map(files_to_download3, function(x) gcs_get_object(x, saveToDisk = x, overwrite = TRUE))



#need to take subsets to save memory
files_to_download2 <- files_to_download[grepl("predictors_8km", files_to_download)]
files_to_download3 <- files_to_download2[grepl("ndvi_gimms", files_to_download2)]
files_to_download3 <- files_to_download2[grepl("tmean", files_to_download2)]
files_to_download3 <- files_to_download3[475:964]
files_to_download3 <- files_to_download2[grepl("srad", files_to_download2)]
files_to_download3 <- files_to_download3[1:445]

files_to_download3 <- files_to_download2[grepl("soilmoist", files_to_download2)]
files_to_download3 <- files_to_download2[grepl("soiltemp", files_to_download2)]


files_to_download <- grep("*masking_summary", contents$name, value = TRUE)
files_to_download3 <- files_to_download[grepl("permaice", files_to_download)]
files_to_download3 <- files_to_download[grepl("countr", files_to_download)]
files_to_download3 <- files_to_download[grepl("Ecoregions2017_tundraboreal.", files_to_download)]
files_to_download3 <- files_to_download[grepl("/countries", files_to_download)]




map(files_to_download3, function(x) gcs_get_object(x, saveToDisk = x, overwrite = TRUE))




# flux data too

contents <- gcs_list_objects()
files_to_download <- grep("*.csv", contents$name, value = TRUE)
files_to_download2 <- files_to_download[grepl("predictions_8km/0.5", files_to_download)]

map(files_to_download2, function(x) gcs_get_object(x, saveToDisk = x, overwrite = TRUE))


gcs_get_object("cloud/flux_upscaling_data/results/final/modeldata_avg.csv", saveToDisk = "flux_upscaling_data/results/final/modeldata_avg.csv")







my_project_id <- "top-operand-328213"
gcs_list_buckets(my_project_id)
gcs_global_bucket("abcflux_modeling_files")
contents <- gcs_list_objects()
gcs_upload_set_limit(50000000L) # increasing data size limit for transferring data to google cloud
setwd("/home/master/local_outputs/")
gcs_upload(file="trends_drivers_8km", 
           name = )

files <- list.files("trends_drivers_8km", full.names=TRUE) 
map(files, function(x) gcs_upload(x, name = x))
