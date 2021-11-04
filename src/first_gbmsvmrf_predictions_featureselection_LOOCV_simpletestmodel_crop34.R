


########### ANNA MUSITA TÄMÄ https://stackoverflow.com/questions/25121725/error-in-predicting-raster-with-randomforest-caret-and-factor-variables

# Packages
library("caret")
library("dplyr")
library("purrr")
library("raster")
library("terra")
#library("terra", lib.loc="/mnt/data1/boreal/avirkkala/packages")

library(stringr)

terraOptions(memfrac=0.95, tempdir = "/mnt/data1/boreal/avirkkala/Temp") # testing a different style


### Data
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv")



### Response variables
resp_vars <- c("NEE_gC_m2") 

### Models
models <- c("gbm")

### Kilometers
kms <- c("1km")


### Time periods
time <- seq(as.Date("2001/01/01"), as.Date("2020/12/31"), "months")
time <- substr(time, 1, 7)
time <- sub("-", "_", sub("_", "", time, fixed=TRUE), fixed=TRUE)
time_alt <- gsub("_0", "_", time)


### Cropping extents: 4
# crops <- rbind(c(-4834843, 4834843, -4834843, 0),  
#                c(-4834843, 4834843,  0, 4834843))
# 
# 
# crops <- rbind(c(-4834843, 0, -4834843, 0),
#                c(0, 4834843, -4834843, 0),
#                c(-4834843, 0, 0, 4834843),
#                c(0, 4834843, 0, 4834843))



# split raster to pieces: https://stackoverflow.com/questions/29784829/r-raster-package-split-image-into-multiples
SplitRas <- function(raster,ppside,save,plot){
  h        <- ceiling(ncol(raster)/ppside)
  v        <- ceiling(nrow(raster)/ppside)
  agg      <- aggregate(raster,fact=c(h,v))
  agg[]    <- 1:ncell(agg)
  agg_poly <- rasterToPolygons(agg)
  names(agg_poly) <- "polis"
  r_list <- list()
  for(i in 1:ncell(agg)){
    e1          <- extent(agg_poly[agg_poly$polis==i,])
    r_list[[i]] <- crop(raster,e1)
  }
  if(save==T){
    for(i in 1:length(r_list)){
      writeRaster(r_list[[i]],filename=paste("SplitRas",i,sep=""),
                  format="GTiff",datatype="FLT4S",overwrite=TRUE)  
    }
  }
  if(plot==T){
    par(mfrow=c(ppside,ppside))
    for(i in 1:length(r_list)){
      plot(r_list[[i]],axes=F,legend=F,bty="n",box=FALSE)  
    }
  }
  return(r_list)
}

setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_1km")

esa <- raster("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_northpolelambert1km_tundraboreal_attfix.tif")

r <- SplitRas(raster=esa,ppside=2,save=TRUE,plot=FALSE)

# get extents
for (i in 1:4) {
  
  print(i)
  # i <- 1
  
  if (i==1) {
    crops <- cbind(extent(r[[i]])@xmin, extent(r[[i]])@xmax, extent(r[[i]])@ymin, extent(r[[i]])@ymax)
  } else {
    crops2 <- cbind(extent(r[[i]])@xmin, extent(r[[i]])@xmax, extent(r[[i]])@ymin, extent(r[[i]])@ymax)
    crops <- rbind(crops, crops2)
  }
}

rm(r)

library("terra")

# creating predictions in two parts: NA and Siberia
### Loop over times and load dynamic vars


print("starting to loop over the files")

for (c in 3:nrow(crops)) {
  # c <- 3
  print(c)
  
  for (km in kms) {
    
    print(km)
    # km <- "1km"
    # i <- "NEE_gC_m2"
    
    
    if (km=="1km") {
      
      ### Load static vars (only once)
      setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_1km")
      
      ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- rast("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_northpolelambert1km_tundraboreal_attfix.tif")
      ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- terra::crop(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, ext(crops[c,]))
      ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- as.data.frame(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, xy=TRUE)
      
      
      
      # merge
      # rename
      pred_rast_static <- ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged
      
      # remove NAs
      pred_rast_static <- na.omit(pred_rast_static)
      str(pred_rast_static)
      
      
      # check to convert back to raster - yep, works ok!
      # pred_rast_static_m  <- as.matrix(pred_rast_static[]) 
      # r <- rast(pred_rast_static_m[,c(1:2, 5)], type="xyz")
      # plot(r)
      
     
      # remove the individual layers - they are just taking space
      rm(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
      gc()
      
      
      print("static vars loaded")
      

      
    }
    
    for (t in 1:length(time)) {
      
      # t <- 1
      srad_terraclimate_sites <- rast(paste0("srad_", time_alt[t], ".tif"))
      plot(srad_terraclimate_sites)
      srad_terraclimate_sites
      summary(d$srad_terraclimate_sites)
      #srad_terraclimate_sites <- srad_terraclimate_sites/100  ### NO CONVERSION NEEDED?
      srad_terraclimate_sites <- crop(srad_terraclimate_sites, ext(crops[c,]))
      srad_terraclimate_sites <- as.data.frame(srad_terraclimate_sites, xy=TRUE)
      
      

      vpd_terraclimate_sites <- rast(paste0("vpd_", time_alt[t], ".tif"))
      plot(vpd_terraclimate_sites)
      vpd_terraclimate_sites
      summary(d$vpd_terraclimate_sites)
      #vpd_terraclimate_sites <- vpd_terraclimate_sites/100  ### NO CONVERSION NEEDED?
      vpd_terraclimate_sites <- crop(vpd_terraclimate_sites, ext(crops[c,]))
      vpd_terraclimate_sites <- as.data.frame(vpd_terraclimate_sites, xy=TRUE)
      
      NDVI_whittaker_constant_monthly_mean <- rast(paste0("ndvi_", time_alt[t], ".tif")) ## EI ARVOJA plotissa mutta valuesin kautta tulee??? mutta on arvoja raster-komennon kautta plotissa???
      #plot(NDVI_whittaker_constant_monthly_mean)
      NDVI_whittaker_constant_monthly_mean
      summary(d$NDVI_whittaker_constant_monthly_mean)
      NDVI_whittaker_constant_monthly_mean <- NDVI_whittaker_constant_monthly_mean/1000
      NDVI_whittaker_constant_monthly_mean <- crop(NDVI_whittaker_constant_monthly_mean, ext(crops[c,]))
      NDVI_whittaker_constant_monthly_mean <- as.data.frame(NDVI_whittaker_constant_monthly_mean, xy=TRUE)
      
      
      Snow.cover_era5_soilmoist_temp_snow <- rast(paste0("snowcover_", time[t], ".tif"))
      plot(Snow.cover_era5_soilmoist_temp_snow)
      Snow.cover_era5_soilmoist_temp_snow
      summary(d$Snow.cover_era5_soilmoist_temp_snow)
      Snow.cover_era5_soilmoist_temp_snow <- Snow.cover_era5_soilmoist_temp_snow/100
      Snow.cover_era5_soilmoist_temp_snow <- crop(Snow.cover_era5_soilmoist_temp_snow, ext(crops[c,]))
      Snow.cover_era5_soilmoist_temp_snow <- as.data.frame(Snow.cover_era5_soilmoist_temp_snow, xy=TRUE)
      
      
      
      
      
      # merge pt 1
      print("merging dynamic rasters pt 1")
      
      pred_rast_dynamic1 <- list(srad_terraclimate_sites,  vpd_terraclimate_sites, NDVI_whittaker_constant_monthly_mean, Snow.cover_era5_soilmoist_temp_snow) %>% reduce(full_join, by = c("x", "y"))
      
      names(pred_rast_dynamic1) <- c("x", "y", "srad_terraclimate_sites",  "vpd_terraclimate_sites", "NDVI_whittaker_constant_monthly_mean", "Snow.cover_era5_soilmoist_temp_snow")
      
      pred_rast_dynamic1 <- na.omit(pred_rast_dynamic1)
      str(pred_rast_dynamic1)
      
      
      # remove the individual layers - they are just taking space
      rm(srad_terraclimate_sites)
      rm(vpd_terraclimate_sites)
      gc()
      
      print("done")
      
     
      
      ### combine all
     
      pred_rast <- merge(pred_rast_static, pred_rast_dynamic1, by=c("x", "y")) # rows that have NA are skipped
      #rm(pred_rast_static) # keep this in memory because we will need it!
      rm(pred_rast_dynamic1)
      gc()
      names(pred_rast) <- c("x", "y", "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", "srad_terraclimate_sites",  "vpd_terraclimate_sites", "NDVI_whittaker_constant_monthly_mean", "Snow.cover_era5_soilmoist_temp_snow")
      pred_rast$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- factor(pred_rast$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
      ###################### TÄmä VAIN VIRHE - OLI UNOHTUNUT KOODISTA !!!!!
      print("prediction data done")
      
      
      
      for (i in resp_vars) {
        
        #i <- "NEE_gC_m2"
        
        print("looping through resp vars and models")
        print(i)
        
        
        # Loop through the models 
        
        for (m in models) {
          
          # m <- "rf"
          # m <- "gbm"
          # m <- "svm"
          print(m)
          
          # Load model files
          mod <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i,  km, m, "loocv_simpletestmodel2", sep="_"), ".rds"))
          
          # # Print the best variables
          # print("Best variables are:")
          mod$optVariables
          
          
          #pred <- predict(pred_data, mod, na.rm=TRUE, cores=7, cpkgs=c("ranger", "caret"), filename="temp_pred.tif", overwrite=TRUE)
          
          if (km=="1km") {
            
            # library("caret")
            # pred <- predict(newdata=pred_rast, object=mod, type = "response") # https://www.py4u.net/discuss/865320
            # 
            # pred <- predict.train(newdata=pred_rast, object=mod, type = "response") # https://www.py4u.net/discuss/865320
            # pred <- extractPrediction(object=mod, newdata = pred_rast[,3:5])
            
            #library("caret")
            library("randomForest") # huom. vastoin tätä ohjetta: https://stackoverflow.com/questions/38623624/usemethodpredict-no-applicable-method-for-predict-applied-to-an-object-o
            # mutta jos laittaisinkin caretin tähän niin se ei toimisi
            pred <- predict(newdata=pred_rast[, 3:7], object=mod)

            
            #multiply by 1000
            pred <- pred*1000
            
            print("prediction to dataframe done")
            
            
            #  convert back to raster
            pred_matrix  <- data.matrix(data.frame(cbind(pred_rast[,1:2], pred)))
            r <- rast(pred_matrix, type="xyz")
            terra::crs(r) <- terra::crs(esa)
            #plot(r)
            
            writeRaster(r, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km/", paste(i,  km, m, time[t],c, "loocv_simpletestmodel2",  sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')

            print(paste("1 km prediction raster done and written out for ", i, km, m, time[t], c))
            
            # # note that prediction with terra should work: https://stackoverflow.com/questions/68082827/problem-with-type-prob-argument-in-carettrain-package
            # 
            # library(terra) # https://stackoverflow.com/questions/38623624/usemethodpredict-no-applicable-method-for-predict-applied-to-an-object-o
            # pred <- predict(pred_rast, mod, na.rm=TRUE,  
            #                 filename=paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km/", paste(i,  km, m, "loocv", c, sep="_"), ".tif"), overwrite=TRUE) # cores=7, cpkgs=c("ranger", "caret"),
            # 
            # print("terra pred done")
            # # testing raster
            # library("raster")
            # #pred_rast_test <- brick(pred_rast)
            # pred <- raster::predict(brick(pred_rast), mod, na.rm=TRUE,  
            #                         filename=paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km/", paste(i,  km, m, "loocv_raster", c, sep="_"), ".tif"), overwrite=TRUE,
            #                         progress='window', fun = function(model, ...) predict(model, ...)$predictions) # cores=7, cpkgs=c("ranger", "caret"), # see this: https://github.com/imbs-hl/ranger/issues/319
            # print("raster pred done")
            # 
            # r <- terra::rast(paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km/", paste(i,  km, m, "loocv", c, sep="_"), ".tif"))*100
            # 
            # writeRaster(r, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km/", paste(i,  km, m, "loocv", c, sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
            # 
            # print("1 km prediction done for ", i, km, m, c)
            # 
            # rm(pred); rm(r)
            # gc()
          }
          
          
        } # model loop
        
        print("model loop done")
        
      } # resp var loop
      
      print("resp loop done")
      
      
    } # time period loop done
    
    print("model loop done")
    
    rm(pred_rast)
    rm(pred_rast_dynamic)
    
    
    
    
  } # km loop done
  
  print("km loop done")



  
} # crop loop done
        
        

        

  





