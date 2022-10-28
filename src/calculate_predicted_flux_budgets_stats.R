
#install.packages("terra", dependencies=TRUE)
library(terra)
#install.packages("zyp", dependencies=TRUE)

library(zyp)
#install.packages("dplyr", dependencies=TRUE)
library(dplyr)
#install.packages("stringi", dependencies=TRUE)
library(stringi)
#install.packages("stringr", dependencies=TRUE)
library(stringr)
library("sf")

### calculate budgets for each year
# separately for each month, climatological season, and full year
# note that for the climatological season (DJF, MAm, "_trend", JJA, SON) and full year budgets, we'll need to aggregate the monthly predictions to cumulative fluxes for each pixel first (i.e. sum of pixel values from the same year)
# winter season always includes the December value from the past year and Jan & Feb from the current year
# budget will be calculated by summing all the pixels in the output together


# load an example flux raster for reprojecting
r <- rast(list.files("/home/master/local_outputs/predictions_8km/raster/0.5", full.names=TRUE)[1])

# download zonal rasters or vectors
# biome
biome <- vect("/home/master/cloud/masking_summary_rasters/Ecoregions2017_tundraboreal.shp")
biome <- project(biome, r)
biome <- aggregate(biome, by='BIOME_NUM')
# biome <- biome[[c("BIOME_NUM")]]
# subset(v, v$NAME_1 == "Diekirch", c("NAME_1", "NAME_2"))

# country
country <- vect("/home/master/cloud/masking_summary_rasters/ne_10m_admin_0_countries.shp")
sf::st_buffer(country, dist = 0)
country <- crop(country, biome)

# biome-country
bc <- intersect(biome, country)
# vegetation type
veg <- rast("/home/master/cloud/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_aggregate_northpolelambert8km_tundraboreal_attfix.tif")


# vegetation type-country

# permafrost
perma <- vect("/home/master/cloud/masking_summary_rasters/permaice.shp")





setwd("/home/master/local_outputs/predictions_8km/raster/0.5/")
files <- list.files("/home/master/local_outputs/predictions_8km/raster/0.5", full.names=TRUE)

# only files that have a length of X or end with sum already
files2 <- files[nchar(files)==nchar("/home/master/local_outputs/predictions_8km/raster/0.5/NEE_gC_m2_08_1992.tif") | str_detect(files, "sum")]


budgets <- data.frame(matrix(ncol = 3, nrow = 0))

for (f in files2) {
  
  
  print(f)
  ### load raster
  r <- rast(f)
  
  
  # budgets
  rarea <- r*(7995.296*7995.296)
  sumr <- extract(rarea, biome, fun="sum", na.rm=TRUE)
  budget <- (sumr/1000)/1000000000000
  
  budget_df <- (data.frame(filename=str_remove(f, "^[^|]+\\/"),
                biome=c("Boreal", "Tundra"),
                budget=budget[,2]))
  
  budgets <- rbind(budgets, budget_df)

  
}

write.csv(budgets, "/home/master/local_outputs/table_summaries/NEE_budgets_8km.csv", row.names=FALSE)

budgets$year <- ifelse(is.na(substr(budgets$filename, 14, 17) %>% as.numeric()), str_sub(budgets$filename, start=-12, end=-9), substr(budgets$filename, 14, 17)) %>% as.numeric()
budgets$temp <- ifelse(nchar(budgets$filename) ==21, "monthly", NA)
budgets$temp <- ifelse(nchar(budgets$filename) ==22, "annual", budgets$temp)

library(ggplot2)
ggplot(subset(budgets, biome=="Tundra" & temp=="annual")) + geom_line(aes(x=year, y=budget/10)) + ylab("Tundra NEE Tg C yr-1")
ggplot(subset(budgets, biome=="Boreal" & temp=="annual")) + geom_line(aes(x=year, y=budget/10))  + ylab("Boreal NEE Tg C yr-1")



# average fluxes
meanr <- extract(r, biome, fun="mean", na.rm=TRUE)
meanflux <- (meanr/100)

# same for trends

### calculate average and standard deviations of fluxes
# calculated for the same temporal periods as the budgets, but this time the final step just calculates an average flux and its st deve, not the sum of the fluxes



