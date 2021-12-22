


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
