


setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv")


### GPP positive?
k <- subset(d, GPP_gC_m2>0)


#### compare ngs flux magnitudes with Natali
d2 <- subset(d, Interval<=4 | Interval>=10)

d2$daily <- d2$NEE_gC_m2/30

k <- subset(d2, select=c(Study_ID, Interval, daily))

summary(d2$daily)

library("ggplot2")

ggplot(d2) + geom_violin(aes(x=Biome, y=daily))



#### compare annual fluxes with our own prediction
# calculate annual sums
d2 <- d %>% group_by(Study_ID_Short, Meas_year, Latitude, Longitude) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2)) %>% filter(n==12) # select annual measurements # and calculate sums. there will be some NAs as some years won't have 12 obs of all fluxes
d4 <- d2 %>% group_by(Study_ID_Short, Latitude, Longitude) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE)) 

xy <- cbind(d4$Longitude,d4$Latitude)
sp <- vect(xy)
crs(sp) <- "+proj=longlat +datum=WGS84 +no_defs"


# add avg prediction
i <- "NEE_gC_m2"
km <- "1km"
m <- "gbm"
# calculate budget for the boreal and tundra
r <- rast(paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/", paste(i,"annual_sum_avg",  km, m,  "loocv", sep="_"), ".tif"))
sp2 <- project(sp, r)

extr <- extract(r, sp2)

d4$pred <- extr$mean/1000

plot(d4$NEE_gC_m2, d4$pred)
abline(0,1)

scale_min <- min(c(min(d4$pred, na.rm=TRUE), min(d4$NEE_gC_m2, na.rm=TRUE)))
scale_max <- max(c(max(d4$pred, na.rm=TRUE), max(d4$NEE_gC_m2, na.rm=TRUE)))

ggplot(d4) + geom_point(aes(x=pred, y=NEE_gC_m2)) + 
  xlim(scale_min, scale_max) + ylim(scale_min, scale_max) + geom_abline(slope = 1) + ylab("obs annual NEE")



### same with gs

# calculate gs sums
d2 <- d %>% filter(Interval<10 & Interval >4) %>% group_by(Study_ID_Short, Meas_year, Latitude, Longitude) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2)) 
d4 <- d2 %>% filter(n==5) %>% group_by(Study_ID_Short, Latitude, Longitude) %>%   summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE)) 

xy <- cbind(d4$Longitude,d4$Latitude)
sp <- vect(xy)
crs(sp) <- "+proj=longlat +datum=WGS84 +no_defs"


# add avg prediction
i <- "NEE_gC_m2"
km <- "1km"
m <- "gbm"
# calculate budget for the boreal and tundra
r <- rast(paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/", paste(i,"gs_sum_avg",  km, m,  "loocv", sep="_"), ".tif"))
sp2 <- project(sp, r)

extr <- extract(r, sp2)

d4$pred <- extr$mean/1000

plot(d4$NEE_gC_m2, d4$pred)
abline(0,1)

scale_min <- min(c(min(d4$pred, na.rm=TRUE), min(d4$NEE_gC_m2, na.rm=TRUE)))
scale_max <- max(c(max(d4$pred, na.rm=TRUE), max(d4$NEE_gC_m2, na.rm=TRUE)))

ggplot(d4) + geom_point(aes(x=pred, y=NEE_gC_m2)) + 
  xlim(scale_min, scale_max) + ylim(scale_min, scale_max) + geom_abline(slope = 1) + ylab("obs gs NEE")




### same with ngs

# calculate gs sums
d2 <- d %>% filter(Interval>=10 | Interval <=4) %>% group_by(Study_ID_Short, Meas_year, Latitude, Longitude) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2)) 
d4 <- d2 %>% filter(n==7) %>% group_by(Study_ID_Short, Latitude, Longitude) %>%   summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE)) 

xy <- cbind(d4$Longitude,d4$Latitude)
sp <- vect(xy)
crs(sp) <- "+proj=longlat +datum=WGS84 +no_defs"


# add avg prediction
i <- "NEE_gC_m2"
km <- "1km"
m <- "gbm"
# calculate budget for the boreal and tundra
r <- rast(paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/", paste(i,"ngs_sum_avg",  km, m,  "loocv", sep="_"), ".tif"))
sp2 <- project(sp, r)

extr <- extract(r, sp2)

d4$pred <- extr$mean/1000

plot(d4$NEE_gC_m2, d4$pred)
abline(0,1)

scale_min <- min(c(min(d4$pred, na.rm=TRUE), min(d4$NEE_gC_m2, na.rm=TRUE)))
scale_max <- max(c(max(d4$pred, na.rm=TRUE), max(d4$NEE_gC_m2, na.rm=TRUE)))

ggplot(d4) + geom_point(aes(x=pred, y=NEE_gC_m2)) + 
  xlim(scale_min, scale_max) + ylim(scale_min, scale_max) + geom_abline(slope = 1) + ylab("obs ngs NEE")

