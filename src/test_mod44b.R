
library("raster")

library("terra")


print("test 1")


setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_1km")

time <- seq(as.Date("2001/01/01"), as.Date("2020/12/31"), "months")
time <- substr(time, 1, 7)
time <- sub("-", "_", sub("_", "", time, fixed=TRUE), fixed=TRUE)

t <- 1

Percent_Tree_Cover_MOD44B_sites <- raster::stack(paste0("Percent_Tree_Cover_mod44b", substr(time[t],1, 4), ".tif")) # THE FIRST LAYER!!!!
print(Percent_Tree_Cover_MOD44B_sites)
Percent_Tree_Cover_MOD44B_sites <- rast(Percent_Tree_Cover_MOD44B_sites[[1]])
print(Percent_Tree_Cover_MOD44B_sites)
plot(Percent_Tree_Cover_MOD44B_sites)
Percent_Tree_Cover_MOD44B_sites




print("test 2")

Percent_Tree_Cover_MOD44B_sites <- raster::stack(paste0("Percent_Tree_Cover_mod44b", substr(time[t],1, 4), ".tif")) # THE FIRST LAYER!!!!
print(Percent_Tree_Cover_MOD44B_sites)
Percent_Tree_Cover_MOD44B_sites <- Percent_Tree_Cover_MOD44B_sites[[1]]
Percent_Tree_Cover_MOD44B_sites <- rast(Percent_Tree_Cover_MOD44B_sites)
Percent_Tree_Cover_MOD44B_sites <- Percent_Tree_Cover_MOD44B_sites[[1]]
print(Percent_Tree_Cover_MOD44B_sites)
plot(Percent_Tree_Cover_MOD44B_sites)
Percent_Tree_Cover_MOD44B_sites




print("test 3")

Percent_Tree_Cover_MOD44B_sites <- raster::stack(paste0("Percent_Tree_Cover_mod44b", substr(time[t],1, 4), ".tif")) # THE FIRST LAYER!!!!
print(Percent_Tree_Cover_MOD44B_sites)
Percent_Tree_Cover_MOD44B_sites <- Percent_Tree_Cover_MOD44B_sites[[1]]
Percent_Tree_Cover_MOD44B_sites <- rast(Percent_Tree_Cover_MOD44B_sites)
Percent_Tree_Cover_MOD44B_sites <- subset(Percent_Tree_Cover_MOD44B_sites, 1)
print(Percent_Tree_Cover_MOD44B_sites)
plot(Percent_Tree_Cover_MOD44B_sites)
Percent_Tree_Cover_MOD44B_sites

