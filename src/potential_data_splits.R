



### Different model data extents ###


### Split to data from 2001-2019 and 1981-2017 (this will be done automatically?)


### Split GS and NGS


### Same as original, but winter NEE -> Reco and GPP==0
d2 <- d
d2$Reco_gC_m2 <- ifelse(is.na(d2$Reco_gC_m2) & d2$Season=="winter", d2$NEE_gC_m2, d2$Reco_gC_m2)
d2$GPP_gC_m2 <- ifelse(is.na(d2$GPP_gC_m2) & d2$Season=="winter", 0, d2$Reco_gC_m2)

### Winter NEE -> Reco only in the tundra?
d3 <- d
d3$Reco_gC_m2 <- ifelse(is.na(d3$Reco_gC_m2) & d3$Season=="winter" & d3$Biome=="Tundra", d3$NEE_gC_m2, d3$Reco_gC_m2)
d3$GPP_gC_m2 <- ifelse(is.na(d3$GPP_gC_m2) & d3$Season=="winter" & d3$Biome=="Tundra", 0, d3$Reco_gC_m2)

### Data filtering based on QAQC and Gapfill

hist(d$Tower_QA.QC.NEE.flag)
hist(d$Gap_perc)

d4 <- subset(d, Tower_QA.QC.NEE.flag>0.2 & Gap_perc<80 | (is.na(Tower_QA.QC.NEE.flag) & is.na(Gap_perc)))

# d4$NEE_ALT_regr <- d4$Reco_gC_m2+d4$GPP_gC_m2
# plot(d4$NEE_gC_m2, d4$NEE_ALT_regr)



### Keep rows that have NEE, GPP, and Reco

d5 <- subset(d, !(is.na(d$Reco_gC_m2)) & !(is.na(d$GPP_gC_m2)) & !(is.na(d$NEE_gC_m2)))



### Remove observations where NEE and GP-Reco don't match

d6 <- d
d6$NEE_ALT_regr <- d6$Reco_gC_m2+d6$GPP_gC_m2
plot(d6$NEE_ALT_regr, d6$NEE_gC_m2)
m1 <- lm(NEE_gC_m2 ~ NEE_ALT_regr, d6 )
residuals <- as.vector(residuals(m1))
d6$resid <- NA
length(d6$resid[!(is.na(d6$Reco_gC_m2)) & !(is.na(d6$GPP_gC_m2)) & !(is.na(d6$NEE_gC_m2))])
d6$resid[!(is.na(d6$Reco_gC_m2)) & !(is.na(d6$GPP_gC_m2)) & !(is.na(d6$NEE_gC_m2))] <- residuals
hist(d6$resid)

# Data points with a residual outside the range of ±3 times the inter-quartile range were removed. # IQR(x) = quantile(x, 3/4) - quantile(x, 1/4)
# IQR(d6$resid, na.rm=TRUE)
# more details here: https://stats.stackexchange.com/questions/175999/determine-outliers-using-iqr-or-standard-deviation
# Lower fence = Q1 - (IQR * multiplier)
# Upper fence = Q3 + (IQR * multiplier)
IQR(d6$resid, na.rm=TRUE)
quantile(d6$resid, 3/4, na.rm=TRUE)
quantile(d6$resid, 1/4, na.rm=TRUE)

d6$pot_remov <- ifelse(d6$resid<(quantile(d6$resid, 1/4, na.rm=TRUE)-(IQR(d6$resid, na.rm=TRUE)*3)) | is.na(d6$resid), "remove", "keep")
d6$pot_remov <- ifelse(d6$resid>(quantile(d6$resid, 3/4, na.rm=TRUE)+(IQR(d6$resid, na.rm=TRUE)*3)) | is.na(d6$resid), "remove", d6$pot_remov)
d6$pot_remov <- ifelse(is.na(d6$GPP_gC_m2), "keep", d6$pot_remov)

d6 %>% group_by(pot_remov) %>% summarise(n=n()) # wow, removes 961 obs

d6 <- subset(d6, pot_remov=="keep")



### Remove chamber measurements conducted during daytime only

d7 <- subset(d, Diurnal_coverage!="Day")


###

### Remove measurements conducted at the same time at the same site with both tower, chamber/diffusion ###
# i.e. these are duplicates!


### DO THIS AGAIN WITH THE FINAL FLUX TABLE!!! AND MAYBE CHANGE THE BUFFER TO 250 M or 200 M (i.e. 400-500 m)
### AND CHECK that below should be d3, not d2

# this was already ran - so just load the overlap file

# # each site to a sf object with coordinates
# sp <- st_as_sf(d2, coords=c("Longitude", "Latitude"))
# plot(sp["NEE_gC_m2"])
# sp
# 
# # add crs
# sp <- st_set_crs(sp, "+proj=longlat +datum=WGS84") 
# 
# # transform to a projected (not geographic) system, pseudo-Mercator: https://epsg.io/3857
# sp <- st_transform(sp, "+init=epsg:3857")
# plot(sp["NEE_gC_m2"])
# 
# 
# # create buffer around each site. buffer 500-2000-4000 m to each direction
# sp_buffer <- st_buffer(sp, dist=500) # this changes each point to be a polygon!
# plot(sp_buffer["Study_ID"])
# 
# 
# # what about intersect?
# overlaps_all <- data.frame(matrix(ncol =2, nrow = 0))
# colnames(overlaps_all) <- c("Study_ID", "Overlapping_IDs")
# 
# characterfunction <- function(x){paste(unique(x),  collapse=',')}
# # loop through sites
# for (i in 1:nrow(sp)) {
#   
#   print(i)
#   sp_one <- sp[i, ]
#   # 
#   sp_overlap <- st_intersects(sp_one, sp_buffer, sparse =  FALSE)
#   
#   overlaps <- as.vector(sp_overlap)
#   
#   overlapping_sites <- sp_buffer$Study_ID[overlaps==TRUE] %>% unique()
#   overlapping_sites_unique <- characterfunction(overlapping_sites)
#   
#   overlaps_one <- cbind(data.frame(Study_ID=sp_one$Study_ID, Overlapping_IDs=overlapping_sites_unique))
#   
#   overlaps_all <- rbind(overlaps_all, overlaps_one)
# }
# 
# 
# overlaps_final <- unique(overlaps_all)
# 
# overlaps <- overlaps_final %>% group_by(Study_ID) %>% summarize(Overlapping_IDs=paste(unique(Overlapping_IDs),  collapse=','))
# 
# overlaps <- subset(overlaps, Study_ID!=Overlapping_IDs)


### THESE ALSO NEED TO BE DONE AGAIN
#overlaps_full <- read.csv("E:/UniversityComputer/ANNVIRKK/documents/WHRC/Flux_database_merging_final/filtering/overlaps.csv")
overlaps <- read.csv("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/data/overlaps_cleaned.csv")


# Loop through the rows in overlaps file
# extract the different study IDs and subset the data frame based on those

library("stringr")
# remove empty characters
overlaps$Overlapping_IDs <- str_replace_all(string=overlaps$Overlapping_IDs, pattern=" ", repl="")

for (i in 1:nrow(overlaps)) {
  
  print(i)
  # i <- 1
  ids <- overlaps$Overlapping_IDs[i]
  print(ids)
  
  iddat <- sapply(strsplit(ids, ","), "[") %>% data.frame()
  names(iddat) <- "sites"
  
  iddata <- subset(d3, d3$Study_ID %in% iddat$sites)
  
  # is there temporal overlap?
  print(duplicated(iddata$Start_date))
  
  if(any(duplicated(iddata$Start_date)) ==TRUE) {
    print(unique(iddata$Veg_type2))
  }
  
  # what will the time series look like?
  if (any(!is.na(iddata$NEE_gC_m2))) {
    
    #print(ggplot(iddata) + geom_point(aes(x=as.Date(Start_date), NEE_gC_m2, col=Study_ID))) #plot(as.Date(iddata$Start_date), iddata$NEE_gC_m2)
    
    
  }
  
  # THis is for Minions
  if (any(is.na(iddata$NEE_gC_m2))) {
    
    #print(ggplot(iddata) + geom_point(aes(x=as.Date(Start_date), Rsoil_gC_m2, col=Study_ID))) #plot(as.Date(iddata$Start_date), iddata$NEE_gC_m2)
    
    
  }
  
  
}

# not many measurements from the same months, time series look OK.
# I think we could just take the mean of the average values, paste the e



######################## REMEMBER TO CHECK Diurnal_coverage SOMEWHERE HERE !!!!!!!!!!!!!!!!!!!!!! ##############################
# Now we are merging measurements that cover the whole day and don't cover the whole day!! -> not ideal
# Select the one that covers the whole day first, and if both are similar then average
# BUT NOTE - THIS ALSO INFLUENCES many other columns, e.g. Gap fill etc??





for (i in 1:nrow(overlaps)) {
  
  print(i)
  # i <- 1
  ids <- overlaps$Overlapping_IDs[i]
  print(ids)
  
  # list the sites
  iddat <- sapply(strsplit(ids, ","), "[") %>% data.frame()
  names(iddat) <- "sites"
  
  # subset the dataframe to those sites
  iddata <- subset(d3, d3$Study_ID %in% iddat$sites)
  
  
  if(any(duplicated(iddata$Start_date)) ==TRUE) {
    
    print(duplicated(iddata$Start_date))
    
    # calculate the mean
    iddata2 <- iddata %>% group_by(Start_date) %>% select_if(is.character) %>%
      summarise_all(characterfunction) 
    
    iddata3 <- iddata %>% group_by(Start_date)%>% select_if(is.numeric) %>%
      summarise_all(mean, na.rm = TRUE)
    
    # some columns have NA -> is not captured by the above commants
    iddata4 <- iddata %>% group_by(Start_date)%>% select_if(is.logical) %>%
      summarise_all(mean, na.rm = TRUE)
    
    # remove Start_date from one 
    iddata3 <- subset(iddata3, select=-c(Start_date))
    iddata4 <- subset(iddata4, select=-c(Start_date))
    
    
    # merge
    iddatafinal <- cbind(iddata2, iddata3, iddata4)
    
    which(!(names(iddata) %in% names(iddatafinal)))
    names(iddata)[which(!(names(iddata) %in% names(iddatafinal)))]
    
    
    # return the column order
    iddatafinal<-iddatafinal[names(iddata)]
    
    
    # prints
    print(paste("size of the original iddata table:", nrow(iddata)))
    print(paste("size of the  iddatafinal table after averaging:", nrow(iddatafinal)))
    
    # replace
    print(paste("size of the  flux table before merging:", nrow(d3)))
    d4 <- subset(d3, !(d3$Study_ID %in% iddat$sites))
    d3 <- rbind(d4, iddatafinal)
    
    print(paste("size of the  flux table after merging:", nrow(d3)))
    
  }
  
}


# tämä sekottaa vähän dataa, vaikeampiselkoista - esim. jos torni ja kammio sekoittuu niin näin sekoittuu myös esim. gapfill-tieto




### And different combinations of these

