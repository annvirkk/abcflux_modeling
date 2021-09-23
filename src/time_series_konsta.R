




# packages 
library("dplyr")
library("nlme")




### Add flux data
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv")

### Annual trend analysis

# calculate annual means
d2 <- d %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2),
                                                              Veg_type_Short=paste(unique(Veg_type_Short)), Biome=paste(unique(Biome)), 
                                                              EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH=paste(unique(EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH)), Country=paste(unique(Country))) %>% filter(n==12) # select annual measurements # and calculate sums. there will be some NAs as some years won't have 12 obs of all fluxes


# very simple graph
ggplot() + geom_point(data=d2, aes(x=Meas_year, y=NEE_gC_m2, col=Biome))  + geom_smooth(data=subset(d2, Biome=="Boreal"), aes(x=Meas_year, y=NEE_gC_m2), col="red", method="lm") + 
  geom_smooth(data=d2, aes(x=Meas_year, y=NEE_gC_m2), col="black", method="lm") +
  geom_smooth(data=subset(d2, Biome=="Tundra"), aes(x=Meas_year, y=NEE_gC_m2), col="blue", method="lm") 

hist(d5$NEE_gC_m2) #normally distributed!


# NEE, whole data, as an example
ggplot(d2) + geom_point(aes(x=Meas_year, y=NEE_gC_m2)) 
# add Meas_year as predictor, Study ID as a random intercept AND slope - we need the slope since the directions can be different, and then the autocorrelation structure
lme1 <- lme(NEE_gC_m2~Meas_year,random=~Meas_year|Study_ID_Short,data=subset(d2, !is.na(NEE_gC_m2)),correlation=corAR1()) 
lme1
summary(lme1)
plot(lme1, which = 1) # residuals quite nicely flat
qqnorm(lme1)

# function to do the same
nlme_function <- function(data, resp_var) {
  
  d5_nona <- subset(data, !is.na(data[[resp_var]]))
  
  # Meas year as a fixed term, Study ID as a random intercept and slope, correlation structure
  lme1 <- lme(as.formula( paste( resp_var, "~", paste(c("Meas_year"), collapse="+") ) ), random=~Meas_year|Study_ID_Short,data=d5_nona,correlation=corAR1(), control=c(maxIter=1000, msMaxIter=1000, niterEM=1000, opt='optim'))
  plot(lme1, which = 1) # residuals quite nicely flat
  qqnorm(lme1)
  lme1
  fixed_effects <- summary(lme1)$tTable
  
}

nee_all <- nlme_function(d2, "NEE_gC_m2")
gpp_all <- nlme_function(d2, "GPP_gC_m2") # error.  this helps: lme1 <- lme(as.formula( paste( resp_var, "~", paste(c("Meas_year"), collapse="+") ) ), random=~Meas_year|Study_ID_Short,data=d5_nona)
reco_all <- nlme_function(d2, "Reco_gC_m2") # same error. 


# same but without the autocorrelation structure which is sometimes too much for the model to handle
nlme_function_nocorar <- function(data, resp_var) {
  
  d5_nona <- subset(data, !is.na(data[[resp_var]]))
  
  # Meas year as a fixed term, Study ID as a random intercept and slope, correlation structure
  lme1 <- lme(as.formula( paste( resp_var, "~", paste(c("Meas_year"), collapse="+") ) ), random=~Meas_year|Study_ID_Short,data=d5_nona, control=c(maxIter=1000, msMaxIter=1000, niterEM=1000, opt='optim'))
  plot(lme1, which = 1) # residuals quite nicely flat
  qqnorm(lme1)
  lme1
  fixed_effects <- summary(lme1)$tTable
  
}

nee_all <- nlme_function_nocorar(d2, "NEE_gC_m2")
gpp_all <- nlme_function_nocorar(d2, "GPP_gC_m2") 
reco_all <- nlme_function_nocorar(d2, "Reco_gC_m2")  # error??



# function to do the same across different levels of a factor variables, no corar (create separate models for tundra, boreal etc...)
nlme_function_category <- function(data, resp_var, category) {
  
  d5_nona <- subset(data, !is.na(data[[resp_var]]))
  
  categorylevels <- levels(factor(data[[category]]))
  
  fixed_effects_all <- NA
  
  for (i in categorylevels) {
    
    # i <- "spring"
    d5_nona2 <- subset(d5_nona, d5_nona[[category]]==i)
    
    # Meas year as a fixed term, Study ID as a random intercept and slope, correlation structure
    lme1 <- lme(as.formula( paste( resp_var, "~", paste(c("Meas_year"), collapse="+") ) ), random=~Meas_year|Study_ID_Short,data=d5_nona2, control=c(maxIter=1000, msMaxIter=1000, niterEM=1000, opt='optim'))
    plot(lme1, which = 1) # residuals quite nicely flat
    qqnorm(lme1)
    lme1
    fixed_effects <- summary(lme1)$tTable
    fixed_effects_all <- rbind(fixed_effects_all, cbind(fixed_effects, "category"=i))
    
  }
  
  return(fixed_effects_all)
  
}


# before applying, check that there are enough years for vegetation types!
dd <- subset(d2, !is.na(NEE_gC_m2))
dd %>% group_by(Veg_type_Short) %>% summarize(n=n(), sites=length(unique(Study_ID_Short)))
ggplot(d2) + geom_point(aes(x=Meas_year, y=NEE_gC_m2)) + facet_wrap(~Veg_type_Short)

d22 <- subset(d2, Veg_type_Short=="BW" | Veg_type_Short=="EN" | Veg_type_Short=="G" | Veg_type_Short=="W")
test <- nlme_function_category(d22, "NEE_gC_m2", "Veg_type_Short")

# biome
ggplot(d2) + geom_point(aes(x=Meas_year, y=NEE_gC_m2)) + facet_wrap(~Biome)

test <- nlme_function_category(d2, "NEE_gC_m2", "Biome")
test <- nlme_function_category(d2, "GPP_gC_m2", "Biome")
test <- nlme_function_category(d2, "Reco_gC_m2", "Biome")

ggplot(subset(d2, Biome=="Boreal")) + geom_point(aes(x=Meas_year, y=NEE_gC_m2)) + geom_abline(slope=-2.57, intercept=5132) + 
  theme_pub + ylab(expression(paste("NEE g C m"^{-2}, yr^{-1}))) + xlab("Measurement year")

ggplot(subset(d2, Biome=="Tundra")) + geom_point(aes(x=Meas_year, y=NEE_gC_m2)) + geom_abline(slope=0.42, intercept=-836) + 
  theme_pub + ylab(expression(paste("NEE g C m"^{-2}, yr^{-1})))  + xlab("Measurement year")

# permafrost
test <- nlme_function_category(d2, "NEE_gC_m2", "EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH")

# Region
test <- nlme_function_category(d2, "NEE_gC_m2", "Country")

# season
d22 <- d %>% group_by(Study_ID_Short, Meas_year, Season) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2),
                                                               Veg_type_Short=paste(unique(Veg_type_Short)), Biome=paste(unique(Biome)), Season=paste(unique(Season))) %>% filter(n==3) 
ggplot(d22) + geom_point(aes(x=Meas_year, y=Reco_gC_m2)) + facet_wrap(~Season)

test <- nlme_function_category(d22, "NEE_gC_m2", "Season")
test <- nlme_function_category(d22, "Reco_gC_m2", "Season")
test <- nlme_function_category(d22, "GPP_gC_m2", "Season")


# season+biome
d22$Season_Biome <- paste(d22$Season, d22$Biome, sep="_")

ggplot(d22) + geom_point(aes(x=Meas_year, y=GPP_gC_m2)) + facet_wrap(~Season_Biome)

test <- nlme_function_category(d, "NEE_gC_m2", "Season_Biome")

# season-vegetation type
d22 <- subset(d22, Veg_type_Short=="BW" | Veg_type_Short=="EN" | Veg_type_Short=="G" | Veg_type_Short=="W")
d22$Season_Veg_type_Short <- paste(d22$Season, d22$Veg_type_Short, sep="_")

ggplot(d22) + geom_point(aes(x=Meas_year, y=GPP_gC_m2)) + facet_wrap(~Season_Veg_type_Short)

test <- nlme_function_category(d22, "NEE_gC_m2", "Season_Veg_type_Short")

