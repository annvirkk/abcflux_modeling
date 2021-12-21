
library("mgcv")
library("tidyverse")
library("dplyr")
library("rethinking")

setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
setwd("D:/repos/flux_upscaling_data/src/")

d <- read.csv("../results/final/modeldata_avg.csv")


# random testing
k <- subset(d, Study_ID_Short=="Vesala_FI-Hyy_tower1")
k$Tsoil_C


m <- gam(Tsoil_C~ s(Interval, bs = "cc", k=12) + s(Meas_year, bs = "tp",k=3) , data=k)
# could also just try: m <- gam(Tsoil_C~ s(Interval, bs = "cc", k=12), data=k)

plot(m)

pred <- predict(m, k)
plot(pred, k$Tsoil_C)
abline(0,1)





# random checks

# keep only sites that have at least 5 measurement months per year
year_site_keep <- d %>% filter(!is.na(NEE_gC_m2)) %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n()) %>% filter(n==12)
d1 <- subset(d, d$Study_ID_Short %in% year_site_keep$Study_ID_Short & d$Meas_year %in% year_site_keep$Meas_year & !is.na(d$NEE_gC_m2))

# then keep only sites that have at least 3 measurement years
year_site_keep2 <- d1  %>% group_by(Study_ID_Short) %>% summarize(nyears =n_distinct(Meas_year)) %>% filter(nyears>=3)
d2 <- subset(d1, d1$Study_ID_Short %in% year_site_keep2$Study_ID_Short)
d2$Study_ID_Short <- factor(d2$Study_ID_Short)


d4 <- subset(d2, Study_ID_Short=="Goulden_CA-Oas_tower1")
m <- lm(NEE_gC_m2 ~ Meas_year, data = d4)


library("lme4")
fits <- lmList(NEE_gC_m2 ~ Meas_year | Study_ID_Short, data = d2)
dd <- coefficients(fits) %>% as.data.frame()
dd$Study_ID_Short <- rownames(dd)

simple <- d2 %>% select(Study_ID_Short, Biome, Veg_type_Short) %>% unique()
dd2 <- merge(dd, simple, by="Study_ID_Short")

dd2 %>% group_by(Biome) %>% count()
dd2 %>% group_by(Biome) %>% filter(Meas_year>0) %>% count()
9/13
12/34



d3 <- d2 %>% group_by(Study_ID_Short) %>% do(model = lm(NEE_gC_m2 ~ Meas_year, data = .))


library(broom)
d3 %>% tidy(model)


d <- data.frame(state=rep(c('NY', 'CA'), c(10, 10)),
                year=rep(1:10, 2),
                response=c(rnorm(10), rnorm(10)))
fitted_models = d %>% group_by(state) %>% do(model = lm(response ~ year, data = .))

# start here



dd <- d %>% dplyr::select(Study_ID_Short,Interval,Meas_year,NEE_gC_m2,tmean_terraclimate_sites,terra_trend_19601990,GPP_gC_m2,Reco_gC_m2) %>% na.omit()
dd$Study_ID_Short <- as.factor(dd$Study_ID_Short)
dd <- dd %>% mutate(id = as.numeric(Study_ID_Short))
dd %>%
  filter(id %in% 1:30) %>%
  ggplot(aes(Meas_year,NEE_gC_m2)) +
  geom_col(aes(color=factor(Interval, levels = 1:12, ordered = TRUE)), position = "dodge") +
  facet_wrap(~id, scales = "free") +
  theme(legend.position = "none")
dd %>% ggplot(aes(Meas_year,NEE_gC_m2)) + geom_point()



d1 <- dd %>% filter(id %in% c(9,12,13,29))

d1 %>% ggplot(aes(Meas_year,GPP_gC_m2)) +
  geom_col(aes(fill=factor(Interval, levels = 1:12, ordered = TRUE)), position = "dodge") +
  facet_wrap(~id, scales = "free") +
  theme(legend.position = "none")
d1 %>% ggplot(aes(Meas_year,Reco_gC_m2)) +
  geom_col(aes(fill=factor(Interval, levels = 1:12, ordered = TRUE)), position = "dodge") +
  facet_wrap(~id, scales = "free") +
  theme(legend.position = "none")

d1$ID <- factor(d1$id)

d1 <- d1 %>% mutate(GPP = -GPP_gC_m2, ER = Reco_gC_m2)
d1 <- d1 %>% mutate(GPP = case_when(GPP<=0 ~ 0.3,TRUE ~ GPP),
                    ER = case_when(ER<=0 ~ 2.9,TRUE ~ ER))


### Example by Konsta that answers the following questions: 
# 1) how does Reco increase annually over time at individual sites?
# 2) what does interannual variability across sites look like?
# 3) how much does an increase in tmean increase Reco?
m1 <- gamm(log(ER) ~ s(Meas_year, bs = "tp",by = ID,k=3) + # tp=thin plate regression splines (TPRS) smoother. by: allow each group-specific smoother to have its own smoothing parameter
             s(Interval, bs = "cc", k=12) +  # bs="cc": Cyclic cubic regression splines: start and end values constrained to match. k=amount of smoothing, ie months in this case
             s(Interval,ID,bs="fs",m=1) +  # bs="fs": factor smooth interaction. This smooth class allows a separate smooth for each level of a factor, with the same smoothing parameter for all smooths. It is an alternative to using factor by variables.
             # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability
             s(ID, bs="re") + # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
             s(tmean_terraclimate_sites),
           #correlation = corARMA(form = ~ 1|ID, p = 1),
           data = d1)

m1$gam
summary(m1$gam)
m1
plot(m1$gam,pages=1)
plot(m1$gam,select = 6, ylim = c(-3, 3))
acf(resid(m1$lme))
pacf(resid(m1$lme))


# We  explicitly include a random effect for the intercept (the bs="re" term), as
# group-specific intercepts are not incorporated into factor by variable smoothers
# (as would be the case with a factor smoother or a tensor product random effect).

### MITEN  s(Interval,ID,bs="fs",m=1) eroaa te(Interval, ID)? Molemmat jotenkin interaction with smoothers?
### MITEN s(Meas_year, bs = "tp",by = ID,k=3) eroaa yll√§ olevista? Muistiinpanoissa: laskee jokaiselle oman, kategorinen muuttuja, interaktio



# Anna: test what happens without 
m1 <- gamm(log(ER) ~ s(Meas_year, bs = "tp",by = ID,k=3) + # tp=thin plate regression splines (TPRS) smoother. by: allow each group-specific smoother to have its own smoothing parameter
             s(Interval, bs = "cc", k=12) +  # bs="cc": Cyclic cubic regression splines: start and end values constrained to match. k=amount of smoothing, ie months in this case
             s(Interval,ID,bs="fs",m=1) +  # bs="fs": factor smooth interaction. This smooth class allows a separate smooth for each level of a factor, with the same smoothing parameter for all smooths. It is an alternative to using factor by variables.
             # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability
             s(ID, bs="re") + # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
             s(tmean_terraclimate_sites),
           #correlation = corARMA(form = ~ 1|ID, p = 1),
           data = d1)





### Anna modifying the model to understand how much Reco is increasing over time when controlling for interannual variability 
# For this model, we want to include
# 1) A single common smoother for meas year - Reco
# 2) Group-level smoothers for year-Reco with the same wiggliness (no need to allow for very variable wiggliness in this case)
# 3) A single common smoother for month - Reco
# 4) Group-level smoothers for month-Reco with the same wiggliness
# 5) Random effect smoother, i.e. the random intercept
m1 <- gam(log(ER) ~ s(Meas_year, bs = "tp",k=3) + # tp=thin plate regression splines (TPRS) smoother. 
                           s(Meas_year,ID,bs="fs",m=1) + # smoother shapes differ depending on ID. Each group has the same wiggliness.
                           s(Interval, bs = "cc", k=12) +  # bs="cc": Cyclic cubic regression splines: start and end values constrained to match. k=amount of smoothing, ie months in this case
                           s(Interval,ID,bs="fs",xt=list(bs="cc"), m=1) +  # bs="fs": factor smooth interaction. This smooth class allows a separate smooth for each level of a factor, with the same smoothing parameter for all smooths. It is an alternative to using factor by variables.
                           # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability s(Meas_year,ID,bs="fs",m=1) +
                           s(ID, bs="re"), # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
                       data = d1) # s(Interval,ID,bs="fs",xt=list(bs="cc"), m=1) adding xt was important for group-level effects

plot(m1)
summary(m1)

# if you use gamm instead of gam then the codes below work:
m1 # intercept 3, meas year global effect 0.274?
summary(m1$gamm)
summary(m1$lme)
coef(m1$gam)
coef(m1$lme)
plot(m1$gam,pages=1)
plot(m1$lme,pages=1)



# Predict using the global smoother only
# new data frame

years <- data.frame(Meas_year=rep(seq(1989, 2020, by=1), 12*4), Interval=rep(rep(1:12, each=32), 4), ID=rep(rep(unique(d1$ID), each=32*12), 4)) 
# years <- data.frame(Meas_year=rep(seq(1989, 2020, by=1), 12), Interval=rep(1:12, each=32)) 
# yearsd <- data.frame(rbind(data.frame(cbind(years$Meas_year, years$Interval, years$ID=))))

p <- predict(m1, years, se.fit=TRUE, type="terms") # include only the two basic splines
years$pred <- p$fit
years$se <- p$se.fit


# Calculate % change
years2 <- years %>% group_by(ID, Meas_year) %>% summarize(mean=mean(pred)) ##  sum of log is the same as multiplication, don't calculate that

years3 <- years2 %>% group_by(ID) %>% mutate(first_flux=mean[Meas_year==1989], last_flux=mean[Meas_year==2020]) %>% mutate (change = last_flux-first_flux) 

# log change is already percentage change: https://stats.stackexchange.com/questions/244199/why-is-it-that-natural-log-changes-are-percentage-changes-what-is-about-logs-th
mean(years3$change) # Reco decreased by 36 % over the 32-year study period, i.e. 36/32=1 % per year

# Calculate flux change by using intercept (exp) as an average flux
exp(coef(m1)[1]) # Reco in the beginning of the period
exp(coef(m1)[1])*(1-0.36)
# so flux decreased by 20 g C m-2 over the study period




### Try the same without Interval
m1 <- gam(log(ER) ~ s(Meas_year, bs = "tp",k=3) + # tp=thin plate regression splines (TPRS) smoother. 
            s(Meas_year,ID,bs="fs",m=1) + # smoother shapes differ depending on ID. Each group has the same wiggliness.
            # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability s(Meas_year,ID,bs="fs",m=1) +
            s(ID, bs="re"), # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
          data = d1) # s(Interval,ID,bs="fs",xt=list(bs="cc"), m=1) adding xt was important for group-level effects

plot(m1) # losing the trend here...
summary(m1) 



# Try the same with annual sum - need more data for this
d2 <- d %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n(), NEEsum=sum(NEE_gC_m2) , Recosum=sum(Reco_gC_m2)) %>% filter(n==12) 
d2$Study_ID_Short <- as.factor(d2$Study_ID_Short)
m1 <- gam(NEEsum ~ s(Meas_year, bs = "tp",k=3) + # tp=thin plate regression splines (TPRS) smoother. 
            s(Meas_year,Study_ID_Short,bs="fs",m=1) + # smoother shapes differ depending on ID. Each group has the same wiggliness.
            # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability s(Meas_year,ID,bs="fs",m=1) +
            s(Study_ID_Short, bs="re"), # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
          data = d2) # s(Interval,ID,bs="fs",xt=list(bs="cc"), m=1) adding xt was important for group-level effects

plot(m1) # losing the trend here...
summary(m1) 

### Add complexity: biome information to the model 
m1 <- gam(log(ER) ~ s(Meas_year, bs = "tp",k=3) + # tp=thin plate regression splines (TPRS) smoother. 
            s(Meas_year,ID,bs="fs",m=1) + # smoother shapes differ depending on ID. Each group has the same wiggliness.
            s(Interval, bs = "cc", k=12) +  # bs="cc": Cyclic cubic regression splines: start and end values constrained to match. k=amount of smoothing, ie months in this case
            s(Interval,ID,bs="fs",xt=list(bs="cc"), m=1) +  # bs="fs": factor smooth interaction. This smooth class allows a separate smooth for each level of a factor, with the same smoothing parameter for all smooths. It is an alternative to using factor by variables.
            # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability s(Meas_year,ID,bs="fs",m=1) +
            s(ID, bs="re"), # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
          data = d1) # s(Interval,ID,bs="fs",xt=list(bs="cc"), m=1) adding xt was important for group-level effects

plot(m1)
summary(m1)








### Test the model with NEE at first

# keep only sites that have at least 5 measurement months per year
year_site_keep <- d %>% filter(!is.na(NEE_gC_m2)) %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n()) %>% filter(n>=5)
d1 <- subset(d, d$Study_ID_Short %in% year_site_keep$Study_ID_Short & d$Meas_year %in% year_site_keep$Meas_year & !is.na(d$NEE_gC_m2))

# then keep only sites that have at least 3 measurement years
year_site_keep2 <- d1  %>% group_by(Study_ID_Short) %>% summarize(nyears =n_distinct(Meas_year)) %>% filter(nyears>=3)
d2 <- subset(d1, d1$Study_ID_Short %in% year_site_keep2$Study_ID_Short)
d2$Study_ID_Short <- factor(d2$Study_ID_Short)

m1 <- gam(NEE_gC_m2 ~ s(Meas_year, bs = "tp",k=3) + # tp=thin plate regression splines (TPRS) smoother. 
            s(Meas_year,Study_ID_Short,bs="fs",m=1) + # smoother shapes differ depending on ID. Each group has the same wiggliness.
            s(Interval, bs = "cc", k=12) +  # bs="cc": Cyclic cubic regression splines: start and end values constrained to match. k=amount of smoothing, ie months in this case
            s(Interval,Study_ID_Short,bs="fs",xt=list(bs="cc"), m=1) +  # bs="fs": factor smooth interaction. This smooth class allows a separate smooth for each level of a factor, with the same smoothing parameter for all smooths. It is an alternative to using factor by variables.
            # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability s(Meas_year,ID,bs="fs",m=1) +
            s(Study_ID_Short, bs="re"), # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
          data = d2) # s(Interval,ID,bs="fs",xt=list(bs="cc"), m=1) adding xt was important for group-level effects

plot(m1)
summary(m1)

years <- data.frame(Meas_year=rep(seq(1989, 2020, by=1), 12*77), Interval=rep(rep(1:12, each=32), 77), Study_ID_Short=rep(rep(unique(d2$Study_ID_Short), each=32*12), 77)) 
# years <- data.frame(Meas_year=rep(seq(1989, 2020, by=1), 12), Interval=rep(1:12, each=32)) 
# yearsd <- data.frame(rbind(data.frame(cbind(years$Meas_year, years$Interval, years$ID=))))

p <- predict(m1, years, se.fit=TRUE, type="terms") # include only the two basic splines
years$pred <- p$fit
years$se <- p$se.fit

years2 <- years %>% group_by(Study_ID_Short, Meas_year) %>% summarize(sum=sum(pred))
plot(years2$Meas_year, years2$sum)



#years3$change = (years3$last_flux-years3$first_flux)/years3$first_flux
mean(years3$change) # Reco decreased by 1.5 % over the 32-year study period

[18.51, 30.11.2021] Konsta: Sit jos haluat lis‰ill‰ splinej‰ toisiinsa (esim vuosi + kuukausi), niin olettamalla splinit kesken‰‰n korreloimattomiksi, niiden SE on sqrt(se_vuosi^2 + se_kuukausi^2)
[18.51, 30.11.2021] Konsta: eli siis summan varianssi on varianssien summa.

# p <- predict(m1, years, se.fit=TRUE, terms = c("s(Meas_year)", "s(Interval)")) # include only the two basic splines
# p <- predict(m1$lme, years, re.form=NA) #re.form=NA should set all random effects to zero
# p <- predict(m1, years, exclude = c("s(Meas_year,ID)", "s(Interval, ID)"), re.form=NA) # exclude the factor smooth interactions AND random effects

#devtools::install_github("m-clark/gammit")
library("gammit")
p <- predict_gamm(m1$gam, years, se.fit=TRUE, terms = c("s(Meas_year)"))
p <- predict_gamm(m1$gam, years, se.fit=TRUE, re.form=NA)

# some useful discussions
# https://stackoverflow.com/questions/68438028/troubles-predicting-fixed-effects-from-a-hierarchical-gam-in-mgcv
# https://stats.stackexchange.com/questions/189384/predicting-mean-smooth-in-gam-with-smooth-by-random-factor-interaction


## predict trend contributions
p  <- predict(m1$gam,  newdata = d1, type = "terms", se.fit = TRUE, level = 0) # level=0; ignore random component: https://stackoverflow.com/questions/28684645/r-lme-cannot-evaluate-groups-for-desired-levels-on-newdata
p  <- predict(m1$lme,  newdata = d1, type = "terms", se.fit = TRUE, level = 0) # level=0; ignore random component: https://stackoverflow.com/questions/28684645/r-lme-cannot-evaluate-groups-for-desired-levels-on-newdata





### MUTTA EHK√Ñ KIINNOSTAVINTA OLISI VAIN ENNUSTAA VOIDEN MUUTOSTA v. 1989-2020?? -> luo uusi data? jossa vain meas year
newdat <- data.frame(Meas_year=rep(seq(1989, 2020, by=1), 12), Interval=rep(seq(1, 12, by=1), 32))
p2 <- mgcv::predict(m1$gam,  newdata = newdat, type = "terms", se.fit = TRUE, level = 0, exclude = "s(ID)")
plot(Temperature - mean(Temperature) ~ Date, data = cet, type = "n",
     +      ylab = ylab, ylim = ylim)
lines(p  ~ Date, data = pdat, col = "black")

### VIEL√Ñ SIIS EP√ÑSELV√Ñ√Ñ: MITEN SAA PURISTETTUA SEN MUUTOKSEN??????




m1 <- gamm(log(GPP) ~ s(Meas_year, bs = "tp",k=3) + # tp=thin plate regression splines (TPRS) smoother. by: allow each group-specific smoother to have its own smoothing parameter
             s(Interval, bs = "cc", k=12) +  # bs="cc": Cyclic cubic regression splines: start and end values constrained to match. k=amount of smoothing, ie months in this case
             s(Interval,ID,bs="fs",m=1) +  # bs="fs": factor smooth interaction. m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability
             s(Meas_year,ID,bs="fs",m=1) +
             s(ID, bs="re"),  # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
           data = d2)

plot(m1$gam,pages=1)







# # this could be one option too
# m1 <- gamm(log(ER) ~ s(Meas_year, bs = "tp",k=3, m=2) + # tp=thin plate regression splines (TPRS) smoother. Global meas year-ER relationship, i.e. a single common smoother for all obs
#              s(Meas_year, by=ID, bs = "tp",k=3, m=1) + # group-level smoothers in addition to the global one. But maybe we don't need this?
#              s(Interval, bs = "cc", k=12) +  # bs="cc": Cyclic cubic regression splines: start and end values constrained to match. k=amount of smoothing, ie months in this case
#              s(Interval,ID,bs="fs",m=1) +  # bs="fs": factor smooth interaction. This smooth class allows a separate smooth for each level of a factor, with the same smoothing parameter for all smooths. It is an alternative to using factor by variables.
#              # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability s(Meas_year,ID,bs="fs",m=1) +
#              s(ID, bs="re"), # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
#            data = d1)


# ### What have I tried so far?
# ### try out with whole data set!
# d2 <- dd %>% mutate(GPP = -GPP_gC_m2, ER = Reco_gC_m2)
# d2 <- d2 %>% mutate(GPP = case_when(GPP<=0 ~ 0.3,TRUE ~ GPP),
#                     ER = case_when(ER<=0 ~ 2.9,TRUE ~ ER))
# d2$ID <- factor(d2$id)
# m1 <- gamm(log(ER) ~ s(Meas_year, bs = "tp",k=3) + # tp=thin plate regression splines (TPRS) smoother. by: allow each group-specific smoother to have its own smoothing parameter
#              s(Interval, bs = "cc", k=12) +  # bs="cc": Cyclic cubic regression splines: start and end values constrained to match. k=amount of smoothing, ie months in this case
#              s(Interval,ID,bs="fs",m=1) +  # bs="fs": factor smooth interaction. This smooth class allows a separate smooth for each level of a factor, with the same smoothing parameter for all smooths. It is an alternative to using factor by variables.
#              # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stabilitys(Meas_year,ID,bs="fs",m=1) +
#              s(ID, bs="re"),  # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
#            data = d2)
# summary(m1$gamm)
# summary(m1$lme)
# 
# plot(m1$gam,pages=1)
# 
# 
# 
# m1 <- gamm(log(GPP) ~ s(Meas_year, bs = "tp",k=3) + # tp=thin plate regression splines (TPRS) smoother. by: allow each group-specific smoother to have its own smoothing parameter
#              s(Interval, bs = "cc", k=12) +  # bs="cc": Cyclic cubic regression splines: start and end values constrained to match. k=amount of smoothing, ie months in this case
#              s(Interval,ID,bs="fs",m=1) +  # bs="fs": factor smooth interaction. This smooth class allows a separate smooth for each level of a factor, with the same smoothing parameter for all smooths. It is an alternative to using factor by variables.
#              # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability s(Meas_year,ID,bs="fs",m=1) +
#              s(ID, bs="re"),  # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
#            data = d2)
# 
# plot(m1$gam,pages=1)
# 
