
library("mgcv")
library("tidyverse")
library("dplyr")
library("rethinking")

setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
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



dd <- d %>% dplyr::select(Study_ID_Short,Interval,Meas_year,NEE_gC_m2,tmean_terraclimate_sites,terra_trend_19601990,GPP_gC_m2,Reco_gC_m2) %>% na.omit()
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
### MITEN s(Meas_year, bs = "tp",by = ID,k=3) eroaa yllä olevista? Muistiinpanoissa: laskee jokaiselle oman, kategorinen muuttuja, interaktio



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
m1 <- gamm(log(ER) ~ s(Meas_year, bs = "tp",k=3) + # tp=thin plate regression splines (TPRS) smoother. by: allow each group-specific smoother to have its own smoothing parameter
             s(Interval, bs = "cc", k=12) +  # bs="cc": Cyclic cubic regression splines: start and end values constrained to match. k=amount of smoothing, ie months in this case
             s(Interval,ID,bs="fs",m=1) +  # bs="fs": factor smooth interaction. This smooth class allows a separate smooth for each level of a factor, with the same smoothing parameter for all smooths. It is an alternative to using factor by variables.
             # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability s(Meas_year,ID,bs="fs",m=1) +
             s(ID, bs="re"), # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
           data = d1)
summary(m1$gamm)
summary(m1$lme)

plot(m1$gam,pages=1)



## predict trend contributions
p  <- predict(m1$gam,  newdata = d1, type = "terms", se.fit = TRUE, level = 0) # level=0; ignore random component: https://stackoverflow.com/questions/28684645/r-lme-cannot-evaluate-groups-for-desired-levels-on-newdata

#https://stackoverflow.com/questions/68438028/troubles-predicting-fixed-effects-from-a-hierarchical-gam-in-mgcv



### MUTTA EHKÄ KIINNOSTAVINTA OLISI VAIN ENNUSTAA VOIDEN MUUTOSTA v. 1989-2020?? -> luo uusi data? jossa vain meas year
newdat <- data.frame(Meas_year=rep(seq(1989, 2020, by=1), 12), Interval=rep(seq(1, 12, by=1), 32))
p2 <- predict(m1$gam,  newdata = newdat, type = "terms", se.fit = TRUE, level = 0)
plot(Temperature - mean(Temperature) ~ Date, data = cet, type = "n",
     +      ylab = ylab, ylim = ylim)
lines(p  ~ Date, data = pdat, col = "black")

### VIELÄ SIIS EPÄSELVÄÄ: MITEN SAA PURISTETTUA SEN MUUTOKSEN??????




m1 <- gamm(log(GPP) ~ s(Meas_year, bs = "tp",k=3) + # tp=thin plate regression splines (TPRS) smoother. by: allow each group-specific smoother to have its own smoothing parameter
             s(Interval, bs = "cc", k=12) +  # bs="cc": Cyclic cubic regression splines: start and end values constrained to match. k=amount of smoothing, ie months in this case
             s(Interval,ID,bs="fs",m=1) +  # bs="fs": factor smooth interaction. m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability
             s(Meas_year,ID,bs="fs",m=1) +
             s(ID, bs="re"),  # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
           data = d2)

plot(m1$gam,pages=1)









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
