


# Add data
setwd("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/src/")
d <- read.csv("../results/final/modeldata_avg.csv")


# Define response variable
i <- "GPP_gC_m2"


### Linear model tests
# just for fun: test how a linear model with year, month and veg type as factors would work in a linear model
modeldata <- d
modeldata$ESACCI_cavm_general_ESACCI_CAVM_merged <- factor(modeldata$ESACCI_cavm_general_ESACCI_CAVM_merged) 
modeldata$Meas_year <- factor(modeldata$Meas_year)
modeldata$Interval <- factor(modeldata$Interval)

lm1 <- lm(as.formula(paste(i,"~", paste(Baseline_vars, collapse="+"))), data=modeldata)
summary(lm1)
plot(modeldata$'i', predict(lm1, modeldata))


### General additive model tests

