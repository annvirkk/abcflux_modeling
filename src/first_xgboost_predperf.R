


# Packages
library("caret")
library("vip")
library("pdp")
library("ggplot2")
library("viridis")

### Data
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv")


### Calculate the factor variables 
### Factor conversions
# note that randomForest and svmRadial can handle variables coded as "as.factor"
# but xgboost cannot: https://github.com/dmlc/xgboost/issues/95
# so we will need to do one-hot encoding

# Thermokarst
d <- as.data.frame(d)
d$Thermokarst <- factor(d$Thermokarst)
onehot <- model.matrix(~0+d[, 'Thermokarst'])
attr(onehot, "dimnames")[[2]] <- paste("Thermokarst", levels(d$Thermokarst), sep="_")
d <- cbind(d, onehot)

# land cover
# first change NA in ESA CCI to something else
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged <- ifelse(is.na(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged), "0000", d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged)
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged <- factor(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged)
onehot <- model.matrix(~0+d[, 'ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged'])
attr(onehot, "dimnames")[[2]] <- paste("ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged", levels(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged), sep="_")
d <- cbind(d, onehot)

# Fire burn classes
d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned <- factor(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)
onehot <- model.matrix(~0+d[, 'Number_of_days_since_fire_classes_MCD64A1_sites_cleaned'])
attr(onehot, "dimnames")[[2]] <- paste("Number_of_days_since_fire_classes_MCD64A1_sites_cleaned", levels(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned), sep="_")
d <- cbind(d, onehot)



### Response variables
resp_vars <- c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2") 

### Models
models <- c("gbm", "rf", "svm")


# For figures

theme_pub <- theme_bw() + theme(panel.border=element_rect(size=1, colour="black"),
                                axis.text=element_text(size=14, face="bold"),
                                plot.title = element_text(size = 14, face = "bold"),
                                axis.title=element_text(size=14, face="bold"), 
                                plot.subtitle=element_text(size=14, face="bold", color="black"), 
                                strip.text.x = element_text(size = 14, face="bold"),
                                legend.text=element_text(size=14, face="bold"), legend.title=element_text(size=14))



for (i in resp_vars) {
  
  # i <- "NEE_gC_m2"
  
  # empty varimp data frame
  all_varImp <- NA
  
  
  # Loop through the models 
  
  for (m in models) {
    
    # m <- "rf"

    # Load model files
    mod <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i, "1km", m, sep="_"), ".rds"))
    # m <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i, "1km_xgboost_test", sep="_"), ".rds"))  # remove _test when we have final
    # rfFit <- readRDS(paste0("../results/", paste(i, "rf", sep="_"), ".rds"))
    # svmFit <- readRDS(paste0("../results/", paste(i, "svm", sep="_"), ".rds"))
    
    
    # Print the best variables
    print("Best variables are:")
    mod$bestSubset
    mod$optVariables
    Selected_vars_1km <- mod$optVariables

    ### Predictive performance plots ###
    
    
    # Define x and y lab titles for the plot
    if (i=="GPP_gC_m2") {
      ylab = expression(paste("Observed GPP g C m"^{-2}, month^{-1}))
      xlab = expression(paste("Predicted GPP g C m"^{-2}, month^{-1}))
      
    }
    
    if (i=="NEE_gC_m2") {
      ylab = expression(paste("Observed NEE g C m"^{-2}, month^{-1}))
      xlab = expression(paste("Predicted NEE g C m"^{-2}, month^{-1}))
      
    }
    
    if (i=="Reco_gC_m2") {
      ylab = expression(paste("Observed ER g C m"^{-2}, month^{-1}))
      xlab = expression(paste("Predicted ER g C m"^{-2}, month^{-1}))
      
    }
    
    
    
    
    # First plot scatterplots for each variable based on individual models
    # Max and min of several columns 
    scale_max <- max(c(mod$pred$obs, mod$pred$pred))
    scale_min <- min(c(mod$pred$obs, mod$pred$pred))
    
    # scale_max <- max(c(gbmFit$pred$obs, gbmFit$pred$pred, rfFit$pred$obs, rfFit$pred$pred, svmFit$pred$obs, svmFit$pred$pred))
    # scale_min <- min(c(gbmFit$pred$obs, gbmFit$pred$pred, rfFit$pred$obs, rfFit$pred$pred, svmFit$pred$obs, svmFit$pred$pred))
    
    
    # Merge observations and predictions to one data frame, and merge it with cavm
    # First need to sort the tables in the same order as in d
    figdata <- data.frame(cbind(mod$pred$obs, mod$pred$pred))
    names(figdata) <- c("obs", m)
    
    r <- merge(figdata, d, by.x="obs", by.y=i) # this is not ideal - there are duplicate observations that will mix this up

    
    # check whether Rsquared and correlation-derived estimate match
    #cor(r$obs, r[, m])^2 # yep, they are the same!
    #rmse(r$obs, r[, m])
    #mean(abs(r$obs -r[, m]))
    
    # Merge pred perf
    # not sure how to access the results from the best model! This is a shortcut: mod$results[which.min(mod$results[, "RMSE"]), ]
    r_stats <- data.frame(cbind(model=c(m), RMSE=c(mod$results[which.min(mod$results[, "RMSE"]), ]$RMSE),
                                Rsquared=(mod$results[which.min(mod$results[, "RMSE"]), ]$Rsquared),
                                MAE=c(mod$results[which.min(mod$results[, "RMSE"]), ]$MAE)))
    r_stats$RMSE <- as.character(r_stats$RMSE) %>% as.numeric()
    r_stats$Rsquared <- as.character(r_stats$Rsquared) %>% as.numeric()
    r_stats$MAE <- as.character(r_stats$MAE) %>% as.numeric()
    
    
    ### Plot XGBOOST based on categorical information that we have
    
    # Colored by biome
    p1 <- ggplot(r, aes(x=r[,m], y=obs, colour=factor(Biome))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f   \nRsquared = %.2f   \nRMSE = %.1f   \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    # Print out
    setwd("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/figures/")
    p1
    dev.copy(png, paste0(i, "_xgboost_predperf_biome.png"), width=500, height=400)
    dev.off()
    
    
    # Colored by veg type
    p2 <- ggplot(r, aes(x=r[,m], y=obs, colour=factor(Veg_type2))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    p2
    dev.copy(png, paste0(i, "_xgboost_predperf_vegtype.png"), width=500, height=400)
    dev.off()
    
    
    # Colored by disturbance
    r$Disturbance <- ifelse(is.na(r$Disturbance), "NA", r$Disturbance)
    p3 <- ggplot(r, aes(x=r[,m], y=obs, colour=factor(Disturbance))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    p3
    dev.copy(png, paste0(i, "_xgboost_predperf_disturbance.png"), width=500, height=400)
    dev.off()
    
    
    # Colored by Study ID Clean
    p4 <- ggplot(r, aes(x=r[,m], y=obs, colour=factor(Study_ID_Clean))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    p4
    dev.copy(png, paste0(i, "_xgboost_predperf_studyID.png"), width=500, height=400)
    dev.off()
    
    
    # Colored by flux method
    p5 <- ggplot(r, aes(x=r[,m], y=obs, colour=factor(Flux_method))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    p5
    dev.copy(png, paste0(i, "_xgboost_predperf_fluxmethod.png"), width=500, height=400)
    dev.off()
    
    
    # Colored by flux method detail
    p6 <- ggplot(r, aes(x=r[,m], y=obs, colour=factor(Flux_method_detail))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    
    p6
    dev.copy(png, paste0(i, "_xgboost_predperf_fluxmethoddetail.png"), width=500, height=400)
    dev.off()
    
    
    
    # Colored by Measurement frequency
    p7 <- ggplot(r, aes(x=r[,m], y=obs, colour=factor(Measurement_frequency))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    p7
    dev.copy(png, paste0(i, "_xgboost_predperf_measfreq.png"), width=500, height=400)
    dev.off()
    
  
    
    
    ### Variable importance calculation
    varImp <- vip(mod, method="permute", train=r[, Selected_vars_1km], target = r$obs, metric = "rsquared",
                      pred_wrapper = predict, nsim=100) # only 10 most important ones selected
    
    
    

  } # model loop done
  
  ### Variable importance plots
  # Extract data and combine into one data frame
  all_varImp <- rbind(all_varImp, cbind(varImp$data, "Model"=c(m)))

  all_varImp$Model <- ifelse(all_varImp$Model=="xgboost", "XGBOOST", all_varImp$Model)
  all_varImp$Model <- ifelse(all_varImp$Model=="rf", "RF", all_varImp$Model)
  all_varImp$Model <- ifelse(all_varImp$Model=="svm", "SVM", all_varImp$Model)
  all_varImp$Model <- factor(all_varImp$Model)
  #all_varImp$Model <- factor(all_varImp$Model, levels=c("XGBOOST", "RF", "SVM"))
  
  all_varImp <- all_varImp[-1, ]
  
  # Print out
  print(ggplot(all_varImp) + geom_bar(aes(x=Variable, y=Importance, fill=Model), stat="identity", position="dodge")  + 
          scale_fill_manual(values = c(viridis(3)[1],viridis(3)[2], viridis(3)[3]), guide = guide_legend(reverse=TRUE))  + 
          coord_flip() +
          theme_pub) #+ ggtitle(title2)
  
  dev.copy(png, paste0( i, "_vip.png"), width=650, height=400)
  dev.off()
  
  
  
  
  
}






### Partial dependence plots need to be done in a separate loop


# Partial dependence plots

# First baseline var
pd1 <- partial(gbmFit, pred.var = Baseline_vars[1], train=data1)  # don't set plot = TRUE
pd2 <- partial(rfFit, pred.var = Baseline_vars[1], train=data1)
pd3 <- partial(svmFit, pred.var = Baseline_vars[1], train=data1)


# ggplot2
pd1$Model <- "GBM"  # add new column
pd2$Model <- "RF"
pd3$Model <- "SVM"

pd.all1 <- rbind(pd1, pd2, pd3)  # bind rows

pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
  geom_line(size=1) + theme_pub  +labs(y="yhat", x=Baseline_vars[1]) + 
  scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
  theme(legend.position = "none") # + ggtitle(title)

