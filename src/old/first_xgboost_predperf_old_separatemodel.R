


# Packages
library("caret")
library("vip")
library("pdp")
library("ggplot2")
library("viridis")

### Data
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv")

### Response variables
resp_vars <- c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2") 


# For figures

theme_pub <- theme_bw() + theme(panel.border=element_rect(size=1, colour="black"),
                                axis.text=element_text(size=14, face="bold"),
                                plot.title = element_text(size = 14, face = "bold"),
                                axis.title=element_text(size=14, face="bold"), 
                                plot.subtitle=element_text(size=14, face="bold", color="black"), 
                                strip.text.x = element_text(size = 14, face="bold"),
                                legend.text=element_text(size=14, face="bold"), legend.title=element_text(size=14))



for (i in resp_vars) {
  
  
  # Loop through the models 
  
  for (m in models) {
  
  
  }
  
  
  # Print the best variables
  xgFit$bestSubset
  xgFit$optVariables

  # i <- "NEE_gC_m2"
  
  
  
  ### Predictive performance plots ###
  
  # Load model files
  xgFit <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i, "1km_xgboost_test", sep="_"), ".rds"))  # remove _test when we have final
  # rfFit <- readRDS(paste0("../results/", paste(i, "rf", sep="_"), ".rds"))
  # svmFit <- readRDS(paste0("../results/", paste(i, "svm", sep="_"), ".rds"))
  
  
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
  scale_max <- max(c(xgFit$pred$obs, xgFit$pred$pred))
  scale_min <- min(c(xgFit$pred$obs, xgFit$pred$pred))
  
  # scale_max <- max(c(gbmFit$pred$obs, gbmFit$pred$pred, rfFit$pred$obs, rfFit$pred$pred, svmFit$pred$obs, svmFit$pred$pred))
  # scale_min <- min(c(gbmFit$pred$obs, gbmFit$pred$pred, rfFit$pred$obs, rfFit$pred$pred, svmFit$pred$obs, svmFit$pred$pred))
  
  
  # Merge observations and predictions to one data frame, and merge it with cavm
  # First need to sort the tables in the same order as in d
  xgdata <- data.frame(cbind(xgFit$pred$obs, xgFit$pred$pred))
  names(xgdata) <- c("obs", "xgboost")
  # rfdata <- data.frame(cbind(rfFit$pred$obs, rfFit$pred$pred))
  # names(rfdata) <- c("obs", "rf")
  # svdata <- data.frame(cbind(svmFit$pred$obs, svmFit$pred$pred))
  # names(svdata) <- c("obs", "svm")
  
  r <- merge(xgdata, d, by.x="obs", by.y=i) # this is not ideal - there are duplicate observations that will mix this up
  # r1 <- merge(gbmdata, rfdata, by="obs") 
  # r2 <- merge(r1, svdata, by="obs")
  # r <- merge(r2, d, by.x="obs", by.y=i)  

    

  
  
  
  # check whether Rsquared and correlation-derived estimate match
  #cor(r$obs, r$gbm)^2 # yep, they are the same!
  #rmse(r$obs, r$gbm)
  #mean(abs(r$obs -r$gbm))
  
  # Merge pred perf
  # not sure how to access the results from the best model! This is a shortcut: xgFit$results[which.min(xgFit$results[, "RMSE"]), ]
  r_stats <- data.frame(cbind(model=c("xgboost"), RMSE=c(xgFit$results[which.min(xgFit$results[, "RMSE"]), ]$RMSE),
                                                                      Rsquared=(xgFit$results[which.min(xgFit$results[, "RMSE"]), ]$Rsquared),
                              MAE=c(xgFit$results[which.min(xgFit$results[, "RMSE"]), ]$MAE)))
  r_stats$RMSE <- as.character(r_stats$RMSE) %>% as.numeric()
  r_stats$Rsquared <- as.character(r_stats$Rsquared) %>% as.numeric()
  r_stats$MAE <- as.character(r_stats$MAE) %>% as.numeric()
  
  # r_stats <- data.frame(cbind(model=c("xgboost", "rf", "svm"), RMSE=c(xgFit$results[which.min(xgFit$results[, "RMSE"]), ]$RMSE, rfFit$results[which.min(rfFit$results[, "RMSE"]), ]$RMSE, svmFit$results[which.min(svmFit$results[, "RMSE"]), ]$RMSE), 
  #                                                                     Rsquared=(xgFit$results[which.min(xgFit$results[, "RMSE"]), ]$Rsquared, rfFit$results[which.min(rfFit$results[, "RMSE"]), ]$Rsquared, svmFit$results[which.min(svmFit$results[, "RMSE"]), ]$Rsquared), 
  #                             MAE=c(xgFit$results[which.min(xgFit$results[, "RMSE"]), ]$MAE, rfFit$results[which.min(rfFit$results[, "RMSE"]), ]$MAE, svmFit$results[which.min(svmFit$results[, "RMSE"]), ]$MAE)))
  # 
  
  
  ### Plot XGBOOST based on different information
  
  # Colored by biome
  p1 <- ggplot(r, aes(x=xgboost, y=obs, colour=factor(Biome))) +
    geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
    
    annotate(label = sprintf("MAE = %.1f   \nRsquared = %.2f   \nRMSE = %.1f   \n ", 
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(Rsquared) %>% as.numeric(),
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
    
    theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE)
  
  # Print out
  setwd("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/figures/")
  p1
  dev.copy(png, paste0(i, "_xgboost_predperf_biome.png"), width=500, height=400)
  dev.off()
  
  
  # Colored by veg type
  p2 <- ggplot(r, aes(x=xgboost, y=obs, colour=factor(Veg_type2))) +
    geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
    
    annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(Rsquared) %>% as.numeric(),
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
    
    theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE)
    
  p2
  dev.copy(png, paste0(i, "_xgboost_predperf_vegtype.png"), width=500, height=400)
  dev.off()
  
  
  # Colored by disturbance
  r$Disturbance <- ifelse(is.na(r$Disturbance), "NA", r$Disturbance)
  p3 <- ggplot(r, aes(x=xgboost, y=obs, colour=factor(Disturbance))) +
    geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
    
    annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(Rsquared) %>% as.numeric(),
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
    
    theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE)
  
  p3
  dev.copy(png, paste0(i, "_xgboost_predperf_disturbance.png"), width=500, height=400)
  dev.off()
  
  
  # Colored by Study ID Clean
  p4 <- ggplot(r, aes(x=xgboost, y=obs, colour=factor(Study_ID_Clean))) +
    geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
    
    annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(Rsquared) %>% as.numeric(),
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
    
    theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE)
  
  p4
  dev.copy(png, paste0(i, "_xgboost_predperf_studyID.png"), width=500, height=400)
  dev.off()
  
  
  # Colored by flux method
  p5 <- ggplot(r, aes(x=xgboost, y=obs, colour=factor(Flux_method))) +
    geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
    
    annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(Rsquared) %>% as.numeric(),
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
    
    theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE)
  
  p5
  dev.copy(png, paste0(i, "_xgboost_predperf_fluxmethod.png"), width=500, height=400)
  dev.off()
  
  
  # Colored by flux method detail
  p6 <- ggplot(r, aes(x=xgboost, y=obs, colour=factor(Flux_method_detail))) +
    geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
    
    annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(Rsquared) %>% as.numeric(),
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
    
    theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE)
  
  
  p6
  dev.copy(png, paste0(i, "_xgboost_predperf_fluxmethoddetail.png"), width=500, height=400)
  dev.off()
  
  
  
  # Colored by Measurement frequency
  p7 <- ggplot(r, aes(x=xgboost, y=obs, colour=factor(Measurement_frequency))) +
    geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
    
    annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(Rsquared) %>% as.numeric(),
                             r_stats %>% filter(model=="xgboost") %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
    
    theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE)
  
  p7
  dev.copy(png, paste0(i, "_xgboost_predperf_measfreq.png"), width=500, height=400)
  dev.off()
  
  
  
}

