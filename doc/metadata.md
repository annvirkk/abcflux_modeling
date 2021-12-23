# Documentation for scripts and results
All file paths are relative to the project root.

The order to run the scripts: model tuning -> model exploration -> model prediction -> model prediction output exploration

Datasets that are needed to run the scripts are located in Cloudops:\
Model training data: /mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/results/final/modeldata_avg.csv\
Predictors: /mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_1km and /mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km\
Additional spatial datasets for zonal stats etc: /mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters

## src


### Model tuning

#### Quantile random forest regression with the full dataset  - [src/final_qrf_caret_runs_LOOCV](../src/final_qrf_caret_runs_LOOCV)
This code tunes the model for each response variable and spatial resolution based on leave-one-site-out CV and RMSE. All predictors are included to the models. 
Note that 1 and 8 km models have almost identical set of predictors, however NDVI and temperature data sources are different (MODIS vs. AVHRR, MODIS LST vs. TerraClimate tmean). 1km models also have a few additional variables (MODIS NDWI and NDII) not included in 8km models.\
Script done and ran on the desktop server 12/2021.

##### Outputs in the results folder
Caret model files saved in .rds format\
e.g. NEE_gC_m2_1km_qrf_train_loocv.rds: NEE response variable, 1 km model, qrf model, tuned using the train command (feature selection command rfe not used), validated based on leave one site out cross validation

#### Quantile random forest regression with different subsets of the data  - [src/final_qrf_caret_runs_LOOCV_datasubsets](../src/final_qrf_caret_runs_LOOCV_datasubsets)
This code tunes several models for each response variable and spatial resolution based on leave-one-site-out CV and RMSE using different subsets of model training data (i.e. sensitivity tests).
The goal is to understand how different flux measurement methods and disturbed sites influence the predictions. \
Script done and ran on the desktop server 12/2021.

##### Outputs in the results folder
Caret model files saved in .rds format. Similar naming logic as in above, but subsets defined after the more general model description:\
full_model_without_larvaloutbreak - all the predictors included, larval outbreak chamber measurements removed from model training data\
full_model_without_vegtype - full model training dataset used, vegetation type removed\
nofactors_model_with_econly - all the predictors that are continuous included (categorical variables not included because these different subsets do not have data from all factor levels), only EC fluxes included\
nofactors_model_with_reichsteinonly - all the predictors that are continuous included (categorical variables not included because these different subsets do not have data from all factor levels), only EC fluxes partitioned with night-time approach included\
nofactors_model_without_daytimechambermeas - all the predictors that are continuous included (categorical variables not included because these different subsets do not have data from all factor levels), chamber measurements with only midday measurements excluded \
nofactors_model_datafromallfluxes - all the predictors that are continuous included (categorical variables not included because these different subsets do not have data from all factor levels), only observations that have GPP, Reco, and NEE values included (so that the sample size is the same across models)\
nofactors_model_without_disturbedsites - all the predictors that are continuous included (categorical variables not included because these different subsets do not have data from all factor levels), only undisturbed sites included\
nofactors_model_without_larvaloutbreak - all the predictors that are continuous included (categorical variables not included because these different subsets do not have data from all factor levels), larval outbreak disturbance sites removed (similar to the full_model_without_larvaloutbreak model except here factor variables are not included)\
nofactors_model_without_disturbedsites_plusharvest - all the predictors that are continuous included (categorical variables not included because these different subsets do not have data from all factor levels), only undisturbed sites plus harvested sites included
...

#### HGAM - [src/hGAM](../src/hGAM)
Most of the scripts in this folder have been developed for producing and exploring machine learning -based flux upscaling results. To back up those results, we will also do some in-situ analyses with the site-level flux measurements alone.
For this, we will use hierarchical GAMs that can account for the monthly nature of the data, site-level ranfom effects, and temporal autocorrelation of the measurements. 
Hierarchical GAMs will be used to 1) explore long-term trends in fluxes over time across years, seasons, and months (data from ~1990 to 2020), and 2) explore the relationships between the key predictors and fluxes. 
This script does all those things.\
Script not finalized yet, waiting for feedback from Konsta 12/2021. Need to continue improving the script once the machine learning models are working OK.


### Model exploration

#### Quantile random forest regression visualization with the full dataset: predictive performance, variable importance, and partial dependence plots - [src/final_qrf_predperf_LOOCV](../src/final_qrf_predperf_LOOCV)
This code visualizes the model performance and behaviour.\
Script done and ran on the desktop server 12/2021.

##### Outputs in the results/final folder
Predictive performance figures have the word prefperd in the filename plus some descriptive information on what was visualized (LOOCV results).\
Variable importance figures have the word vip in the filename.\
Partial dependence figures always include the word pdp and the predictor name.

results folder includes the variable improtance scores table for each model

#### Quantile random forest regression visualization: predictive performance, variable importance, and partial dependence plots - [src/final_qrf_predperf_LOOCV_datasubsets](../src/final_qrf_predperf_LOOCV_datasubsets)
This code visualizes the model performance and behaviour of the model trained without larval outbreak chamber measurements. \
Script done and ran on the desktop server 12/2021. If understanding the different model predictions is challenging, this script might need to be edited so that it produces the figures for all the models trained with different subsets.

#### Quantile random forest regression exploration: predictive performance of the models trained with different subsets  - [src/summarize_predictive_performance_from_sensitivitytests](../src/summarize_predictive_performance_from_sensitivitytests)
This code prints the CV performance estimates of all the trained qrf models. \
Script done and ran on CloudOps 12/2021 to extract some estimates, should probably be edited so that the estimates could be saved to a .csv file.



### Model prediction 

#### Predict at 8 km resolution from 1982 to 2016 with the models trained with the full dataset - [src/final_qrf_8km_predictions_LOOCV](../src/final_qrf_8km_predictions_LOOCV)
This code gathers the predictor rasters resampled to 8 km resolution, loads the 8 km model trained in ../src/final_qrf_caret_runs_LOOCV, loops through each year and month and predicts fluxes across the domain.
Each predictor and its unit should have been explained in the code. Predictor rasters are in North Pole Lambert Azimuthal Equal Area projection.
Note that the different predictors that were resampled and scaled so that they can be saved as integer tifs first need to be divided by the scaling factor to get back to the same scale used in flux model training data (this is done in the code).
Also note that NA values of the resampled rasters were sometimes transferred to large negative values -248.... or zeroes even though I defined that NAs should remain NAs. This must be some kind of a bug in terra's package version.
But those NA values should be removed when the script removes the cells that are missing data in any of the predictors. But it is good to keep this in mind when predictor variables are being visualized.
Prediction outputs are in .csv files (fluxes multiplied by 1000 and stored as integers to save memory). \
Script done and preliminary model runs made in Kubernetes 12/2021, but the script should be run as a whole again on ADAPT and might need to be adjusted to this platform.

#### Predict at 8 km resolution from 1982 to 2016 with the models trained with different model training data set subsets - [src/final_qrf_8km_predictions_LOOCV_datasubsets](../src/final_qrf_8km_predictions_LOOCV_datasubsets)
Same as above, but with the models produced in [src/final_qrf_caret_runs_LOOCV_datasubsets](../src/final_qrf_caret_runs_LOOCV_datasubsets).\
Script done and preliminary model runs made in Kubernetes 12/2021, but the script should be run as a whole again on ADAPT and might need to be adjusted to this platform. 1km data subset prediction code not prepared yet. 

#### Predict at 1 km resolution from 2001 to 2020 with the models trained with the full dataset - [src/final_qrf_1km_predictions_LOOCV](../src/final_qrf_1km_predictions_LOOCV)
This code gathers the predictor rasters resampled to 1 km resolution, loads the 1 km model trained in ../src/final_qrf_caret_runs_LOOCV, loops through each year and month and predicts fluxes across the domain.
Each predictor and its unit should be explained in the code. Predictor rasters are in North Pole Lambert Azimuthal Equal Area projection.
Note that the different predictors that were resampled and scaled so that they can be saved as integer tifs first need to be divided by the scaling factor to get back to the same scale used in flux model training data (this is done in the code).
Also note that NA values of the resampled rasters were sometimes transferred to large negative values -248.... or zeroes even though I defined that NAs should remain NAs. This must be some kind of a bug in terra's package version.
But those NA values should be removed when the script removes the cells that are missing data in any of the predictors. But it is good to keep this in mind when predictor variables are being visualized.
Because the 1 km predictor files take a lot of memory when loaded in R, I had to crop the region to several subsets and run the prediction in pieces. This might not be the most efficient way to deal with this - hopefully Stefano will be able to improve the script a bit.
But in case this cropping system is kept in the code, [src/prediction_crop_regions](../src/prediction_crop_regions) needs to be ran first to produce the extents for the different prediction subsets.
Outputs are in .csv files (fluxes multiplied by 1000 and stored as integers to save memory). \
Script done and preliminary model runs in a few regions made in Kubernetes 12/2021, but the script should be run as a whole again on ADAPT and might need to be adjusted to this platform.

#### Extract the individual tree predictions from the 8 km qrf model trained with the full dataset to evaluate uncertainty - [src/final_qrf_8km_predictionuncertainty_LOOCV](../src/final_qrf_8km_predictionuncertainty_LOOCV)
This code is identical to [src/final_qrf_8km_predictions_LOOCV](../src/final_qrf_8km_predictions_LOOCV) but it extracts all the 500 individual tree predictions, and not just the aggregate ensemble prediction. 
The code then writes out all those 500 predictions out of which prediction uncertainty can be summarized. The script for summarizing the uncertainty has not been done yet, 
but basically it should 1) calculate the variability in predicted values in each pixel to visualize uncertainty on a map (in different months, seasons, and years, e.g. using prediction intervals from 0.025 and 0.975 quantiles), and
2) provide uncertainty estimates for the budgets, i.e. a flux budget should be calculated from each individual prediction, and then out of the 500 budgets the budget uncertainty can be estimated from e.g. 0.025 and 0.975 quantiles.
Some example pixelwise uncertainty estimates: https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2019GB006264
Note that if we decide to use another model training subset as a basis of the main results (e.g., larval outbreak chamber sites removed), we will need to re-run this code with that model. 
There is no need to run this script across all the different model versions. \
Script done and preliminary model runs made in Kubernetes 12/2021, but the script should be run as a whole again on ADAPT (when we have a decision on the final model) and might need to be adjusted to this platform.
We will also need to prepare another script that summarizes the prediction uncertainty and budget uncertainty based on the outputs of this script (or those could also be added here).


### Model prediction output exploration

#### Convert prediction .csvs to .tifs and calculate average rasters for different temporal periods - [src/produce_flux_rasters](../src/produce_flux_rasters) 
This code includes a function that calculates annual cumulative, seasonal cumulative, and monthly average rasters for different temporal periods 
(e.g., average cumulative annual NEE, winter/summer/autumn/spring cumulative NEE, monthly NEE over 2000-2005). Throughout the scripts, we use the climatological seasons for the season definition (DJF winter, MAM spring, JJA summer, SON autumn).\
Script done and preliminary 8 km prediction averages made in Kubernetes 12/2021. The script needs to be adjusted for the 1 km predictions too.

#### Calculate raster trends and statistics from flux predictions - [src/calculate_predicted_flux_budgets_stats_trends](../src/calculate_predicted_flux_budgets_stats_trends) 
This code should calculate 1) budgets across the domain and across the key regions for each year (i.e. sum of pixel values across the domain, table output), 2) regional averages of fluxes in key regions for each year (table output),
3) pixelwise temporal trends across annual cumulative, seasonal cumulative, and monthly rasters (map output), 4) regional summaries of those trends (zonal statistics) in key regions (table output). 
Budgets will be calculated by multiplying the g C m-2 fluxes by the area of the pixel (~1 or ~8 km) and summing the pixels. Flux unit should ideally be Tg C yr-1 and values for NEE should be between -1000 and +1000 Tg C yr-1.
Ultimately a similar script should be used to calculate average values in environmental rasters (tmean, NDVI etc.)\
Script not done - only some comments and links to datasets used in zonal statistics calculation added. 

#### Visualize the distribution of environmental and flux values in different time periods to understand potential shifts in conditions - [src/env_flux_density_histograms](../src/env_flux_density_histograms) 
Preliminary script with environmental raster visualizations done 12/2021. 

#### Visualize environmental coverage of flux sites included in model training data - [src/visualize_env_coverage](../src/visualize_env_coverage) 
This code calculates the area of extrapolation across the pan-Arctic domain by comparing the predictor values in model training data to the raster predictor values across the entire domain. 
Negative values= extrapolation, positive= no extrapolation. The code estimates the extrapolation area for different time periods based on the predictors that have been aggregated to annual averages over that period.\
Preliminary script and analysis runs done 12/2021 on CloudOps. 

#### In-situ flux averages based on model training data - [src/calculate_insitu_flux_stats](../src/calculate_insitu_flux_stats) 
This code calculates average fluxes in the model training data. We might need to check what the dataset that we use to estimate is (e.g. full data or one of the data subsets), and whether the statistics should be based on gridded vs. in-situ information (e.g., vegetation type).\
Script done and preliminary averages explored 12/2021.


### Other files

#### Quantile random forest regression with the full dataset, feature selection  - [src/final_qrf_caret_runs_featureselection_LOOCV](../src/final_qrf_caret_runs_featureselection_LOOCV)
This code tunes the model for each response variable and spatial resolution based on leave-one-site-out CV and RMSE. Simultaneously, best predictors for each model are selected based on backward feature selection algorithm. 
Note that these models will not be used in the final paper as we chose to use all the predictors across the models - these scripts were just run to double check that the number of predictors does not play an improtant role for the models. 
Script done and ran on the desktop server 12/2021.

#### Model tuning, exploration, and prediction script using the three machine learning models - [src/final_gbmsvmrf...LOOCV](../src/final_gbmsvmrf...LOOCV) 
We were originally exploring three different machine learning models, out of which we aimed to use the ensemble median prediction. However, gbm performed poorly and random forest seemed to be the best model in almost all cases. 
Consequently, we decided to focus on random forest analyses only, but I kept these scripts here just in case.

#### old scripts - [src/old](../src/old) 
Some old files that I still wanted to keep in the repository, e.g. feature selection with 10-fold cross validation, first gbm/svm/rf scripts.