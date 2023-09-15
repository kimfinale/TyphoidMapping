Mapping the incidence rate of typhoid fever in sub-Saharan Africa
================
2023-09-15

Running the scripts below will create CSV files within the ‘output’ and
‘output/Forecasting’ directories. Therefore, to ensure the functions run
properly, under the current working directory, a directory named
‘output’ and a directory ‘output/Forecasting’ are needed for the codes
to run. Also, the following R packages are necessary for executing the
codes. If these packages are not yet installed on your machine, kindly
proceed to install them.

### Load packages

``` r
library(car)
library(MASS)
library(ciTools)
library(AER)
library(viridis)
library(raster)
library(ggplot2)
library(dplyr)
library(sp)
library(plyr)
library(data.table)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

files = list.files("R/", full.names = TRUE)
sapply(files, source)

cov.name <- c("elevation","distance_water","improved_water",
            "improved_sanitation","annual_rainfall",
            "annual_mean_temp","stunting_prev",
            "HIV_prev",
            "travel_time_city","piped_water","piped_sanitation",
            "surface_water",
            "open_defecation","wasting","underweight","pop_size")
```

### Regression

#### Poisson regression

``` r
res <- poisson_model()
```

#### Negative binomial regression

``` r
res1 <- negbin_model_1()
res2 <- negbin_model_2()
```

### Prediction

#### Prepare covariates for prediction

``` r
pred_loc <- get_prediction_location()
pred_cov <- get_prediction_covariate()
```

#### Predict based on the poissonl model

``` r
pred <- predict_poisson_model()
```

#### Predict based on the NegBin model

``` r
pred1 <- predict_negbin_model_1()
pred2 <- predict_negbin_model_2()
```

### Plots

#### Figure 1

Plot incidence rate from surveillance studies

``` r
plt <- figure_1()
plt
# ggsave(plot=plt, file="plots/plot_incidence_rates.png", width=11, height=8,
#        units="in")
```

For Figure 2 and onwards, following computations are required. \####
Preliminaries

``` r
# Reference shapefiles
afss <- readRDS("data/africa_sub_Sahara_adm0_shp.rds")
afssadm1 <- readRDS("data/africa_sub_Sahara_adm1_shp.rds")

# Functions
# source function would not be needed if the package is loaded
# source("R/ggplot2_theme.R")
# source("R/util.R")
# source("R/map_functions.R")
# # library(raster)
# library(terra)
# variable names for estimates
estim_type <- c("pred", "lower", "upper") # mean, lower bounds, and upper bounds
ag <- c("0_1y", "2_4y", "5_14y", "over14y") # age group
countries <- unique(afssadm1$NAME_0)
```

### Incidence rate (IR) accounting for blood culture sensitivity and maximum rate

Some studies reported incidence rates that do not account for blood
culture sensitivity while others reported incidence rates that do. In
addition, studies that reported incidence rates accounting for blood
culture sensitivity used different sensitivity values. We therefore
modeled incidence rates that do not account for BC sensitivity for
consistency and later took it into consideration by multiplying the
estimates with uniformly 1/0.6

``` r
## function to account for blood culture sensitivity
adjust_blood_culture_sensitivity <- function(raster, BCS=0.6, max_IR=1e4){
  raster[] <- raster[] / BCS
  raster[][raster[] > max_IR] <- max_IR
  
  return(raster)
} 

for (bd in c("pred", "lower", "upper")) {
  ir_0_1y <- raster(paste0("data\\Results_20221206\\Final output\\Forecasting_", bd, "_Age_0-1 y_PoiReg.tif"))
  
  ir_2_4y <- raster(paste0("data\\Results_20221206\\NB with Ver2\\Forecasting_", bd, "_Age_2-4 y_NBReg_Stepwise_v6_202212_Ver2.tif"))
  
  ir_5_14y <- raster(paste0("data\\Results_20221206\\NB with Ver2\\Forecasting_", bd, "_Age_5-14 y_NBReg_Stepwise_v6_202212_Ver2.tif"))
  
  ir_over14y <- raster(paste0("data\\Results_20221206\\NB with Ver2\\Forecasting_", bd, "_Age_over 14y_NBReg_Stepwise_v6_202212_Ver2.tif"))
  
  rstlist <- vector("list", length(ag))
  rstlist[[1]] <- ir_0_1y
  rstlist[[2]] <- ir_2_4y
  rstlist[[3]] <- ir_5_14y
  rstlist[[4]] <- ir_over14y
  
  for (i in seq_along(rstlist)) {
    rst <- adjust_blood_culture_sensitivity(raster = rstlist[[i]])
    saveRDS(rst, paste0("output/ir_", bd, "_bc_adj_", ag[i], "_", tstamp(), ".rds"))
  }
}
```

### IR by age by region

Use the incidence rate by age (original incidence rate estimates) and
population size by age (which is original population per pixel \[ppp\]
multiplied by proportion by age by country) on 20 km by 20 km grids.
Simply multiply the two to get the number of cases on grids and sum
across the grids on the region to get the region-specific number of
cases. To get the averaged incidence rate at the regional level, divide
the number of cases in the region with the population size of the
region, which that is achieved by summing ppp across the region.

Two rasters (ppp and ir\_\*) have the same resolutions and were not
aligned perfectly and therefore, resample is done as ppp for 0-1 yo as
the reference

#### Resampling

``` r
ppp_0_1y <- readRDS(paste0("data/covariates/prediction/ppp_", ag[1], "_20km_af_2017_20221208.rds"))

ir_raster_resample <- function(pred, age, ref_raster){
  rst <- readRDS(paste0("output/ir_", pred, "_bc_adj_", age, "_20230208.rds"))
  rst_resampled <- raster::resample(rst, ref_raster, method = 'bilinear') 
  
  return(rst_resampled)
}

for(pd in estim_type){
  for(a in ag){
    rst <- ir_raster_resample(pred=pd, age=a, ref_raster=ppp_0_1y) 
    saveRDS(rst, paste0("output/ir_resampled_", pd, "_bc_adj_", a, "_", tstamp(), ".rds"))
  }
}
```

#### Accounting for population size

``` r
regions <- c("country", "subnational", "subregion")
# region <- regions[2] # as an example
tm <- "20230208"
for(region in regions){
  for (j in 1:3) {
    irdata <- list()
    for (i in 1:4) {
      cat("i =", i, ", j =", j, "\n")
      ppp <- readRDS(paste0("data/prediction/ppp_", ag[i], "_20km_af_2017_20221208.rds"))
      ir <- readRDS(paste0("output/ir_resampled_", estim_type[j], "_bc_adj_", ag[i], "_", tm, ".rds"))
      res <- IR_by_region(ppp=ppp, ir=ir, region=region, shape=afssadm1)
      
      saveRDS(res$raster, paste0("output/ir_", estim_type[j], "_", region, "_", ag[i], "_", tstamp(), ".rds"))
      irdata[[i]] <- res
    }
    saveRDS(irdata, paste0("output/summary_ir_", estim_type[j], "_", region, "_", tstamp(), ".rds"))
  }
}
```

### IR for all ages

Use the incidence rate by age (original incidence rate estimates) and
population size by age ltiplied by proportion by age by country) on 20
km by 20 km grids. Simply multiply the two to get the number of cases on
grids and sum across the region to get the region-specific number of
cases. Divide that with the region-specific population size that is
achieved by summing ppp across the region.

``` r
# overall population size
pppall <- readRDS("data/prediction/ppp_20km_af_2017_20221208.rds")
# case = ir *  ppp
# raster template - values will change
rst <- readRDS(paste0("output/ir_resampled_", estim_type[1], "_bc_adj_", ag[1], "_20230208.rds"))
for (j in 1:length(estim_type)) {
  rst[] <- 0
  # sum the number of cases (IR * pop) across age group
  for (i in 1:length(ag)) {
    ir <- readRDS(paste0("output/ir_resampled_", estim_type[j], "_bc_adj_", ag[i], "_20230208.rds"))
    ppp <- readRDS(paste0("data/prediction/ppp_", ag[i], "_20km_af_2017_20221208.rds"))
    rst[] <- rst[] + (ir[] * ppp[])
  }
  # divide with the overall population size
  rst[] <- rst[] / pppall[]
  names(rst) <- paste0("ir_allage_", estim_type[j])
  saveRDS(rst, paste0("output/ir_allage_", estim_type[j], "_", tstamp(), ".rds"))
}

# summarize at the country level
regions <- c("country", "subnational", "subregion")
# region <- regions[2]

tm <- "20230208"
for(region in regions) {
  for (j in 1:3) {
    irdata <- list()
    cat("j =", j, "\n")
    irall <- readRDS(paste0("output/ir_allage_", estim_type[j], "_", tm, ".rds"))
    res <- IR_by_region(ppp=pppall, ir=irall, region=region, shape=afssadm1)
    
    saveRDS(res$raster, paste0("output/ir_allage_", region, "_", estim_type[j], "_", tstamp(), ".rds"))
    saveRDS(res, paste0("output/summary_ir_allage_", region, "_", estim_type[j], "_", tstamp(), ".rds"))
  }
}
```

### Number of case

Number of cases are obtained by multiplying the incidence rate per
100000 person-yeras and population per pixel (ppp) for each age group
with both on 20 km by 20 km grids

``` r
# by age group
tm <- "20230208"
for(j in 1:3) {
  for(i in 1:4) {
    cat("i =", i, ", j =", j, "\n")
    ppp <- readRDS(paste0("data/prediction/ppp_", ag[i], "_20km_af_2017_20221208.rds"))
    ir <- readRDS(paste0("output/ir_resampled_", estim_type[j], "_bc_adj_", ag[i], "_", tm, ".rds"))
  # output raster
  caserst <- ir
  names(caserst) <- c("Number of predicted cases per grid")
  caserst[] <- ir[] * ppp[] / 1e5
  saveRDS(caserst, paste0("output/case_", estim_type[j], "_", ag[i], "_", tstamp(), ".rds"))
  }
}
# all age groups
# template raster with values changed
rst <- readRDS(paste0("output/case_", estim_type[1], "_", ag[1], "_", tstamp(), ".rds"))
for(j in 1:3) {
  rst[] <- 0
  for(i in 1:4) {
    rst_age <- readRDS(paste0("output/case_", estim_type[j], "_", ag[i], "_", tstamp(), ".rds"))
    rst[] <- rst[] + rst_age[]
  }
  saveRDS(rst, paste0("output/case_allage_", estim_type[j], "_", tstamp(), ".rds"))
}
```

### Number of cases by region

``` r
ages <- c(ag, "allage")
tm <- "20230208"
regions <- c("country", "subnational", "subregion")
for (k in 1:length(regions)){
  for (j in 1:length(estim_type)) {
    reslist <- vector("list", length(ages)) 
    for (i in 1:length(ages)) {
      cat("k =", k ,", j =", j,", i =", i, "\n")
      r <- readRDS(paste0("output/case_", estim_type[j], "_", ages[i], "_", tm, ".rds"))
      res <- case_by_region(case = r, region = regions[k])
      saveRDS(res$raster, paste0("output/case_", estim_type[j], "_", regions[k], "_", ages[i], "_", tstamp(), ".rds"))
      reslist[[i]] <- res
    }
  saveRDS(reslist, paste0("output/summary_case_", estim_type[j], "_", regions[k], "_",  tstamp(), ".rds"))
  }
}
```

### IR subnational by age

#### Figure 2

Plot of predicted versus observed based on the leave-one-out
cross-validation

``` r
plt <- figure_2()
plt
# fac <- 2
# ggsave(paste0("plots/obs_pred_", tstamp(), ".png"), p, width=3.4*fac,
#        height=3.4*fac, units="in")
```

Calculate the proportion of observations that are within the 95%
confidence interval of the predicted value.

``` r
dlist <- list()
dlist[[1]] <- fread("data/Prediction_Age_0-1 y_PoiReg.csv")
dlist[[2]] <- fread("data/Prediction_Age_2-4 y_NBReg.csv")
dlist[[3]] <- fread("data/Prediction_Age_5-14 y_NBReg.csv")
dlist[[4]] <- fread("data/Prediction_Age_over 14y_NBReg.csv")
ages <- c("0-1 yo", "2-4 yo", "5-14 yo", "15+ yo")
for (i in 1:4) {
  dlist[[i]]$age <- ages[i]
}
d <- do.call("rbind", dlist)
d$age <- factor(d$age, levels = c("0-1 yo", "2-4 yo", "5-14 yo", "15+ yo"))

for(i in 1:nrow(d)) {
  if(d$lower[i] <= d$y[i] & d$upper[i] >= d$y[i]) {
    d$included[i] <- 1
  } else{
    d$included[i] <- 0
  }
}

d |>
  dplyr::group_by(age) |>
  dplyr::summarise(prop = sum(included) / n())
#   age      prop
#   <fct>   <dbl>
# 1 0-1 yo  0.136
# 2 2-4 yo  0.5  
# 3 5-14 yo 0.538
# 4 15+ yo  0.625  
```

#### Figure 3A-D

``` r
for(i in 1:length(ag)){
  r <- readRDS(paste0("output/ir_pred_subnational_", ag[i], "_20230208.rds"))
  # p <- IR_plot(raster=r, color_ramp="RdYlBu", rev=FALSE)
  p <- IR_plot(raster=r) 
  # ggsave(paste0("plots/ir_subnational_pred_", ag[i], "_", tstamp(),".png"), 
  #        p, width=7.4, height=7.4*map_ratio(r), units="in")
}
```

#### Figure 4A

``` r
r <- readRDS("output/ir_allage_country_pred_20230208.rds")
p <- IR_plot(raster=r)
ggsave(paste0("plots/ir_allage_country_pred_", tstamp(),".png"), p,
       width=7.4, height=7.4*map_ratio(r), units="in")
```

#### Figure 4B

``` r
ir_allage <- readRDS("output/ir_allage_pred_20230208.rds") # 20 km by 20 km 
ir_allage_cntry <- readRDS("output/ir_allage_country_pred_20230208.rds")
shape <- readRDS("data/africa_sub_Sahara_adm0_shp.rds")
afregions <- data.table::fread("data/africa_subregion_country_names.csv")
areas <- unique(shape$NAME_0)

ccmsc <- data.frame(matrix(NA, nrow=length(areas), ncol=6))
names(ccmsc) <- c("country","cellcount", "mean", "sd", "cv", "country_mean")
ccmsc$country <- areas
for (i in seq_along(areas)) {
  poly <- shape[shape$NAME_0 %in% areas[i], ] # SpatialPolygon  
  irgrid <- raster::extract(ir_allage, poly, df = TRUE, cellnumbers = TRUE)
  irgrid_cntry <- raster::extract(ir_allage_cntry, poly, df = TRUE,
                                  cellnumbers = TRUE)
  ccmsc$cellcount[i] <- length(irgrid$cell)
  ccmsc$mean[i] <- mean(irgrid$ir_allage_pred, na.rm=T) 
  ccmsc$sd[i] <- sd(irgrid$ir_allage_pred, na.rm=T)
  ccmsc$cv[i] <- ccmsc$sd[i] / ccmsc$mean[i]
  ccmsc$country_mean[i] <- mean(irgrid_cntry$value, na.rm=T)
}


meansdcv <- ccmsc
summary(meansdcv$cv)
summary(meansdcv$country_mean)

country_abbr <- function(x){
  x[x == "Central African Republic"] <- "CAR"
  x[x == "Congo, Republic of the"] <- "Congo"
  x[x == "Congo, Democratic Republic of the"] <- "DR Congo"
  x[x == "Tanzania, United Republic of"] <- "Tanzania"
  
  return(x)
}
meansdcv$country_abbr <- country_abbr(meansdcv$country)
myPalette <- colorRampPalette(brewer.pal(9, "YlOrBr"))

library(ggrepel)
p <- ggplot(meansdcv, aes(x=cv, y=country_mean, color=country_mean, label=country_abbr))+
  geom_point(size=4)+
  geom_text_repel(color="black", size=4)+
  scale_color_gradientn(trans = "log10", 
                        limits = c(1, 1e4),
                        breaks = c(1, 1e1, 1e2, 1e3, 1e4),
                        colors = myPalette(1e4), 
                        "Incidence rate")+
  labs(y="Mean incidence rate per 100000 person-years", x="Coefficient of variation")+
  theme_bw() +
  theme(legend.position = c(0.9,.85))

# p  
m <- 0.9
ggsave(paste0("plots/ir_allage_country_coeffvar_",
                tstamp(),".png"), p, width=7.4*m, height=7.4*m, units="in")
```

### Summary tables

Country-level incidence rates (mean with 95% confidence intervals)

#### Table S3

``` r
format_mean_95CI <- function(mean_lb_ub, digits) {
  pred <- format(round(mean_lb_ub[[1]], digits=digits), big.mark=",", trim=TRUE)
  lb <- format(round(mean_lb_ub[[2]], digits=digits), big.mark=",", trim=TRUE)
  ub <- format(round(mean_lb_ub[[3]], digits=digits), big.mark=",", trim=TRUE)

  paste0(pred, " (", lb , " - ", ub, ")")
}

pred <- readRDS("output/summary_ir_allage_country_pred_20230208.rds")
lower <- readRDS("output/summary_ir_allage_country_lower_20230208.rds")
upper <- readRDS("output/summary_ir_allage_country_upper_20230208.rds")

estimates <- vector("list", 3) 
estimates[[1]] <- pred$data$IR
estimates[[2]] <- lower$data$IR
estimates[[3]] <- upper$data$IR

tab <- data.frame(Country=pred$data$Area, Overall=NA)
tab$Overall <- format_mean_95CI(estimates, digits = 1)

pred <- readRDS("output/summary_ir_pred_country_20230208.rds")
lower <- readRDS("output/summary_ir_lower_country_20230208.rds")
upper <- readRDS("output/summary_ir_upper_country_20230208.rds")

ir_by_age_meanci <- vector("list", length=4) # 4 age groups 
for (i in 1:4) {
  estimates <- vector("list", 3)
  estimates[[1]] <- pred[[i]]$data$IR
  estimates[[2]] <- lower[[i]]$data$IR
  estimates[[3]] <- upper[[i]]$data$IR
  
  ir_by_age_meanci[[i]] <- format_mean_95CI(estimates, digits = 1)
}
ir_by_age <- do.call("cbind", ir_by_age_meanci)

tab_all <- cbind(tab, ir_by_age)
names(tab_all) <- c("Country", "Overall", "0-1 yo", "2-4 yo", "5-14 yo", "over 14 yo")
tab_all$Country <- country_abbr(tab_all$Country)
fwrite(tab, paste0("output/IR_by_age_overall_country_tab_", tstamp(), ".csv"))
```

#### Table S4

``` r
pred <- readRDS("output/summary_ir_allage_subregion_pred_20230208.rds")
lower <- readRDS("output/summary_ir_allage_subregion_lower_20230208.rds")
upper <- readRDS("output/summary_ir_allage_subregion_upper_20230208.rds")

estimates <- vector("list", 3) 
estimates[[1]] <- pred$data$IR
estimates[[2]] <- lower$data$IR
estimates[[3]] <- upper$data$IR

tab <- data.frame(Subregion=pred$data$Area, IR=NA)
tab$IR <- format_mean_95CI(estimates, digits = 1)
fwrite(tab_all, paste0("output/IR_allage_subregion_tab_", tstamp(), ".csv"))
```

#### Table S5

Expected number of cases by country

``` r
pred <- readRDS("output/summary_case_pred_country_20230208.rds")
lower <- readRDS("output/summary_case_lower_country_20230208.rds")
upper <- readRDS("output/summary_case_upper_country_20230208.rds")

estimates <- vector("list", 3) 
estimates[[1]] <- pred
estimates[[2]] <- lower
estimates[[3]] <- upper

tab <- data.frame(Country=pred[[1]]$data$Area, 
                  Case_0_1y=NA,
                  Case_2_4y=NA,
                  Case_5_14y=NA,
                  Case_over14y=NA,
                  Case_all=NA)

for(i in 1:5) {
  tab[, i+1] <- format_mean_95CI(list(pred[[i]]$data$Case, lower[[i]]$data$Case, upper[[i]]$data$Case), digits=0)
}

data.table::fwrite(tab, paste0("output/case_country_tab_", tstamp(), ".csv"))
```

#### Table S6

Expected number of cases by Africa sub-region

``` r
pred <- readRDS("output/summary_case_pred_subregion_20230208.rds")
lower <- readRDS("output/summary_case_lower_subregion_20230208.rds")
upper <- readRDS("output/summary_case_upper_subregion_20230208.rds")

estimates <- vector("list", 3) 
estimates[[1]] <- pred
estimates[[2]] <- lower
estimates[[3]] <- upper

tab <- data.frame(Subregion=pred[[1]]$data$Area, 
                  Case_0_1y=NA,
                  Case_2_4y=NA,
                  Case_5_14y=NA,
                  Case_over14y=NA,
                  Case_all=NA)

for(i in 1:5) {
  tab[, i+1] <- format_mean_95CI(list(pred[[i]]$data$Case, lower[[i]]$data$Case, upper[[i]]$data$Case), digits=0)
}

data.table::fwrite(tab, paste0("output/case_subregion_tab_", tstamp(), ".csv"))
```

### Supplementary figures

#### Figure S18

Expected incidence rate at the country level

``` r
for(i in 1:length(ag)){
  r <- readRDS(paste0("output/ir_pred_country_", ag[i], "_20230208.rds"))
  # p <- IR_plot(raster=r, color_ramp="RdYlBu", rev=FALSE)
  p <- IR_plot(raster=r) 
  ggsave(paste0("plots/ir_pred_", ag[i], "_", tstamp(),".png"), p, width=7.4, height=7.4*map_ratio(r), units="in")
}
```
