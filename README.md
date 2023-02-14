Mapping the incidence rate of typhoid fever in sub-Saharan Africa
================
2023-02-10

#### 

Running the scripts below saves csv files in the output directory.

### Log-linear regression

``` r
library(GGally)
res <- loglinear_model()
```

### Poisson regression

``` r
res <- poisson_model()
```

### Negative binomial regression

``` r
res1 <- negbin_model_1()
res2 <- negbin_model_2()
```

### Prepare covariates for prediction

I may change this code simply to raster::resample

``` r
pred_loc <- get_prediction_location()
pred_cov <- get_prediction_covariate()
```

### Forecasting for the log-linear model

``` r
pred <- predict_loglinear_model()
```

### Forecasting for the poissonl model

``` r
pred <- predict_poisson_model()
```

### Forecasting for the NegBin

``` r
pred1 <- predict_negbin_model_1()
pred2 <- predict_negbin_model_2()
```

### Plots

#### Figure 1

#### Figure 2

#### Figure 3

#### Figure 4
