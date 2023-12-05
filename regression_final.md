regression_final
================
Zihan Wu
2023-12-04

### Data Loading and Preprocessing

``` r
# Load the rat sightings data
rat_data <- read_csv("data/rat_sightings.csv")
```

    ## Rows: 232417 Columns: 38
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (26): Created Date, Closed Date, Agency, Agency Name, Complaint Type, De...
    ## dbl  (5): Unique Key, X Coordinate (State Plane), Y Coordinate (State Plane)...
    ## lgl  (7): Vehicle Type, Taxi Company Borough, Taxi Pick Up Location, Bridge ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Convert 'Created Date' to datetime format and extract year and month
rat_data$Created_Date <- as.Date(rat_data$`Created Date`, format="%m/%d/%Y %H:%M:%S")
rat_data$Year <- year(rat_data$Created_Date)
rat_data$Month <- month(rat_data$Created_Date)

# Define start dates for Rat Czar and COVID-19 pandemic
rat_czar_start_date <- as.Date("2023-04-12")
pandemic_start_date <- as.Date("2020-03-11")

# Creating binary variables for pre and post Rat Czar and COVID-19 periods
rat_data$Pre_Rat_Czar <- ifelse(rat_data$Created_Date < rat_czar_start_date, 1, 0)
rat_data$Post_Rat_Czar <- ifelse(rat_data$Created_Date >= rat_czar_start_date, 1, 0)
rat_data$Pre_Covid <- ifelse(rat_data$Created_Date < pandemic_start_date, 1, 0)
rat_data$During_Covid <- ifelse(rat_data$Created_Date >= pandemic_start_date & rat_data$Created_Date < rat_czar_start_date, 1, 0)
rat_data$Post_Covid <- ifelse(rat_data$Created_Date >= rat_czar_start_date, 1, 0)

# Aggregate data by month and year for rat sightings
monthly_sightings <- rat_data %>%
  group_by(Year, Month) %>%
  summarise(Sightings = n())
```

    ## `summarise()` has grouped output by 'Year'. You can override using the
    ## `.groups` argument.

``` r
# Merge with original data to include binary variables
merged_data <- merge(monthly_sightings, unique(rat_data[,c("Year", "Month", "Pre_Rat_Czar", "Post_Rat_Czar", "Pre_Covid", "During_Covid", "Post_Covid")]), by = c("Year", "Month"))
```

### Regression Analysis

#### Impact of Rat Czar Appointment

``` r
model_rat_czar <- lm(Sightings ~ Pre_Rat_Czar, data = merged_data)
summary(model_rat_czar)
```

    ## 
    ## Call:
    ## lm(formula = Sightings ~ Pre_Rat_Czar, data = merged_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -940.87 -383.83  -99.83  389.17 1669.17 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    2285.9      199.1  11.481  < 2e-16 ***
    ## Pre_Rat_Czar   -936.0      204.0  -4.589 8.72e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 563.1 on 167 degrees of freedom
    ## Multiple R-squared:  0.112,  Adjusted R-squared:  0.1067 
    ## F-statistic: 21.06 on 1 and 167 DF,  p-value: 8.719e-06

#### Impact of COVID-19 Pandemic

``` r
model_covid <- lm(Sightings ~ Pre_Covid + During_Covid, data = merged_data)
summary(model_covid)
```

    ## 
    ## Call:
    ## lm(formula = Sightings ~ Pre_Covid + During_Covid, data = merged_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -983.68 -313.76  -60.87  331.24 1086.32 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    2285.9      164.8  13.872  < 2e-16 ***
    ## Pre_Covid     -1116.1      170.1  -6.563 6.44e-10 ***
    ## During_Covid   -353.2      181.3  -1.948   0.0531 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 466.1 on 166 degrees of freedom
    ## Multiple R-squared:  0.3953, Adjusted R-squared:  0.388 
    ## F-statistic: 54.26 on 2 and 166 DF,  p-value: < 2.2e-16

### Interpretation of Regression Analysis Results

#### Impact of Rat Czar Appointment

The regression analysis indicates a significant increase in rat
sightings following the appointment of the Rat Czar. The model, with an
R-squared value of 0.112, explains approximately 11.2% of the variance
in rat sightings. The significant negative coefficient for
`Pre_Rat_Czar` (-936, p \< 0.01) suggests that there were, on average,
936 fewer sightings per month before the Rat Czar’s appointment compared
to the period after. This finding could imply that the measures
implemented by the Rat Czar were not immediately effective in reducing
rat sightings, or other factors might have contributed to an increase in
sightings during this period.

#### Impact of COVID-19 Pandemic

The COVID-19 pandemic also appears to have had a significant impact on
rat sightings. The model’s R-squared value of 0.3953 indicates that it
explains about 39.53% of the variance in rat sightings. The coefficients
for `Pre_Covid` and `During_Covid` are -1116.1 (p \< 0.01) and -353.2 (p
= 0.0531), respectively. This suggests that there were significantly
fewer sightings during the pre-COVID and during-COVID periods compared
to the post-COVID period, with a more pronounced decrease in the
pre-COVID era. The marginal significance of the `During_Covid`
coefficient suggests a slight reduction in sightings during the
pandemic, though this result is less certain.

#### Overall Conclusion

These findings underscore the complex nature of urban wildlife dynamics,
particularly in response to significant events such as the appointment
of a public official or a global pandemic. While the analyses provide
valuable insights, they also highlight the need for further research,
incorporating additional variables and data, to fully understand the
factors influencing rat sightings in urban environments.

### Bootstrapping of Rat Czar

``` r
# Define the statistic function for bootstrapping with only Pre_Rat_Czar
stat_function <- function(data, indices) {
  d <- data[indices, ] # allows bootstrapping to select sample
  fit <- lm(Sightings ~ Pre_Rat_Czar, data = d)
  return(coef(fit))
}

# Apply the bootstrapping with R = 1000 bootstrap replicates
results <- boot(data = merged_data, statistic = stat_function, R = 1000)

# Print the results
print(results)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = merged_data, statistic = stat_function, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##      original     bias    std. error
    ## t1* 2285.8750  0.4169434    167.5870
    ## t2* -936.0427 -2.5392402    172.5642

### Interpretation of Bootstrap Results

The bootstrapping process was applied to the regression analysis of rat
sightings with respect to the Rat Czar appointment. The bootstrap
statistics provide an insight into the stability and variability of the
estimated regression coefficients.

#### Bootstrap Results:

- **Intercept (Post_Rat_Czar period) Coefficient**:
  - **Original**: 2285.875
  - **Bias**: -5.886
  - **Standard Error**: 173.254
- **Pre_Rat_Czar Coefficient**:
  - **Original**: -936.043
  - **Bias**: 3.779
  - **Standard Error**: 176.354

#### Conclusions from Bootstrap Analysis:

The bootstrap analysis shows that the bias for both coefficients is
small, which suggests that the estimates from our original model are
stable across different samples of our data. The standard errors from
the bootstrap procedure provide a measure of the variability in the
estimates and can be used to construct more robust confidence intervals
that do not rely on the normality assumption of the residuals.

### Cross-Validation of Rat Czar

``` r
# Define control using k-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Train the model
model <- train(Sightings ~ Pre_Rat_Czar + Post_Rat_Czar,
               data = merged_data,
               trControl = train_control,
               method = "lm")
```

    ## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,
    ## : There were missing values in resampled performance measures.

``` r
# Summarize the results
print(model)
```

    ## Linear Regression 
    ## 
    ## 169 samples
    ##   2 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 152, 152, 152, 152, 153, 151, ... 
    ## Resampling results:
    ## 
    ##   RMSE     Rsquared   MAE     
    ##   561.037  0.1806282  458.1055
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

### Bootstrap Analysis for COVID-19 Impact

``` r
# Define the statistic function for bootstrapping
stat_function_covid <- function(data, indices) {
  d <- data[indices, ] # allows bootstrapping to select sample
  fit <- lm(Sightings ~ Pre_Covid + During_Covid, data = d)
  return(coef(fit))
}

# Apply the bootstrapping with R = 1000 bootstrap replicates
bootstrap_results_covid <- boot(data = merged_data, statistic = stat_function_covid, R = 1000)

# Print the results
print(bootstrap_results_covid)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = merged_data, statistic = stat_function_covid, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##       original    bias    std. error
    ## t1*  2285.8750 -8.203824    171.9859
    ## t2* -1116.1108  9.569559    178.2595
    ## t3*  -353.1908  7.824894    197.9294

### Cross-Validation for COVID-19 Impact

``` r
# Define the control using 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Train the model
model_cv_covid <- train(Sightings ~ Pre_Covid + During_Covid,
                        data = merged_data,
                        trControl = train_control,
                        method = "lm")

# Summarize the results
print(model_cv_covid)
```

    ## Linear Regression 
    ## 
    ## 169 samples
    ##   2 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 152, 151, 152, 150, 153, 153, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared   MAE     
    ##   465.3739  0.4217767  379.1727
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

### Interpretation of Bootstrap and Cross-Validation Results for COVID-19 Impact Analysis

We performed a bootstrap analysis and cross-validation to assess the
stability and predictive performance of our regression model, which
estimates the impact of the COVID-19 pandemic on rat sightings.

#### Bootstrap Results for COVID-19 Analysis

The bootstrap analysis provides an estimate of the variability of the
regression coefficients:

- **Intercept (Post-COVID period) Coefficient**:
  - **Original**: 2285.875
  - **Bias**: -3.056
  - **Standard Error**: 172.188
- **Pre_COVID Coefficient**:
  - **Original**: -1116.111
  - **Bias**: 3.621
  - **Standard Error**: 175.594
- **During_COVID Coefficient**:
  - **Original**: -353.191
  - **Bias**: 1.357
  - **Standard Error**: 193.458

The small biases suggest that the coefficient estimates are robust and
not highly sensitive to the specific sample of data used. The standard
errors reflect the variability in the coefficients and can be used to
construct confidence intervals. The findings confirm that there were
significantly fewer rat sightings during the pre-COVID and during-COVID
periods compared to the post-COVID period.

#### Cross-Validation Results for COVID-19 Analysis

Cross-validation was used to assess the model’s predictive performance.
The results are as follows:

- **RMSE (Root Mean Squared Error)**: 470.6385
- **R-squared**: 0.3918
- **MAE (Mean Absolute Error)**: 382.339

The RMSE and MAE provide measures of the model’s prediction error, with
lower values indicating better fit. The R-squared value indicates that
approximately 39.18% of the variance in rat sightings is explained by
the model across the cross-validated datasets.

These results suggest that while the model has a moderate explanatory
power, there is still a considerable amount of variability in rat
sightings that the model does not capture. This could be due to other
factors not included in the model or inherent variability in the data.

Overall, the analysis suggests that the COVID-19 pandemic had a
statistically significant impact on rat sightings, with changes in human
behavior during the pandemic likely contributing to fluctuations in rat
sighting reports.
