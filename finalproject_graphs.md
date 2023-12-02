graphs
================
Zihan Wu
2023-12-01

## Monthly Rat Sightings Before and After Rat Czar Office

### Data Loading

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
# Converting 'Created Date' to datetime format
rat_data$Created_Date <- mdy_hms(rat_data$`Created Date`)

# Assuming the Rat Czar started office on April 12, 2023
rat_czar_start_date <- as.Date("2023-04-12")

# Filtering data for the period from one year before the Rat Czar started to the latest date in the dataset
one_year_before <- rat_czar_start_date - years(1)
latest_date <- max(rat_data$Created_Date)

data_after_one_year_before_rat_czar <- filter(rat_data, Created_Date >= one_year_before & Created_Date <= latest_date)

# Grouping by month and converting to Date format
data_after_one_year_before_rat_czar$Month <- as.Date(floor_date(data_after_one_year_before_rat_czar$Created_Date, "month"))
monthly_counts_after <- data_after_one_year_before_rat_czar %>%
  group_by(Month) %>%
  summarise(Count = n())
```

### Data Visualization

``` r
# Plotting with ggplot2
rat_czar_start_month <- as.Date("2023-04-01") # The first day of the month when Rat Czar started

ggplot(monthly_counts_after, aes(x = Month, y = Count)) +
  geom_col(fill = "skyblue") +
  geom_vline(xintercept = as.numeric(rat_czar_start_month), linetype="dashed", color = "red", size=1.5) +
  geom_text(aes(x = rat_czar_start_month, y = max(monthly_counts_after$Count), label = "Rat Czar Start"), 
            color = "red", vjust = -1, angle = 90, size = 4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  labs(title = 'Monthly Rat Sightings from April 2022 to November 2023',
       x = 'Month',
       y = 'Number of Sightings') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: Use of `monthly_counts_after$Count` is discouraged.
    ## ℹ Use `Count` instead.

![](finalproject_graphs_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Interpretation of the Graph

The bar graph above illustrates the number of reported rat sightings
from April 2022 to November 2023. The red dashed line marks the
beginning of the Rat Czar’s term in April 2023.

A cursory glance at the graph suggests that there is variability in the
number of sightings reported each month, which may be influenced by a
variety of factors including seasonal changes and the urban environment.
While there is no immediate drastic reduction in sightings following the
appointment of the Rat Czar, this could imply that strategies employed
require time to take effect, or that their impact is not instantly
observable from this data alone.

## Rat Sightings Comparison by Borough Before and After Rat Czar Office

### Rat Sightings Comparison by Borough Before and After Rat Czar Office

``` r
# Assuming 'rat_data' has already been loaded and 'Created_Date' has been processed

# Grouping data by borough and month
sightings_by_borough_month <- rat_data %>%
  filter(Created_Date >= one_year_before & Created_Date <= latest_date) %>%
  mutate(Month = as.Date(floor_date(Created_Date, "month"))) %>%
  group_by(Borough, Month) %>%
  summarise(Count = n(), .groups = 'drop')


# Define a custom color palette without red for the boroughs
borough_colors <- c("BRONX" = "#33a02c", "BROOKLYN" = "#1f78b4", "MANHATTAN" = "#b2df8a",
                    "QUEENS" = "#a6cee3", "STATEN ISLAND" = "#fdbf6f", "Unspecified" = "#cab2d6")

# Plotting with ggplot2 using the custom color palette
ggplot(sightings_by_borough_month, aes(x = Month, y = Count, fill = Borough)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = borough_colors) + # Use the custom color palette
  geom_vline(xintercept = as.numeric(as.Date("2023-04-01")), linetype="dashed", color = "#d62728", size=1.5) + # Red line for Rat Czar start date
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  labs(title = 'Monthly Rat Sightings by Borough from April 2022 to November 2023',
       x = 'Month',
       y = 'Number of Sightings',
       fill = 'Borough') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom")
```

![](finalproject_graphs_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Interpretation of the Graph

The graph displays monthly rat sightings by borough in New York City
from April 2022 to November 2023. Each borough is represented by a
different color, with an additional category for unspecified locations.
A red dashed line denotes April 2023, when the Rat Czar commenced their
role.

Interpreting the graph, we can observe that sightings fluctuate monthly,
with peaks and troughs that may be influenced by various factors, such
as changes in weather or waste management practices. After the red line,
indicating the Rat Czar’s start date, there is no immediate and drastic
change in the trend of sightings for any borough. This could suggest
that any measures implemented by the Rat Czar may take time to show
significant effects.

Additionally, it’s noticeable that certain boroughs, such as Brooklyn
and Manhattan, consistently report higher numbers of sightings. The
graph also shows that some months exhibit a higher number of sightings
across all boroughs, which might indicate seasonal patterns in rat
activity or reporting.

Lastly, the unspecified category indicates reports that did not have a
borough listed. The presence of this category suggests that data
collection or reporting may have inconsistencies, which could affect the
analysis.

Overall, while the Rat Czar’s impact isn’t immediately discernible from
the graph, ongoing analysis with more data over time will be essential
to understand the full effect of the initiatives taken to control the
rat population.

## Rat Sightings and Covid-19

The COVID-19 pandemic brought unprecedented challenges to urban
management and public health. In New York City, changes in human
activity during the pandemic may have influenced rat behavior and
sightings. This section analyzes the rat sightings data to observe
potential shifts in rat populations across the city’s boroughs during
the pre-pandemic, pandemic, and post-pandemic periods.

### Defining Pandemic Periods for Analysis

``` r
# Define the dates for the COVID-19 pandemic periods in New York City
pre_pandemic_start <- as.Date("2019-01-01")
pre_pandemic_end <- as.Date("2020-02-29")
pandemic_start <- as.Date("2020-03-01")
pandemic_end <- as.Date("2021-12-31")
post_pandemic_start <- as.Date("2022-01-01")

# Classify each sighting by pandemic period
rat_data$Pandemic_Period <- case_when(
  rat_data$Created_Date < pandemic_start ~ "Pre-Pandemic",
  rat_data$Created_Date >= pandemic_start & rat_data$Created_Date <= pandemic_end ~ "During Pandemic",
  rat_data$Created_Date > pandemic_end ~ "Post-Pandemic",
  TRUE ~ NA_character_
)
```

### Visualizing Rat Sightings during Different Pandemic Period

General Plot (Overall Trends): \* Objective: To observe the overall
trend of rat sightings in New York City during different periods of the
COVID-19 pandemic. \* Method: Aggregate the total number of rat
sightings for each pandemic period across all boroughs. Then, normalize
this data by calculating the average daily sightings to account for the
varying lengths of each period. \* Plot Type: A bar chart where each bar
represents a different pandemic period (Pre-pandemic, During Pandemic,
Post-pandemic), showing the average daily number of sightings.

``` r
# Calculate the number of days in each pandemic period
num_days_pre_pandemic <- as.numeric(pandemic_start - pre_pandemic_start)
num_days_during_pandemic <- as.numeric(pandemic_end - pandemic_start + 1)
num_days_post_pandemic <- as.numeric(Sys.Date() - post_pandemic_start)

# Aggregate the counts of rat sightings by pandemic period and calculate average daily sightings
sightings_by_period_overall <- rat_data %>%
  group_by(Pandemic_Period) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  filter(!is.na(Pandemic_Period)) %>%
  mutate(Avg_Daily_Sightings = Count / case_when(
    Pandemic_Period == "Pre-Pandemic" ~ num_days_pre_pandemic,
    Pandemic_Period == "During Pandemic" ~ num_days_during_pandemic,
    Pandemic_Period == "Post-Pandemic" ~ num_days_post_pandemic
  ))

# Plot the data showing average daily sightings for each pandemic period
ggplot(sightings_by_period_overall, aes(x = Pandemic_Period, y = Avg_Daily_Sightings, fill = Pandemic_Period)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Pre-Pandemic" = "#6baed6", "During Pandemic" = "#fd8d3c", "Post-Pandemic" = "#74c476")) +
  labs(title = 'Average Daily Rat Sightings During Different COVID-19 Pandemic Periods',
       x = 'Pandemic Period',
       y = 'Average Daily Sightings',
       fill = 'Pandemic Period') +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](finalproject_graphs_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Interpretation of Rat Sightings During COVID-19 Pandemic Periods

The bar chart above provides a comparison of average daily rat sightings
in New York City across three distinct periods related to the COVID-19
pandemic: during, post, and pre-pandemic. Notably, the average daily
sightings were highest before the pandemic, which may reflect normal
urban activity and waste production providing ample food sources for
rats. Sightings dipped during the pandemic, likely due to the closure of
restaurants and reduced pedestrian traffic, which may have limited food
availability for the rat population. Although sightings have increased
in the post-pandemic period, they have not returned to pre-pandemic
levels, suggesting a lasting impact of the pandemic on rat sightings or
possibly continued changes in human behavior and waste management.

### Visualizing Rat Sightings by Borough during Different Pandemic Period

By Borough Plot (Specific Trends): \* Objective: To compare rat sighting
trends across different boroughs of New York City during the COVID-19
pandemic. \* Method: Group the data by both borough and pandemic period.
Calculate the average daily sightings for each borough in each period.
This approach reveals if some boroughs experienced more significant
changes in rat sightings than others during the pandemic. \* Plot Type:
A grouped bar chart with each group representing a borough and each bar
within the group representing a different pandemic period. This
visualization allows for a direct comparison of sighting trends across
boroughs and periods.

``` r
# Aggregate the counts of rat sightings by borough and pandemic period, then calculate average daily sightings
sightings_by_borough_period <- rat_data %>%
  group_by(Borough, Pandemic_Period) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  filter(!is.na(Pandemic_Period)) %>%
  mutate(Avg_Daily_Sightings = Count / case_when(
    Pandemic_Period == "Pre-Pandemic" ~ num_days_pre_pandemic,
    Pandemic_Period == "During Pandemic" ~ num_days_during_pandemic,
    Pandemic_Period == "Post-Pandemic" ~ num_days_post_pandemic
  ))

# Plot the data showing average daily sightings by borough for each pandemic period
ggplot(sightings_by_borough_period, aes(x = Borough, y = Avg_Daily_Sightings, fill = Pandemic_Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Pre-Pandemic" = "#6baed6", "During Pandemic" = "#fd8d3c", "Post-Pandemic" = "#74c476")) +
  labs(title = 'Average Daily Rat Sightings by Borough During Different Pandemic Periods',
       x = 'Borough',
       y = 'Average Daily Sightings',
       fill = 'Pandemic Period') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom")
```

![](finalproject_graphs_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Interpretation of Average Daily Rat Sightings by Borough During Pandemic Periods

The bar chart above contrasts the average daily rat sightings across New
York City boroughs before, during, and after the COVID-19 pandemic.
Brooklyn stands out with the highest average sightings in the
pre-pandemic period, indicating a substantial rat population or
heightened reporting activity. A decline in sightings is noted during
the pandemic across all boroughs, likely a result of the pandemic’s
disruptions to normal urban life and its impact on rat foraging
behaviors. The post-pandemic period sees a continuation of this reduced
level of sightings, suggesting potential long-term effects of the
pandemic on rat activity or the success of pest control measures
implemented during this time. The data also includes an ‘Unspecified’
category, denoting sightings that could not be allocated to a particular
borough, highlighting a need for better data classification in future
analyses.