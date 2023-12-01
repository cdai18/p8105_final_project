graphs
================
Zihan Wu
2023-12-01

# Monthly Rat Sightings Before and After Rat Czar Office

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

# Rat Sightings Comparison by Borough Before and After Rat Czar Office

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
