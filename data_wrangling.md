Data Wrangling
================
Ziang Niu
2025-11-06

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(lubridate)
```

``` r
file_path <- "raw_data/202509-citibike-tripdata_3.csv"
na_values <- c(".", "NA", "")

bike_data <- read_csv(file_path, na = na_values) %>% 
  janitor::clean_names() %>% 
  mutate(
    datetime_obj = ymd_hms(started_at), 
    start_year = year(datetime_obj),
    start_month = month(datetime_obj),
    start_day = day(datetime_obj),
    start_time = format(datetime_obj, "%H:%M:%S")
  ) %>% 
  mutate(
    datetime_obj = ymd_hms(ended_at), 
    end_year = year(datetime_obj),
    end_month = month(datetime_obj),
    end_day = day(datetime_obj),
    end_time = format(datetime_obj, "%H:%M:%S")
  ) %>% 
  select(-started_at, -ended_at)
```

    ## Rows: 1000000 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
bike_data_light <- bike_data %>%
  filter(start_day >= 1 & start_day <= 30) %>%
  group_split(start_day) %>%
  map_dfr(function(group_df) {
    current_day <- group_df$start_day[1]
    if (current_day == 1) {
      n_to_sample <- 3343
    } else {
      n_to_sample <- 3333
    }
    slice_sample(group_df, n = n_to_sample, replace = FALSE)
  }) %>%
  mutate(
    start_time_obj = hms(start_time),
    end_time_obj = hms(end_time),
    diff_in_seconds = end_time_obj - start_time_obj,
    riding_time_raw = as.numeric(diff_in_seconds, units = "hours"),
    riding_time = ifelse(riding_time_raw < 0, riding_time_raw + 24, riding_time_raw),
    start_time_obj = NULL,
    end_time_obj = NULL,
    diff_in_seconds = NULL,
    riding_time_raw = NULL
  )

write.csv(bike_data_light, file = "data/bike_data_light.csv", row.names = FALSE)
```
