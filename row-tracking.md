Newcastle ’97ers rowing performance tracking
================

``` r
library(tidyverse)
library(googlesheets4)
library(lme4)
library(lubridate)
library(janitor)
library(skimr)
library(here)

gs_url <- "https://docs.google.com/spreadsheets/d/1GxCmTZ8E1-y-wfgvKNHqYh4cazBaKz2s1CS8alvqQ-Q/edit#gid=935753699"
rowing_data <- read_sheet(gs_url, sheet = "Erg Times") %>% clean_names()

skim(rowing_data)
```

|                                                  |             |
|:-------------------------------------------------|:------------|
| Name                                             | rowing_data |
| Number of rows                                   | 71          |
| Number of columns                                | 6           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |             |
| Column type frequency:                           |             |
| character                                        | 1           |
| numeric                                          | 4           |
| POSIXct                                          | 1           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |             |
| Group variables                                  | None        |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| rower         |         0 |             1 |   4 |   6 |     0 |        4 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |     sd |  p0 |     p25 |  p50 |  p75 | p100 | hist  |
|:--------------|----------:|--------------:|--------:|-------:|----:|--------:|-----:|-----:|-----:|:------|
| erg_no        |         0 |             1 |    2.07 |   1.09 |   1 |    1.00 |    2 |    3 |    6 | ▇▃▁▁▁ |
| time_mins     |         0 |             1 |    9.23 |   3.51 |   3 |    6.31 |   12 |   12 |   12 | ▃▁▂▁▇ |
| distance      |         0 |             1 | 2313.11 | 803.84 | 833 | 1832.50 | 2731 | 2995 | 3535 | ▆▁▅▇▇ |
| stroke_rate   |         0 |             1 |   22.94 |   4.42 |  14 |   21.00 |   22 |   26 |   30 | ▂▅▇▅▆ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2023-10-09 | 2023-11-27 | 2023-10-31 |       11 |

``` r
calculate_days_between <- function(data) {
  data %>%
    arrange(rower, date) %>%
    group_by(rower) %>%
    mutate(days_between_sessions = date - lag(date, default = first(date)))
}

calculate_speed <- function(data) {
  data %>% mutate(speed = distance / time_mins)
}

calculate_efficiency <- function(data) {
  data %>% mutate(efficiency = distance / stroke_rate)
}

calculate_workload <- function(data) {
  data %>% mutate(workload = speed * erg_no)
}

calculate_burnout_index <- function(data) {
  data %>% mutate(burnout_index = workload / days_between_sessions - efficiency)
}

calculate_days_between <- function(data) {
  data %>%
    arrange(rower, date) %>%
    group_by(rower) %>%
    mutate(days_between_sessions = as.numeric(difftime(date, lag(date, default = first(date)), units="days")))
}

calculate_relative_improvement <- function(data) {
  data %>%
    group_by(rower) %>%
    mutate(baseline_speed = first(speed),
           relative_improvement = (speed - baseline_speed) / baseline_speed * 100)
}

processed_data <- rowing_data %>%
  calculate_days_between() %>%
  calculate_speed() %>%
  calculate_efficiency() %>%
  calculate_workload() %>%
  calculate_burnout_index() %>%
  calculate_relative_improvement()

# skim(processed_data)
```

``` r
summary_stats <- processed_data %>%
  group_by(rower) %>%
  summarise(
    avg_speed = mean(speed, na.rm = TRUE),
    avg_efficiency = mean(efficiency, na.rm = TRUE),
    total_distance = sum(distance, na.rm = TRUE),
    total_time = sum(time_mins, na.rm = TRUE),
    avg_burnout_index = mean(burnout_index, na.rm = TRUE)
  )

print(summary_stats)
```

    ## # A tibble: 4 × 6
    ##   rower  avg_speed avg_efficiency total_distance total_time avg_burnout_index
    ##   <chr>      <dbl>          <dbl>          <dbl>      <dbl>             <dbl>
    ## 1 Hamish      258.          143.           10828       42.8               Inf
    ## 2 Joel        266.           94.4          45147      175.                Inf
    ## 3 Josh        247.          110.           48359      201.                Inf
    ## 4 Rory        256.          119.           59897      237.                Inf

``` r
ggplot(processed_data, aes(x = date, y = burnout_index, color = rower)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Burnout Index Over Time",
       y = "Burnout Index",
       x = "Date") +
  theme_minimal()
```

![](row-tracking_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggplot(processed_data, aes(x = date, y = relative_improvement, color = rower)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Relative Improvement Over Time",
       y = "Improvement (%)",
       x = "Date") +
  theme_minimal()
```

![](row-tracking_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggplot(processed_data, aes(x = date, y = efficiency, color = rower)) +
  geom_point() +
  geom_smooth(se = FALSE) +  # 'se = FALSE' removes the shading around the trend line.
  labs(title = "Stroke Efficiency Over Time",
       y = "Efficiency (distance/stroke rate)",
       x = "Date") +
  theme_minimal()
```

![](row-tracking_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggplot(processed_data, aes(x = date, y = speed, color = rower)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Performance Score over Time",
       y = "Speed (m/min)",
       x = "Date") +
  theme_minimal()
```

![](row-tracking_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# Mixed-Effects Model
speed_model <- lmer(speed ~ days_between_sessions + (1 + days_between_sessions|rower), data=processed_data)
```

    ## boundary (singular) fit: see ?isSingular

``` r
speed_model <- lmer(speed ~ days_between_sessions + (1|rower), data=processed_data)
summary(speed_model)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: speed ~ days_between_sessions + (1 | rower)
    ##    Data: processed_data
    ## 
    ## REML criterion at convergence: 631
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4969 -0.5473 -0.1145  0.7343  1.9466 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  rower    (Intercept)  52.29    7.232  
    ##  Residual             445.58   21.109  
    ## Number of obs: 71, groups:  rower, 4
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error t value
    ## (Intercept)           256.45126    4.75905  53.887
    ## days_between_sessions   0.06179    0.63744   0.097
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## dys_btwn_ss -0.269

``` r
# Visualizations

# Individual trajectories of speed over time
ggplot(processed_data, aes(x=days_between_sessions, y=speed, color=rower)) +
  geom_line() +
  labs(title="Individual Speed Trajectories Over Time", x="Days Between Sessions", y="Speed")
```

![](row-tracking_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# Boxplots for speed, efficiency, and workload
metrics_list <- list("Speed" = processed_data$speed,
                    "Efficiency" = processed_data$efficiency,
                    "Workload" = processed_data$workload)

for(metric_name in names(metrics_list)) {
  print(ggplot(processed_data, aes(x=rower, y=metrics_list[[metric_name]], fill=rower)) +
    geom_boxplot() +
    labs(title=paste(metric_name, "by Rower"), x="Rower", y=metric_name))
}
```

![](row-tracking_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->![](row-tracking_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->![](row-tracking_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

``` r
# Scatter plot of speed vs efficiency
ggplot(processed_data, aes(x=speed, y=efficiency, color=rower)) +
  geom_point() +
  labs(title="Speed vs. Efficiency", x="Speed", y="Efficiency")
```

![](row-tracking_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->
