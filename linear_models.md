Linear Models
================
Kimberly Lopez
2024-11-24

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)
library(ggplot2)
```

# Problem 1

# Problem 2

Import data. Create a city_state variable (e.g. “Baltimore, MD”), and a
binary variable indicating whether the homicide is solved. Omit cities
Dallas, TX; Phoenix, AZ; and Kansas City, MO – these don’t report victim
race. Also omit Tulsa, AL – this is a data entry mistake. For this
problem, limit your analysis those for whom victim_race is white or
black. Be sure that victim_age is numeric.

- open case: 0 -closed case: 1

``` r
homicide_data = 
  read.csv("data/homicide-data.csv")|>
  janitor::clean_names()|>
  mutate(
    city_state = paste(city,state, sep=", "),
    status_bin= ifelse(disposition=="Open/No arrest", 0, 1), 
    victim_race = fct_relevel(victim_race, "White"), 
    victim_sex = fct_relevel(victim_sex, "Female")
  )|>
  filter(!city_state %in% c("Dallas, TX", "Phoenix, AZ", "Kansas City, MO", "Tulsa, AL"))|>
  filter(victim_race =="White" | victim_race =="Black"  )|>
  filter(!victim_age =="Unknown")|>
  mutate(victim_age = as.numeric(victim_age))

class(homicide_data[["victim_age"]])
```

    ## [1] "numeric"

``` r
class(homicide_data[["victim_sex"]])
```

    ## [1] "factor"

For the city of Baltimore, MD, use the glm function to fit a logistic
regression with resolved vs unresolved as the outcome and victim age,
sex and race as predictors. Save the output of glm as an R object; apply
the broom::tidy to this object; and obtain the estimate and confidence
interval of the adjusted odds ratio for solving homicides comparing male
victims to female victims keeping all other variables fixed.

``` r
baltimore_df = 
  homicide_data|>
  filter(city_state == "Baltimore, MD")

model = 
  baltimore_df|>
  glm(status_bin ~ victim_age + victim_sex + victim_race, family = "binomial", data=_)

model|> 
  broom::tidy(conf.int=TRUE) |> 
  mutate(OR = exp(estimate)) |>
  select(term, OR, conf.low, conf.high) |> 
  knitr::kable(digits = 3)
```

| term             |    OR | conf.low | conf.high |
|:-----------------|------:|---------:|----------:|
| (Intercept)      | 4.776 |    1.090 |     2.051 |
| victim_age       | 0.995 |   -0.012 |     0.001 |
| victim_sexMale   | 0.355 |   -1.320 |    -0.759 |
| victim_raceBlack | 0.407 |   -1.258 |    -0.550 |

Now run glm for each of the cities in your dataset, and extract the
adjusted odds ratio (and CI) for solving homicides comparing male
victims to female victims. Do this within a “tidy” pipeline, making use
of purrr::map, list columns, and unnest as necessary to create a
dataframe with estimated ORs and CIs for each city.

``` r
nest_lm_res =
  homicide_data |> 
  nest(data = -city_state) |> 
  mutate(
    models = map(data, \(df) lm(status_bin ~ victim_age + victim_sex + victim_race, data = df)),
    results = map(models, broom::tidy, conf.int = TRUE)) |> 
  select(-data, -models) |> 
  unnest(results)

city_OR= 
  nest_lm_res |> 
  filter(term == "victim_sexMale") |> 
  select(city_state, term, estimate, conf.low, conf.high) |> 
  mutate(
    term = fct_inorder(term),
    OR = exp(estimate), 
    conf.low = exp(conf.low),
    conf.high = exp(conf.high)) |> 
  pivot_wider(
    names_from = term, values_from = estimate)

city_OR
```

    ## # A tibble: 47 × 5
    ##    city_state      conf.low conf.high    OR victim_sexMale
    ##    <chr>              <dbl>     <dbl> <dbl>          <dbl>
    ##  1 Albuquerque, NM    0.773     1.03  0.894        -0.112 
    ##  2 Atlanta, GA        0.807     0.957 0.879        -0.129 
    ##  3 Baltimore, MD      0.733     0.833 0.781        -0.247 
    ##  4 Baton Rouge, LA    0.691     0.899 0.788        -0.238 
    ##  5 Birmingham, AL     0.841     1.02  0.927        -0.0757
    ##  6 Boston, MA         0.806     1.06  0.924        -0.0786
    ##  7 Buffalo, NY        0.724     0.946 0.828        -0.189 
    ##  8 Charlotte, NC      0.803     0.938 0.868        -0.142 
    ##  9 Chicago, IL        0.764     0.836 0.800        -0.224 
    ## 10 Cincinnati, OH     0.691     0.855 0.769        -0.263 
    ## # ℹ 37 more rows

Create a plot that shows the estimated ORs and CIs for each city.
Organize cities according to estimated OR, and comment on the plot.

``` r
city_OR |> 
  ggplot(aes(x= reorder(city_state, OR), y = OR))+
  geom_point()  + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
  theme(axis.text.x = element_text(angle = 80, hjust = 1))+ 
  labs(
    x= "City, State", 
    title = "Comparing Males to Females odds of having a Solved Homicide case"
  )
```

![](linear_models_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

This plot shows increasing OR of males compared to females odds of
having their case solved in cities in the US while controling for race
and age. In the city of New York, males have 0.73 the odds of having a
case closed compared to females holding other factors such as age and
race constant. This means that males are less likely to have a closed
homicide and solved case compared to females in NY. However, in Fresno
California, males have 1.02 the odds of having a case closed compared to
females holding all other factors constant. This means males are
slightly more likley to have their case resolved than females.

# Problem 3
