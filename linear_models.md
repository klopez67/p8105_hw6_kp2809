HW6
================
Kimberly Lopez
2024-11-24

# Problem 1

# Problem 2

Import data. Create a city_state variable (e.g. “Baltimore, MD”), and a
binary variable indicating whether the homicide is solved. Omit cities
Dallas, TX; Phoenix, AZ; and Kansas City, MO – these don’t report victim
race. Also omit Tulsa, AL – this is a data entry mistake. For this
problem, limit your analysis those for whom victim_race is white or
black. Be sure that victim_age is numeric.

- open case: 0
- closed case: 1

``` r
homicide_data = 
  read_csv("data/homicide-data.csv")|>
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
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
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
  dplyr::select(term, OR, conf.low, conf.high) |> 
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

Load and clean the data for regression analysis (i.e. convert numeric to
factor where appropriate, check for missing data, etc.).

``` r
bwt_df = 
  read_csv("data/birthweight.csv") |> 
  janitor::clean_names()|>
  mutate( 
    babysex = 
        case_match(babysex,
            1 ~ "male",
            2 ~ "female"),
    babysex = fct_infreq(babysex),
    frace =  case_match(frace,
            1 ~ "white",
            2 ~ "black", 
            3 ~ "asian", 
            4 ~ "puerto rican", 
            8 ~ "other"),
    frace = fct_infreq(frace), 
    mrace = case_match(mrace,
            1 ~ "white",
            2 ~ "black", 
            3 ~ "asian", 
            4 ~ "puerto rican",
            8 ~ "other"),
    mrace = fct_infreq(mrace), 
    malform = as.logical(malform))
```

    ## Rows: 4342 Columns: 20
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (20): babysex, bhead, blength, bwt, delwt, fincome, frace, gaweeks, malf...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
colSums(is.na(bwt_df))
```

    ##  babysex    bhead  blength      bwt    delwt  fincome    frace  gaweeks 
    ##        0        0        0        0        0        0        0        0 
    ##  malform menarche  mheight   momage    mrace   parity  pnumlbw  pnumsga 
    ##        0        0        0        0        0        0        0        0 
    ##    ppbmi     ppwt   smoken   wtgain 
    ##        0        0        0        0

Propose a regression model for birthweight. This model may be based on a
hypothesized structure for the factors that underly birthweight, on a
data-driven model-building process, or a combination of the two.
Describe your modeling process and show a plot of model residuals
against fitted values – use add_predictions and add_residuals in making
this plot.

``` r
set.seed(123)
```

**Cross Validation**

``` r
bwt_df= 
  bwt_df|>
   mutate(id = row_number())

train_df = sample_frac(bwt_df, size = .8)
test_df = anti_join(bwt_df, train_df, by = "id")

x = model.matrix(bwt ~ ., train_df)[,-1]
y = train_df |> pull(bwt)
```

**Lasso Method**

``` r
lambda = 10^(seq(-2, 2.75, 0.1))

lasso_fit =
  glmnet(x, y, lambda = lambda)

lasso_cv =
  cv.glmnet(x, y, lambda = lambda)

lambda_opt = lasso_cv[["lambda.min"]]

lasso_fit_opt = 
  glmnet(x,y, lambda = lambda_opt)

lasso_fit_opt|>
  broom::tidy()
```

    ## # A tibble: 19 × 5
    ##    term               step   estimate lambda dev.ratio
    ##    <chr>             <dbl>      <dbl>  <dbl>     <dbl>
    ##  1 (Intercept)           1 -5922.      0.794     0.720
    ##  2 babysexfemale         1    31.5     0.794     0.720
    ##  3 bhead                 1   126.      0.794     0.720
    ##  4 blength               1    77.8     0.794     0.720
    ##  5 delwt                 1     1.59    0.794     0.720
    ##  6 fincome               1     0.0805  0.794     0.720
    ##  7 fracepuerto rican     1   -39.2     0.794     0.720
    ##  8 gaweeks               1    11.8     0.794     0.720
    ##  9 malformTRUE           1    22.7     0.794     0.720
    ## 10 menarche              1    -4.59    0.794     0.720
    ## 11 mheight               1     5.30    0.794     0.720
    ## 12 momage                1    -0.739   0.794     0.720
    ## 13 mraceblack            1  -117.      0.794     0.720
    ## 14 mracepuerto rican     1   -44.3     0.794     0.720
    ## 15 mraceasian            1   -30.9     0.794     0.720
    ## 16 parity                1    87.1     0.794     0.720
    ## 17 smoken                1    -4.46    0.794     0.720
    ## 18 wtgain                1     2.54    0.794     0.720
    ## 19 id                    1    -0.0214  0.794     0.720

``` r
lasso_fit_opt |> 
  broom::tidy() |> 
  filter(term != "(Intercept)")|>
  pull(term)
```

    ##  [1] "babysexfemale"     "bhead"             "blength"          
    ##  [4] "delwt"             "fincome"           "fracepuerto rican"
    ##  [7] "gaweeks"           "malformTRUE"       "menarche"         
    ## [10] "mheight"           "momage"            "mraceblack"       
    ## [13] "mracepuerto rican" "mraceasian"        "parity"           
    ## [16] "smoken"            "wtgain"            "id"

``` r
model_1 = lm(bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + mheight + mrace + parity + smoken + wtgain, data = train_df)

summary(model_1)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
    ##     frace + gaweeks + mheight + mrace + parity + smoken + wtgain, 
    ##     data = train_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1112.6  -184.9    -3.5   173.1  2408.4 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -6082.0663   153.2811 -39.679  < 2e-16 ***
    ## babysexfemale        33.0409     9.4628   3.492 0.000486 ***
    ## bhead               128.1989     3.8632  33.185  < 2e-16 ***
    ## blength              76.6653     2.2722  33.741  < 2e-16 ***
    ## delwt                 1.5883     0.2600   6.110 1.11e-09 ***
    ## fincome               0.2708     0.1968   1.376 0.168883    
    ## fraceblack           41.8841    51.5298   0.813 0.416381    
    ## fracepuerto rican   -38.4964    48.9366  -0.787 0.431535    
    ## fraceasian           12.8246    78.4931   0.163 0.870225    
    ## fraceother           -8.0798    77.3762  -0.104 0.916840    
    ## gaweeks              12.0633     1.6413   7.350 2.47e-13 ***
    ## mheight               5.7657     1.9915   2.895 0.003813 ** 
    ## mraceblack         -176.4426    51.3786  -3.434 0.000601 ***
    ## mracepuerto rican   -67.0625    49.6501  -1.351 0.176879    
    ## mraceasian          -62.0470    81.6059  -0.760 0.447113    
    ## parity               97.3413    40.4631   2.406 0.016195 *  
    ## smoken               -4.6392     0.6531  -7.104 1.47e-12 ***
    ## wtgain                2.3629     0.4799   4.923 8.90e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 272.9 on 3456 degrees of freedom
    ## Multiple R-squared:  0.7177, Adjusted R-squared:  0.7163 
    ## F-statistic: 516.8 on 17 and 3456 DF,  p-value: < 2.2e-16

``` r
bwt_df_predictions = 
  bwt_df |>
  modelr::add_residuals(model_1) |>
  modelr::add_predictions(model_1)

# Plot residuals vs fitted values
bwt_df_predictions |>
  ggplot(aes(x = pred, y = resid)) + 
  geom_point(alpha=.2)+ 
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  labs(title = "Plot of Model Residuals vs. Fitted values")
```

![](linear_models_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

I used the lasso method to identify important predictors for the outcome
`bwt`. Since we have multiple coefficients in this data set, I used the
lasso method as it adds a penalty on the sum of all coefficients to
balance overall fit and coefficient size in the model. The lasso method
returned coefficients which were deemed important to predicting the
outcome.

Compare your model to two others:

- One using length at birth and gestational age as predictors (main
  effects only)
- One using head circumference, length, sex, and all interactions
  (including the three-way interaction) between these

``` r
model_2= lm(bwt ~ blength + gaweeks , data = train_df)
model_3= lm(bwt ~  bhead + blength + babysex + bhead*blength*babysex, data = train_df )
```

Make this comparison in terms of the cross-validated prediction error;
use crossv_mc and functions in purrr as appropriate.

``` r
cv_df = crossv_mc(bwt_df,100)

cv_df |> 
  pull(train) |> 
  nth(1)|>
  as.tibble()
```

    ## Warning: `as.tibble()` was deprecated in tibble 2.0.0.
    ## ℹ Please use `as_tibble()` instead.
    ## ℹ The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## # A tibble: 3,473 × 21
    ##    babysex bhead blength   bwt delwt fincome frace gaweeks malform menarche
    ##    <fct>   <dbl>   <dbl> <dbl> <dbl>   <dbl> <fct>   <dbl> <lgl>      <dbl>
    ##  1 female     34      51  3629   177      35 white    39.9 FALSE         13
    ##  2 male       34      48  3062   156      65 black    25.9 FALSE         14
    ##  3 female     36      50  3345   148      85 white    39.9 FALSE         12
    ##  4 female     34      52  3374   156       5 white    41.6 FALSE         13
    ##  5 male       33      52  3374   129      55 white    40.7 FALSE         12
    ##  6 female     33      46  2523   126      96 black    40.3 FALSE         14
    ##  7 female     33      49  2778   140       5 white    37.4 FALSE         12
    ##  8 male       36      52  3515   146      85 white    40.3 FALSE         11
    ##  9 male       33      50  3459   169      75 black    40.7 FALSE         12
    ## 10 female     35      51  3317   130      55 white    43.4 FALSE         13
    ## # ℹ 3,463 more rows
    ## # ℹ 11 more variables: mheight <dbl>, momage <dbl>, mrace <fct>, parity <dbl>,
    ## #   pnumlbw <dbl>, pnumsga <dbl>, ppbmi <dbl>, ppwt <dbl>, smoken <dbl>,
    ## #   wtgain <dbl>, id <int>

``` r
cv_df |> 
  pull(test) |> 
  nth(1) |> 
  as_tibble()
```

    ## # A tibble: 869 × 21
    ##    babysex bhead blength   bwt delwt fincome frace gaweeks malform menarche
    ##    <fct>   <dbl>   <dbl> <dbl> <dbl>   <dbl> <fct>   <dbl> <lgl>      <dbl>
    ##  1 male       34      52  3062   157      55 white    40   FALSE         14
    ##  2 male       35      51  3459   146      55 white    39.4 FALSE         12
    ##  3 male       35      56  3232   147      55 white    42.1 FALSE         13
    ##  4 female     35      57  3374   147      45 white    39.6 FALSE         12
    ##  5 male       34      49  3175   148      96 black    39.7 FALSE         10
    ##  6 female     33      49  2948   129      25 white    41   FALSE         13
    ##  7 female     35      52  3289   135      55 white    40.6 FALSE         13
    ##  8 male       38      53  3799   167      75 white    39.9 FALSE         12
    ##  9 female     35      53  3600   141      35 white    42.3 FALSE         14
    ## 10 female     33      49  3147   140      45 white    40.6 FALSE         12
    ## # ℹ 859 more rows
    ## # ℹ 11 more variables: mheight <dbl>, momage <dbl>, mrace <fct>, parity <dbl>,
    ## #   pnumlbw <dbl>, pnumsga <dbl>, ppbmi <dbl>, ppwt <dbl>, smoken <dbl>,
    ## #   wtgain <dbl>, id <int>

``` r
cv_df =
  cv_df |> 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble))|>
  mutate(
    model_1= map(train, \(df) lm(bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + menarche + malform + mheight + momage + mrace + parity + smoken + wtgain, data =df)), 
    model_2= map(train, \(df) lm(bwt ~ blength + gaweeks , data = df)),
    model_3 = map(train, \(df) lm(bwt ~  bhead + blength + babysex + bhead*blength*babysex, data = df )))|>
  mutate(
    rmse_m1 = map2_dbl(model_1, test, \(mod, df) rmse(model = mod, data = df)),
    rmse_m2 = map2_dbl(model_2, test, \(mod, df) rmse(model = mod, data = df)),
    rmse_m3 = map2_dbl(model_3, test, \(mod, df) rmse(model = mod, data = df)))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `rmse_m1 = map2_dbl(model_1, test, function(mod, df) rmse(model
    ##   = mod, data = df))`.
    ## Caused by warning in `predict.lm()`:
    ## ! prediction from rank-deficient fit; attr(*, "non-estim") has doubtful cases

Plotting the prediction error to compare models

``` r
cv_df |> 
  dplyr::select(starts_with("rmse"))|> 
  pivot_longer(
    everything(),
    names_to = "model", 
    values_to = "rmse",
    names_prefix = "rmse_") |> 
  mutate(model = fct_inorder(model)) |> 
  ggplot(aes(x = model, y = rmse)) + geom_violin()
```

![](linear_models_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

This plot shows that model 1 had a lower Root Mean Squared Error
compared to Model 2 and 3 and therefore consistently out predicts the
model 2 of `blength` and `gaweeks` and 3 which uses `bhead`, `blength`,
`babysex`, and their interaction term.
