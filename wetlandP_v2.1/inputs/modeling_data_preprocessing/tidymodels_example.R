library(tidymodels)
#> ── Attaching packages ─────────────────────────────────────────── tidymodels 0.1.1 ──
#> ✓ broom     0.7.0      ✓ recipes   0.1.13
#> ✓ dials     0.0.8      ✓ rsample   0.0.7 
#> ✓ dplyr     1.0.0      ✓ tibble    3.0.3 
#> ✓ ggplot2   3.3.2      ✓ tidyr     1.1.0 
#> ✓ infer     0.5.3      ✓ tune      0.1.1 
#> ✓ modeldata 0.0.2      ✓ workflows 0.1.2 
#> ✓ parsnip   0.1.2      ✓ yardstick 0.0.7 
#> ✓ purrr     0.3.4
#> ── Conflicts ────────────────────────────────────────────── tidymodels_conflicts() ──
#> x purrr::discard() masks scales::discard()
#> x dplyr::filter()  masks stats::filter()
#> x dplyr::lag()     masks stats::lag()
#> x recipes::step()  masks stats::step()

## some example data to use
data("hpc_data")

hpc_data <- hpc_data %>%
  select(-protocol, -class)

lm_mod <-
  linear_reg(mode = "regression") %>%
  set_engine("lm")

wf <-
  workflow() %>%
  add_model(lm_mod)

## big function of model fitting and predicting
predict_hpc <- function(df) {
  split <- initial_split(df)
  train_df <- training(split)
  test_df <- testing(split)
  
  #create recipe
  recipe_train <-
    recipe(compounds ~., data = train_df) %>%
    step_normalize(all_predictors())
  
  #fit workflow on train data
  fit_wf <-
    wf %>%
    add_recipe(recipe_train) %>%
    fit(data = train_df)
  
  #predict on test data
  predict(fit_wf, test_df) 
  
}



hpc_nested <- hpc_data %>%
  group_by(day) %>%
  nest()

hpc_nested %>%
  mutate(predictions = map(data, possibly(predict_hpc, otherwise = NA)))
#> Timing stopped at: 0.001 0 0.001
#> # A tibble: 7 x 3
#> # Groups:   day [7]
#>   day   data               predictions       
#>   <fct> <list>             <list>            
#> 1 Tue   <tibble [900 × 5]> <tibble [225 × 1]>
#> 2 Thu   <tibble [720 × 5]> <tibble [180 × 1]>
#> 3 Fri   <tibble [923 × 5]> <tibble [230 × 1]>
#> 4 Wed   <tibble [903 × 5]> <tibble [225 × 1]>
#> 5 Mon   <tibble [692 × 5]> <tibble [173 × 1]>
#> 6 Sat   <tibble [32 × 5]>  <lgl [1]>         
#> 7 Sun   <tibble [161 × 5]> <tibble [40 × 1]>
