# ====================================================================================================== #
# Description
#
#   Feature engineering: implementing findings from EDA
#
# Change log:
#   Ver   Date        Comment
#   1.0   05/04/23    Initial version
#
# ====================================================================================================== #
# ------------------------------------------------------------------------------------------------------ #
# LIBRARIES
# ------------------------------------------------------------------------------------------------------ #

library(data.table)
library(tidyverse)
library(magrittr)

library(tidymodels)

# ------------------------------------------------------------------------------------------------------ #
# IMPORT AND SOURCES
# ------------------------------------------------------------------------------------------------------ #

load("./Output/01_output.RData")

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

df_train_split <- training(output_01$data_split)

df_train_split %<>% 
  rows_append(
    y = output_01$df_train_org
  )

# ---- Feature Engineering -----------------------------------------------------

# -- Recipe - base -------------------------------------------------------------
#    Minimal feature engineering - all following recipes builds on this one

recipe_base <- 
  recipe(
    formula = target ~ .,
    data = df_train_split
  ) %>% 
  # -- update roles
  update_role(
    id,
    new_role = "id"
  ) 


# -- Recipe - no original data ----------------------------------------------------------------

recipe_noorg <- 
  recipe_base %>% 
  step_filter(original == 0)


# -- Recipe - norm -----------------------------------------------------------

recipe_norm <- 
  recipe_base %>% 
  step_normalize(
    all_double_predictors()
  )

if (FALSE) {
  
  rec_norm_test <- 
    recipe_norm %>% 
    prep() %>% 
    bake(df_train_split) %>% 
    as.data.table
  
}


# -- Recipe - manual feateng ----------------------------------------------------
#    Manual feature engineering based on domain knowledge

recipe_mfeng <- 
  recipe_base %>%
  # -- Normalize first to ensure predictors are on the same scale
  step_normalize(
    all_double_predictors()
  ) %>% 
  step_mutate(
    calc_urea_ratio = calc / urea,
    gravity_osm_ratio = gravity / osmo,
    calc_osm_product = calc * osmo,
    gravity_cond_product = gravity * cond,
    calc_gravity_ratio = calc / gravity,
    urea_gravity_ratio = urea / gravity,
    osm_cond_product = osmo * cond,
    calc_osm_ratio = calc / osmo,
    urea_osm_ratio = urea / osmo,
    gravity_urea_product = gravity * urea
  )

if (FALSE) {
  
  rec_mfeng_test <- 
    recipe_mfeng %>% 
    prep() %>% 
    bake(df_train_split) %>% 
    as.data.table
  
}



# -- Recipe - minimal --------------------------------------------------------
#    Avoid overfitting by selecting only a few predictors based on EDA

recipe_mini <- 
  recipe(
    formula = target ~ calc + urea,
    data = df_train_split
  ) %>% 
  step_interact(
    terms = ~ calc:urea
  )

# -- Recipe - interact -------------------------------------------------------

recipe_int <- 
  recipe_base %>% 
  step_normalize(
    all_double_predictors()
  ) %>% 
  step_interact(
    terms = ~ all_predictors():all_predictors()
  ) %>% 
  step_normalize(
    all_double_predictors()
  ) 
  
if (FALSE) {
  
  rec_int_test <- 
    recipe_int %>% 
    prep() %>% 
    bake(df_train_split) %>% 
    as.data.table
  
}


# -- Recipe - nonlinear terms ------------------------------------------------

recipe_spline <- 
  recipe_base %>% 
  step_ns(
    all_double_predictors(),
    deg_free = 4
  )

if (FALSE) {
  
  rec_int_spline <- 
    recipe_spline %>% 
    prep() %>% 
    bake(df_train_split) %>% 
    as.data.table
  
}


# ==== EXPORT ------------------------------------------------------------------------------------------ 

output_03 <- 
  list(
    "recipe_base" = recipe_base,
    "recipe_noorg" = recipe_noorg,
    "recipe_norm" = recipe_norm,
    "recipe_mfeng" = recipe_mfeng,
    "recipe_mini" = recipe_mini,
    "recipe_int" = recipe_int,
    "recipe_spline" = recipe_spline
  )  

save(
  output_03,
  file = "./Output/03_output.RData"
)
