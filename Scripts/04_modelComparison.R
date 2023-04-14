# ====================================================================================================== #
# Description
#
#   Model comparison through resampling techniques (cross validation) and
#   workflow sets (combining feature engineering and models)
#   Run initTracking before this script to initialize the tibble for tracking
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
library(finetune) # race approach
library(bonsai) # lightgbm

library(tictoc)

# ------------------------------------------------------------------------------------------------------ #
# IMPORT AND SOURCES
# ------------------------------------------------------------------------------------------------------ #

load("./Output/01_output.RData")
load("./Output/03_output.RData")
load("./Output/04a_tracking.RData")

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

df_train_split <- training(output_01$data_split)

df_train_split %<>% 
  rows_append(
    y = output_01$df_train_org
  )

# ---- Model Specifications ----------------------------------------------------

# -- xgboost: Paramters have been set based on grid search
xgb_spec <- 
  boost_tree(
    tree_depth = 10,#8, 
    learn_rate = 0.001, 
    loss_reduction = 10^(-5),  #0.05, 
    min_n = 15, #15, 
    sample_size = 0.6, #0.6, 
    trees = 3500#5000
  ) %>% 
  set_mode("classification") %>% 
  set_engine(
    "xgboost",
    lambda = 0.5,#0.02, # L2 reg
    alpha = 0.2, # L1 reg
    colsample_bytree = 0.75,
    counts = FALSE
  )

# -- GLM: Paramters have been set based on grid search
glm_spec <- 
  logistic_reg(
    penalty = 0.2
  ) %>% 
  set_mode("classification") %>% 
  set_engine(
    "glmnet"
  )

# -- Light GBM: Paramters have been set based on grid search
lightgbm_spec <- 
  boost_tree(
    tree_depth = 14,#8, 
    learn_rate = 0.003,#0.1, 
    loss_reduction = 10^(-5),#0.1, 
    min_n = 12,#15, 
    sample_size = 0.5,#0.6, 
    trees = 1075#1000
  ) %>% 
  set_mode("classification") %>% 
  set_engine(
    "lightgbm"
  ) 

# -- GAM: Paramters have been set based on grid search
gam_spec <-
  gen_additive_mod(
    select_features = FALSE,
    adjust_deg_free = 3 
  ) %>% 
  set_mode("classification")

# ---- Workflow set ------------------------------------------------------------

wflow <- 
  workflow_set(
    preproc = 
      list(
        "recipe_base" = output_03$recipe_base,
        #"recipe_noorg" = output_03$recipe_noorg, # performs worse than recipe_base
        "recipe_norm" = output_03$recipe_norm,
        "recipe_mfeng" = output_03$recipe_mfeng,
        #"recipe_mini" = output_03$recipe_mini,
        "recipe_int" = output_03$recipe_int,
        "recipe_spline" = output_03$recipe_spline
      ),
    models = 
      list(
        "xgboost" = xgb_spec,
        "glm" = glm_spec,
        "lightgbm" = lightgbm_spec
      )
  )

wflow_gam <- # GAM cannot be paired with the other recipes, so we need to specify a seprate workflowset
  workflow_set(
    preproc = 
      list(
        "recipe_base" = output_03$recipe_base
      ),
    models = 
      list(
        "gam" = gam_spec
      )
  )

# -- GAM update formula
wflow_gam %<>%
  update_workflow_model(
    id = "recipe_base_gam",
    spec = gam_spec,
    formula =
      target ~
      te(calc, urea) +
      s(osmo, k = 10) +
      s(gravity, k = 10) +
      s(ph, k = 10) +
      s(cond, k = 10)
  )

# -- combine workflows
wflow_all <- 
  bind_rows(
    wflow,
    wflow_gam
  )


# ---- Tune models -------------------------------------------------------------

my_metric_set <- 
  metric_set(
    roc_auc,
    mn_log_loss
  )
  
tic("grid search")
cv_results <- 
  wflow_all %>% 
  workflow_map(
    # fit_resamples is used automatically when no parameters are tagged with tune()
    #"tune_race_anova", # faster, more crude, tuning compared to grid search
    seed = 489321,
    resamples = 
      vfold_cv(
        df_train_split, 
        v = 5,
        strata = target
      ),
    grid = 4,
    control = 
      control_grid( # use control_race to use the race methodology
        save_pred = TRUE,
        parallel_over = NULL, # automatically chooses betweeen "resamples" and "everything"
        save_workflow = TRUE # needs to be TRUE for stacking
      ),
    metrics = my_metric_set
  )
toc()
 
# ---- Compare models ----------------------------------------------------------

# -- Best model
cv_results %>% 
  autoplot(
    select_best = TRUE
  ) + 
  geom_text(
    aes(
      y = mean,
      label = wflow_id
    ), 
    angle = 90, 
    nudge_x = 0.15,
    color = "black",
    size = 3
  ) +
  theme(legend.position = "none")

# -- Parameter values for xgboost
tune_switch <- FALSE
if (tune_switch) {
  
  cv_results %>% 
    autoplot(
      id = "recipe_base_gam",
      metric = "roc_auc"
    )
  
}

# -- Analysis of residuals
cv_best_res <- 
  cv_results %>% 
  collect_predictions() %>% 
  filter(
    wflow_id == "recipe_base_xgboost"
  ) %>% 
  mutate(
    res = ifelse(target == 1, -log(.pred_1), -log(.pred_0))
  )

cv_best_res %>% 
  ggplot(aes(x = res)) +
  geom_histogram() +
  facet_grid(~ target)

# Inspect observations which were severely under- or overpredicted
df_large_res <- 
  cv_best_res %>% 
  filter(
    res > quantile(res, probs = 0.99)
  ) %>% 
  arrange(
    desc(res)
  ) 

df_res_inspect <- 
  df_train_split %>%
  dplyr::slice(df_large_res$.row) %>% 
  unique %>% 
  select(!c(id, target))  

df_res_inspect %>%
  plot_histogram()

# Compare to the EDA


# ---- Track experiment --------------------------------------------------------

exp_results <- 
  tibble(
    "time" = 
      rep_along(
        cv_results$wflow_id, 
        lubridate::now()
      ),
    "wflow_name" = cv_results$wflow_id,
    "val.or.test" = 
      rep_along(
        cv_results$wflow_id, 
        "CV results"
      )
  )

exp_results$recipe <- 
  exp_results$wflow_name %>% 
  purrr::set_names() %>% 
  map(
    ~ extract_preprocessor(
        x = cv_results,
        id = .x
      ) %>% 
      butcher::butcher() # reduce memory demand
  )

exp_results$model.spec <- 
  exp_results$wflow_name %>% 
  purrr::set_names() %>% 
  map(
    ~ extract_spec_parsnip(
        x = cv_results,
        id = .x
      )
  )

exp_results$tuned.par <- 
  exp_results$wflow_name %>% 
  purrr::set_names() %>% 
  map(
    ~ cv_results %>% 
      extract_workflow_set_result(.x) %>% 
      select_best(metric = "roc_auc")
  )

exp_results$metrics <- 
  exp_results$wflow_name %>% 
  purrr::set_names() %>%
  map(
    ~ cv_results %>% 
      workflowsets::rank_results(select_best = TRUE) %>% 
      select(
        wflow_id,
        .metric,
        mean,
        std_err
      ) %>% 
      filter(
        wflow_id == .x
      )
  )

exp_results$description <- 
  rep_along(
    along = exp_results$wflow_name,
    "Tuning parameters"
  )

# -- Append to tracking data
df_expTracking %<>% 
  dplyr::rows_append(
    y = exp_results
  )

# -- save tracking data
save(
  df_expTracking,
  file = "./Output/04a_tracking.RData"
)

# ==== EXPORT ------------------------------------------------------------------------------------------ 

output_04 <- 
  list(
    "my_metric_set" = my_metric_set,
    "cv_results" = cv_results
  )

save(
  output_04,
  file = "./Output/04_output.RData"
)


