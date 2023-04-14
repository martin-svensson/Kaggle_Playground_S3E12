# ====================================================================================================== #
# Description
#
#   Script for stacking model candidates from 04_modelComparison
#
# Change log:
#   Ver   Date        Comment
#   1.0   11/04/23    Initial version
#
# ====================================================================================================== #
# ------------------------------------------------------------------------------------------------------ #
# LIBRARIES
# ------------------------------------------------------------------------------------------------------ #

library(data.table)
library(tidyverse)
library(magrittr)

library(tidymodels)
library(bonsai) # lightgbm
library(stacks)

# ------------------------------------------------------------------------------------------------------ #
# IMPORT AND SOURCES
# ------------------------------------------------------------------------------------------------------ #

load("./Output/04_output.RData")

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

model_stack <- 
  stacks() %>% 
  add_candidates(output_04$cv_results)

set.seed(845635)

model_stack_ens <- 
  blend_predictions(
    model_stack,
    penalty = 10^seq(-3, 0, length = 20)
  )

# -- Compare penalty values
autoplot(
  model_stack_ens
)

# -- See member coefficients
autoplot(
  model_stack_ens,
  "weights"
)

# ==== EXPORT ------------------------------------------------------------------------------------------ 

output_04b <- 
  list(
    "model_stack" = model_stack,
    "model_stack_ens" = model_stack_ens
  )

save(
  output_04b,
  file = "./Output/04b_output.RData"
)
