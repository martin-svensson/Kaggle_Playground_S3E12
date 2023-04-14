# ====================================================================================================== #
# Description
#
#   Preprocessing and data split
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

df_train <- fread("./Data/train.csv")
df_train_org <- fread("./Data/train_original.csv")
df_test <- fread("./Data/test.csv")

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

df_train %>% str  
df_train %>% summary

df_train_org %>% str
df_train_org %>% summary

# -- Create ID for original data
#    starts at 1e3 to be able to distinguish from df_train

df_train_org %<>% 
  mutate(
    id = 1e3 + (row_number() - 1)
  ) %>% 
  select(
    id,
    everything()
  )

# -- Create indicator for original data

df_train %<>% 
  mutate(
    original = 0L
  )

df_train_org %<>% 
  mutate(
    original = 1L
  )

df_test %<>% 
  mutate(
    original = 0L
  )


# -- Rename 
#    Nothing to rename

# df_train %<>% 
#   rename_with(.fn = ~ gsub("\\s", "\\.", .x)) 
  

# -- Factor encoding
#    Target is encoded as factor in accordance with tidymodels methodology

vars_factor <-
  c("target")

df_train[
  ,
  (vars_factor) := map(.SD, ~ as_factor(.x)),
  .SDcols = vars_factor
]

df_train_org[
  ,
  (vars_factor) := map(.SD, ~ as_factor(.x)),
  .SDcols = vars_factor
]


# -- Data split
#    We do not inlcude original data in the test  data

set.seed(684236)

data_split <- 
  df_train %>% 
  initial_split(
    prop = 0.8,
    strata = target
  )

# ==== EXPORT ------------------------------------------------------------------------------------------ 

output_01 <- 
  list(
    "df_train" = df_train,
    "df_test" = df_test,
    "df_train_org" = df_train_org,
    "data_split" = data_split
  )

save(
  output_01,
  file = "./Output/01_output.RData"
)

