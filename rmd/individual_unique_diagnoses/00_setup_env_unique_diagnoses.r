## Setup environment
pacman::p_load(
  DBI, 
  data.table, 
  dtplyr, 
  lubridate, 
  ggplot2, 
  here, 
  stringr, 
  tidyr, 
  tidyverse, 
  qwraps2, 
  zoo, 
  tidyverse
)

library(dplyr, warn.conflicts = FALSE)
options(lubridate.fasttime = TRUE)

source(here("src", "utils.r"))
source(here("src", "functions.r"))
source(here("src", "individual_unique_diagnoses_functions.r"))
source(here("src", "ggplot_defaults.r"))

test_nesting_dfs <- list()
test_nesting_dfs$one <- df_table
test_nesting_dfs$two <- df_unique_in_period