# Setup environment
pacman::p_load(DBI, data.table, dtplyr, lubridate, ggplot2, here, stringr, tidyr, tidyverse, qwraps2, xlsx, zoo)
library(dplyr, warn.conflicts = FALSE)
options(lubridate.fasttime = TRUE)

source("E:/Users/adminmanber/Desktop/FeasibilityMapper/utils.r")
source(here("src", "functions.r"))
source(here("src", "individual_unique_diagnoses_functions.r"))
source(here("src", "ggplot_defaults.r"))

df_raw_outpatient <- get_fct("FOR_besoeg_fysiske_fremmoeder") %>%
  clean_sql_import()

df_raw_lpr3 <- get_fct("FOR_LPR3kontakter_psyk_somatik") %>%
  clean_sql_import()

df_raw_lpr2_inpatient <- get_fct("FOR_indlaeggelser_psyk_somatik_LPR2") %>%
  clean_sql_import()

log_time()