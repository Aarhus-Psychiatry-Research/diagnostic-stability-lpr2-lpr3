source("E:/Users/adminmanber/Desktop/FeasibilityMapper/utils.r")

pacman::p_load(DBI, dplyr, lubridate, ggplot2, stringr, tidyr, zoo)
options(lubridate.fasttime = TRUE)

source("E:/Users/adminmanber/Desktop/FeasibilityMapper/utils.r")
source(here::here("src", "functions.r"))

df_raw_outpatient <- get_fct("FOR_besoeg_fysiske_fremmoeder") %>%
  clean_sql_import()

df_raw_lpr3 <- get_fct("FOR_LPR3kontakter_psyk_somatik") %>%
  clean_sql_import()

df_raw_lpr2_inpatient <- get_fct("FOR_indlaeggelser_psyk_somatik_LPR2") %>%
  clean_sql_import()

log_time()