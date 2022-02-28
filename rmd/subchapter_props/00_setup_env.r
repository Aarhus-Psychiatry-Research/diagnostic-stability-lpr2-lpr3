pacman::p_load(here, DBI, dplyr, lubridate, ggplot2, stringr, tidyr, zoo, tidyverse, scales)
options(lubridate.fasttime = TRUE)

source(here("src", "utils.r"))
source(here("src", "functions.r"))

log_time()