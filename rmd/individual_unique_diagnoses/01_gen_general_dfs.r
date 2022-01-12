df_raw_outpatient <- get_fct("FOR_besoeg_fysiske_fremmoeder_inkl_2021") %>%
  clean_sql_import()

df_raw_lpr3 <- get_fct("FOR_LPR3kontakter_psyk_somatik_inkl_2021") %>%
  clean_sql_import()

df_raw_lpr2_inpatient <- get_fct("FOR_indlaeggelser_psyk_somatik_LPR2_inkl_2021") %>%
  clean_sql_import()

df_outpatient_preprocessed <- df_raw_outpatient %>%
  mutate(no_threshold_constructed_id = paste0(dw_ek_borger, shakafskode_besoeg)) %>%
  keep_only_psych_visits() %>%
  select(
    dw_ek_borger,
    datotid_start,
    adiagnosekode,
    ambbesoeg,
    dw_sk_lpr3forloebsansvar,
    dw_sk_kontakt,
    shakafskode_besoeg,
    no_threshold_constructed_id
  ) %>%
  add_LPR23_quarter_column() %>%
  filter(period > ymd("2012-05-01")) %>% 
  filter(period < ymd("2021-06-01"))

df_outpatient_preprocessed <- write_csv(df_outpatient_preprocessed, here("csv", "df_outpatient_preprocessed.csv"))