library(dplyr, warn.conflicts = FALSE)
library(DBI)
library(lubridate)
options(lubridate.fasttime=TRUE)

#' Drop visits that are within a given window from the last visit of a patient.
#' Due to the type of problem, this has to iterate over each row, so it's quite intensive.
#' For development, recommend to work on only a subset of your data, e.g. using
#' df %>% slice_sample(prop=0.01) %>% drop_visits_within_break_window(months=3)
#' 
#' 
#' @param df Dataframe to do this on
#' @param months How far away from a previous visit does a visit have to be to not get dropped (num, months)
#' @param print_progress Whether to print how many patients have been processed so far, defaults to false
#' @param verbose Whether to print debugging information
#' @param id_column_name Column name (str) with unique patient IDs, defaults to "dw_ek_borger".
#' @param date_column_name Column name (str) for dates, defaults to datotid_start. Expects the column to be datatype POSIXct. 
#' 
#' @return A new dataframe without the visits within the window
drop_visits_within_break_window <- function(df, 
                                             months, 
                                             print_progress = FALSE,
                                             verbose = FALSE,
                                             id_column_name="dw_ek_borger",
                                             date_column_name="datotid_start") {
  
  print("Sorting visits from first to last, this might take a while (20s for 2M visits)")
  arranged_df <- df %>% arrange(!!as.symbol(id_column_name),
                                !!as.symbol(date_column_name))
  
  current_CPR <- 0
  patient_i <- 0
  last_selected_date <- 0
  indeces_to_drop <- c()
  month_in_seconds <- 30 * 24 * 60 * 60 # Calculate seconds in a month to add to POSIXct.
  
  arranged_df_id_column <- arranged_df[ , id_column_name]
  arranged_df_date_column <- arranged_df[ , date_column_name]

  total_patients <- length(unique(arranged_df_id_column))
  
  for (i in 1:nrow(df)) {
    if (arranged_df_id_column[i] != current_CPR) { # Handle switching to new person
      current_CPR = arranged_df_id_column[i]
      last_selected_date = arranged_df_date_column[i]
      
      if (patient_i %% 100 == 0 ) {
        percent_complete_rounded <- round(patient_i/total_patients*100, digits = 1)
        print(str_c(percent_complete_rounded, "%: Processing patient nr. ", patient_i))
      }
      
      patient_i <- patient_i + 1
      
      next()
    }
    
    if (arranged_df_id_column[i] == current_CPR) { # Handle comparison of current visit to previous selected date
      if (arranged_df_date_column[i] < last_selected_date + month_in_seconds * months) {
        if (verbose == TRUE) {
          print(str_c("\n\nDropping index: ", i, " because ", arranged_df_date_column[i], " is smaller than ", last_selected_date + month_in_seconds))
          print(str_c("Processing visit's date: ", arranged_df_date_column[i]))
          print(str_c("Last prediction visit's date: ", last_selected_date))
        }
        
        indeces_to_drop <- c(indeces_to_drop, i)
      } else {
        last_selected_date <- arranged_df_date_column[i]
      }
    }
  }
  
  return(arranged_df %>% slice(-indeces_to_drop))
}

#' Saves a combined boxplot and histogram png file to 
#' working directory/figures/filename
#' 
#' @param df
#' @param x_var
#' @param filename
#' 
#' @return A ggplot object
gen_stacked_box_and_histogram_png <- function(df, x_var) {
  
  gg <- ggplot(df, aes(x={{x_var}})) +
    scale_x_continuous()
  
  hist <- gg +
    geom_histogram() 
  
  box <- gg +
    geom_boxplot()
  
  combined <- hist + box + plot_layout(nrow = 2, height = c(2, 1))
  
  return(combined)
}

#' Adds a column containing the delta between two dates in years
#' @param df
#' @param start_date Expects POSIXct datatype
#' @param end_date Expects POSIXct datatype
#' @param column_name The new column name
#' 
#' @return A new dataframe of df + the column. Column is positive if end_date is after start_date.
add_column_years_between_dates <- function(df, start_date, end_date, column_name) {
  df_out <- df %>% 
    mutate({{column_name}} := interval({{start_date}}, {{end_date}}) / years(1))
  
  return(df_out)
}

#' Groups by grouping_vars and keeps only the record with the highest value on max_col
#' 
#' @param df
#' @param grouping_vars
#' 
#' @return A df with only the min records. If returns an empty df, beware that you min_col might be of type chr.
#' 
group_and_keep_record_with_min_value_on_col <- function(df, grouping_vars, min_col) {
  df_out <- df %>% 
    group_by({{grouping_vars}}) %>% 
    slice(which.min({{min_col}}))
  
  return(df_out)
}

#' Fix typical issues with the SQL import we get
#' - Convert dates from chr to POSIXct
#' - Remove leading Ds from adiagnosis
#' - Lowercase all column names
#' 
#' @param df (df) The dataframe to do it to
#' 
#' @return A cleaned dataframe
clean_sql_import <- function(df) {
  df <- df %>%
    rename_with(tolower)%>% 
    mutate(across(matches(".*diagnosekode.*"), ~ substr(.x, 2, 99))) %>%  # Remove leading Ds from e.g. DF20
    mutate(across(matches("datotid.+"), ~ ymd_hms(.x))) %>% # Set type of datetime columns to POSIXct
    mutate(across(matches("(.)+dato"), ~ ymd(.x))) # Set type of date-columns to date

  return(df)
}

#' Get all contents from SQL view
#' @param fct The fact name
#' 
#' @return A dataframe of the fct
#' 
get_fct <- function(fct) {
  conn <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = "BI-DPA-PROD",
    database = "USR_PS_Forsk",
    Trusted_Connection = "TRUE"
  )

  return(dbGetQuery(conn, paste0("SELECT * FROM [fct].", fct)))
}

log_time <- function() {
  print(format(Sys.time(), '%d/%m/%y %H:%M:%S'))
}

setup_chunk_observation_window <- function () {
  source("src/how_big_an_observation_window.r")
  log_time()
}