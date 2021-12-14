library(tidyverse)
library(lubridate)
library(changepoint)
library(patchwork)

#### ---------- Detrending functions



#' @title linear_detrend
#' @description Detrends a time series using a linear regression.
#' @param x A time series.
#' @return A detrended time series.
linear_detrend <- function(x){
  m  <- lm(x ~ c(1:length(x)))
  detrended_x <- resid(m)
  return(detrended_x)
}

#' @title spline_detrend
#' @description Detrends a time series using splines.
#' @param x A time series.
#' @param n_knots The number of knots to use in the spline.
#' @return A detrended time series.
spline_detrend <- function(x, n_knots){
  m <- smooth.spline(x, nknots=n_knots)
  detrended_x <- resid(m)
  return(detrended_x)
}

#' @title difference_detrend
#' @description Detrends a time series using lagged difference.
#' @param x A time series.
#' @param lag The number of lags to use in the difference.
#' @return A detrended time series. The first lag values are set to be 
#' equal to the first calculaed value
difference_detrend <- function(x, lag=1){
  x <- diff(x, lag=lag)
##  return(x)
  return(c(rep(NA, lag), x))
}


detrending_methods = list("spline" = spline_detrend,
                          "linear" = linear_detrend,
                          "difference" = difference_detrend)


#### ---------- Change point detection

#' @title estimate_change_points
#' @description Estimates change points using the `changepoint` library. Uses the PELT method and calculates changepoints based on both mean and variance shifts
#' @param x A time series.
#' @return A list of the indices of the change points 
estimate_change_points <- function(x){
  pelt <- cpt.meanvar(x, method="PELT")
  change_points <- cpts(pelt)
  return(change_points)
}

#' @title calculate_changepoints_segments
#' @description Extracts which dates correspond to the change point indices extract by `estimate_change_points` as well as the mean and sd of the stationary segments.
#' @param x A time series.
#' @param change_points A list of the indices of the change points
#' @param time A vector of the time points (dates)
#' @return A tibble containing the start and end dates of the change points and the mean and sd of the stationary segments.
calculate_changepoint_segments <- function(x,
                                           change_points,
                                           time
){
  ## add 1 as the the first change point to indicate beginning of first segment
  change_points <- append(change_points, 1, 0)
  ## remove first element and add the end to indicate the end of the last segment
  end_indices <- c(change_points[-1], length(time)) 
  
  ## calculate the mean value during stable segments (for plotting)
  mean_value_at_change_points <- map_dbl(1:length(change_points), 
                                         function(idx) mean(
                                           x[change_points[idx]:end_indices[idx]]
                                         )
  )
  sd_at_change_points <- map_dbl(1:length(change_points), 
                                 function(idx) sd(
                                   x[change_points[idx]:end_indices[idx]]
                                 )
  )
  
  change_point_segments <- tibble(segment_x_start = as.Date(time[change_points]), 
                                  segment_x_end = as.Date(time[end_indices]),
                                  segment_mean = mean_value_at_change_points,
                                  segment_sd = sd_at_change_points
  )
  return(change_point_segments)
  
}

#### --------- Combined estimation and plotting


#' @title estimate_and_plot_changepoints
#' @description Estimates change points and plots the time series with the change points marked. Optionally detrends the time series before estimating change points, and adds a smoothed line to the plot.
#' @param x A time series.
#' @param time A vector of the time points (dates)
#' @param window_size whether to exclude the first window_size points from the time series (e.g. due to window size in novelty calculation)
#' @param do_detrending whether to detrend the time series before estimating change points
#' @param do_smoothings whether to plot a smoothed line on top of the time series
#' @param detrend_method which method to use for detrending. Options are "linear", "spline", and "difference"
#' @param smoothing_span the degree of smoothing to use. Lower values make less smoothing
#' @param ... optional aruguments for the detrending functions
estimate_and_plot_changepoints <- function(x, 
                              time, 
                              window_size = NULL,
                              do_detrending = TRUE,
                              do_smoothing = TRUE,
                              detrend_method = "difference",
                              smoothing_span=0.75,
                              cp_plot_type = "line",
                              ...
                              ){
  ## remove first window_size observations as they are by definition 0 for novelty/resonance
  if(!is.null(window_size)){
    x <- x[-c(1:window_size)]
    time <-time[-c(1:window_size)]   
  }
  
  if(isTRUE(do_detrending)){
    detrended_x <- detrending_methods[detrend_method][[1]](x, ...)
    if(detrend_method == "difference"){
      lag <- 1 ## hard coded for now to avoid going insane
      time <- time[-c(1:lag)]
      x <- x[-c(1:lag)]
      detrended_x <- detrended_x[-c(1:lag)]
    }
    change_points <- estimate_change_points(detrended_x)
  }
  else{
    change_points <- estimate_change_points(x)
  }
  dat <- tibble(Year = as.Date(time), y = x)
  
  p <- ggplot(data = dat, aes(Year, y)) +
    geom_point(alpha=0.1) 
  
  if(isTRUE(do_smoothing)){
    p <- p +
      geom_smooth(color = "steelblue", span=smoothing_span, alpha=0.2) 
  }
  if(cp_plot_type == "line"){
    change_point_segments <- calculate_changepoint_segments(x, 
                                                            change_points, 
                                                            time)
    p <- p + 
      geom_segment(data=change_point_segments,
                aes(x = segment_x_start,
                    xend = segment_x_end,
                    y = segment_mean,
                    yend = segment_mean),
                size=1.2,
                alpha=0.4)
  }
  if(cp_plot_type == "point"){
    cps <- tibble(value_at_change_point = x[change_points],
                  date_at_change_point = as.Date(time[change_points]))
    
    p <- p +
      geom_point(data=cps,
                 aes(x = date_at_change_point,
                     y = value_at_change_point),
                 color = "#DC863B",
                 shape = 17,
                 size = 2.5)
  }
  

  
  
  return(p)
}
    
#' @title plot_detrended_ts
#' @description Plots a de-trended time series next to the original time series.
#' @param x A time series.
#' @param time A vector of the time points (dates)
#' @param window_size whether to exclude the first window_size points from the time series (e.g. due to window size in novelty calculation)
#' @param detrending_method which detrending method to use. Either "all", "linear, "spline", or "difference"
#' @param do_smoothing whether to plot a smoothed line on top of the time series
#' @param smoothing_span the degree of smoothing to use. Lower values make less smoothing
#' @param ... optional aruguments for the detrending functions
plot_detrended_ts <- function(x,
                              time,
                              window_size = NULL,
                              detrending_method = "all",
                              do_smoothing = TRUE,
                              smoothing_span=0.75,
                              n_knots=4,
                              lag=1){
   if(!is.null(window_size)){
    x <- x[-c(1:window_size)]
    time <-time[-c(1:window_size)]   
  }

  detrending_methods <- if (detrending_method == "all") c("Original", "Linear", "Difference", "Spline") else c("Original", str_to_title(detrending_method))
  
  ## calculate all detrending methods
  detrender <- function(x, n_knots, lag){
    detrended <- tibble(Linear = linear_detrend(x),
                        Spline = spline_detrend(x, n_knots),
                        Difference = difference_detrend(x, lag)
    )
    return(detrended)
  }

  detrended_x <- detrender(x, n_knots, lag)
  dat <- detrended_x %>%
    mutate(Year = as.Date(time), Original = x) %>%
    pivot_longer(!Year) %>%
    mutate(name = fct_relevel(name, "Original", "Linear", "Difference", "Spline")) %>% 
    filter(name %in% detrending_methods)

  p <- ggplot(data = dat, aes(Year, value)) +
    geom_point(alpha=0.1) +
    facet_wrap(~name, scales = "free_y") 

  if(isTRUE(do_smoothing)){
      p <- p +
        geom_smooth(color = "steelblue", span=smoothing_span, alpha=0.2) 
    }

    return(p)
}


#' @title estimate_and_plot_changepoints_by_group
#' 
#' 
#' 
estimate_and_plot_changepoints_by_group <- function(
  df,
  ts_col,
  time_col,
  group_col,
  window_size=NULL,
  do_detrending=TRUE,
  detrend_method="difference",
  ...
){
  

  
  df <- df %>% 
    group_by({{group_col}}) %>% 
    arrange({{time_col}}) 
  
  if(!is.null(window_size)){
    ## because programming with dplyr is so much fun
    time <- df %>% ungroup() %>% select({{time_col}}) %>% distinct() %>% slice(-c(1:window_size)) %>% pull({{time_col}})
    #time <- unique(df[[time_col]])[-c(1:WINDOW_SIZE)]
    
    df <- df %>%
      group_by({{group_col}}) %>% 
      slice({{window_size}}+1:n())
  }
  else{
    time <- df %>% ungroup() %>% select({{time_col}}) %>% distinct() %>% pull({{time_col}})
  }
  

  if(isTRUE(do_detrending)){
    detrend_fn <- detrending_methods[detrend_method][[1]]
    
    df <- df %>%
      mutate({{ts_col}} := detrend_fn({{ts_col}}, ...)) %>% 
      ## dropping the first lag points if detrend_method is difference
      drop_na({{ts_col}})
  }

  df <- df %>% 
     summarise(cps = estimate_change_points({{ts_col}})) %>% 
    mutate(date = .env$time[cps]) %>% 
    ggplot(aes(fct_rev({{group_col}}), date)) +
    geom_point(size = 3, color = "#DC863B", shape = 17) + 
    geom_label(aes(label = strftime(date, "%Y-%m")), vjust=-1) + 
    coord_flip() 
  ###### Expand axes

  
  return(df)
  
}