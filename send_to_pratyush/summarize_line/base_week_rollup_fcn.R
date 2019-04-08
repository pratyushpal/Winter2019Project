################################################################################
#
# base_week_rollup.R
# 
# Purpose: a helper module for the summarize_line.R module. 
#  Provides the base_week_rollup_fcn function, which is used in the
#  summarize_line.R module.
# 
# Input: a line chart satisfying the conditions of the input line chart in 
#  summarize_line.R.
# 
# Output: a data frame that summarizes base activity for the SKU/packtype
#  at the current account.
#
################################################################################ 

source("date_processing.R", chdir=T)

# Assume num_weeks > 13
base_week_rollup_fcn <- function(line_chart, performance_metrics = 0) {
  
  acv_threshold <- 10 # Minimum ACV threshold for metrics
  num_weeks <- length(line_chart[,1])
  # initialize data frame
  base_week_rollup <- data.frame(base_price = c(0), clean_base_weeks = c(0), total_weeks = c(0), start_date = c(0), end_date = c(0))
  clean_base_weeks <- 0
  total_weeks <- 0
  base_price <- -1
  base_start_date <- -1
  base_end_date <- -1
  dates_as_months = 0
  
  for (i in 2:num_weeks) {
    last_week <- line_chart[(i-1),]
    cur_week <- line_chart[i,]
    
    if (base_price == -1) {
      # set base_price if it hasn't been set yet
      base_price <- last_week$base_price
      # set start date (in either month or day format)
      # Note: month format is currently not working, so this will only use day format.
      if (dates_as_months) {
        base_start_date <- get_month_num(get_date(last_week$Week))
      } else {
        base_start_date <- last_week$Week
      }
    }
    
    total_weeks <- total_weeks + 1
    if (last_week$AvgPrice_dimes == base_price) {
      clean_base_weeks <- clean_base_weeks + 1
    }
    
    if (last_week$base_price != cur_week$base_price) {
      if (dates_as_months) {
        base_end_date <- get_month_num(get_date(last_week$Week))
      } else {
        base_end_date <- last_week$Week
      }
      
      base_week_rollup <- rbind(base_week_rollup, c(base_price, clean_base_weeks, total_weeks, base_start_date, base_end_date))
      
      clean_base_weeks <- 0
      total_weeks <- 0
      base_price <- -1
      base_start_date <- -1
      base_end_date <- -1
    }
    if (i == num_weeks) {
      if (dates_as_months) {
        base_end_date <- get_month_num(get_date(cur_week$Week))
      } else {
        base_end_date <- cur_week$Week
      }
      
      total_weeks <- total_weeks + 1
      if (cur_week$AvgPrice_dimes == base_price) {
        clean_base_weeks <- clean_base_weeks + 1
      }
      base_week_rollup <- rbind(base_week_rollup, c(base_price, clean_base_weeks, total_weeks, base_start_date, base_end_date))
    }
  }
  
  # Process average_vel for each base event
  
  if (performance_metrics == TRUE) {
    
    average_base_vel <- numeric(0)
    non_promo_units_at_price_point <- numeric(0)
    average_base_units <- numeric(0)
    index <- 1
    
    for (base_price in base_week_rollup$base_price) {
      start_day <- base_week_rollup[index,]$start_date
      end_day <- base_week_rollup[index,]$end_date

      total_non_promo_units <- sum(line_chart$Units[line_chart$AvgPrice_dimes == base_price & line_chart$ACV > acv_threshold & line_chart$Week >= start_day & line_chart$Week <= end_day])
      total_distribution_points <- sum(line_chart$ACV[line_chart$AvgPrice_dimes == base_price & line_chart$ACV > acv_threshold & line_chart$Week >= start_day & line_chart$Week <= end_day])
      if (is.na(total_distribution_points)) {
        total_distribution_points <- 0
      }
      average_base_units[index] <- mean(line_chart$base_units[line_chart$clean_base_week == 1 & line_chart$ACV > acv_threshold & line_chart$Week >= start_day & line_chart$Week <= end_day])

      if (total_distribution_points != 0) {
        average_base_vel[index] <- total_non_promo_units / total_distribution_points
      } else {
        average_base_vel[index] <- 0
      }
      non_promo_units_at_price_point[index] <- total_non_promo_units
      
      index <- index + 1
    }
    
    normalized_volume <- average_base_vel/max(average_base_vel)
    sample <- non_promo_units_at_price_point/sum(non_promo_units_at_price_point)
    
    base_week_rollup$average_base_vel <- average_base_vel
    base_week_rollup$normalized_volume <- normalized_volume
    base_week_rollup$non_promo_units <- non_promo_units_at_price_point
    base_week_rollup$sample <- sample
    base_week_rollup$average_base_units <- average_base_units
    
  }
  base_week_rollup <- base_week_rollup[-1,]
  return(base_week_rollup)
}
