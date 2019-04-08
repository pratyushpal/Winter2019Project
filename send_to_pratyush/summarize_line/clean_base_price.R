################################################################################
#
# clean_base_price.R
# 
# Purpose: a helper module for the summarize_line.R module. 
#  Provides the clean_base_price function, which is used in the
#  summarize_line.R module.
# 
# Input: a line chart satisfying the conditions of the input line chart in 
#  summarize_line.R.
# 
# Output: the same line chart with an estimate of the base price for each week.
#  The output line chart now has a base_price attribute which contains the base price
#  for each week.
#
################################################################################ 

# include base_week_rollup_fcn to roll up the initial naive base price estimate.
source("base_week_rollup_fcn.R", chdir=T)

clean_base_price <- function(line_chart) {

    num_weeks <- length(line_chart[,1])
  
    base_price <- numeric(0)
    
    # First run a quick naive coding looking at the max price of the previous 13 weeks,
    #  and the max of the next 13 weeks.
    for (i in 1:13) {
      base_price[i] <- max(line_chart$AvgPrice_dimes[1:13])
    }
    
    for (i in 14:num_weeks) {
      base_price[i] <- max(line_chart$AvgPrice_dimes[(i-12):i])
    }
    
    for (i in num_weeks:(num_weeks - 12)) {
      potential_base_price <- max(line_chart$AvgPrice_dimes[(num_weeks - 12):num_weeks])
      
      if (potential_base_price < base_price[i]) {
        base_price[i] <- potential_base_price
      }
    }
    
    for (i in (num_weeks - 13):1) {
      potential_base_price <- max(line_chart$AvgPrice_dimes[i:(i+12)])
      
      if (potential_base_price < base_price[i]) {
        base_price[i] <- potential_base_price
      }
    }
    
    line_chart$base_price <- base_price
    
    # Now roll up the naive coding, which can be checked for oddities.
    initial_base_week_rollup <- base_week_rollup_fcn(line_chart)
    
    events <- length(initial_base_week_rollup[,1])
    
    # If there is more than one base_price point, check for oddities in the coding,
    #  such as the event where there are no clean base weeks within a base event
    #  (i.e., no weeks where the dime price is actually equal to the estimated base price).
    # Update the coding to reflect these odd weeks.
    if(events > 1) {

      for (i in 1:events) {
        
        if (initial_base_week_rollup[i,]$clean_base_weeks == 0) {

          new_base_price <- most_recent_clean_base_price
          start_day <- initial_base_week_rollup[i,]$start_date
          end_day <- initial_base_week_rollup[i,]$end_date
          line_chart$base_price[line_chart$Week >= start_day & line_chart$Week <= end_day] <- new_base_price

        } else {
          most_recent_clean_base_price <- initial_base_week_rollup[i,]$base_price
        }
      }    
    }
    return(line_chart)

}