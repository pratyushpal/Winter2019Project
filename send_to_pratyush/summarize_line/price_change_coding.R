################################################################################
#
# price_change_coding.R
# 
# Purpose: a helper module for the summarize_line.R module. 
#  Provides the price_change_coding function, which is used in the
#  summarize_line.R module.
# 
# Input: a line chart satisfying the conditions of the input line chart in 
#  summarize_line.R.
# 
# Output: the same line chart with a price change coding. The price change coding
#  is as follows: -1 if the price decreased past a given threshold from the previous
#  week to the current week, 0 if no change occurred, and 1 if the price increased
#  past a threshold from the previous week to the current week. The threshold is currently
#  0, but can be changed.
#
################################################################################ 

source("date_processing.R", chdir=T)

price_change_coding <- function(line_chart) {
    
    num_weeks <- length(line_chart[,1])
    
    price_change <- numeric(num_weeks)
    price_change[1] <- 0
    
    for (i in 2:num_weeks) {
      prev_week <- line_chart[i-1,]
      cur_week <- line_chart[i,]
      price_change[i] <- cur_week$AvgPrice_dimes - prev_week$AvgPrice_dimes
    }
    
    price_change_threshold <- 0
    
    price_change_code <- (price_change >= -price_change_threshold) - 1 + (price_change > price_change_threshold)
    
    line_chart$price_change <- price_change
    line_chart$price_change_code <- price_change_code
    return(line_chart)

}
