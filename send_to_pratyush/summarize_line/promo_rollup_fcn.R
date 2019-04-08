################################################################################
#
# promo_rollup_fcn.R
# 
# Purpose: a helper module for the summarize_line.R module. 
#  Provides the promo_rollup_fcn function, which is used in the
#  summarize_line.R module.
# 
# Input: a line chart satisfying the conditions of the input line chart in 
#  summarize_line.R.
# 
# Output: a data frame that summarizes promotional activity for the SKU/packtype
#  at the current account.
#
################################################################################ 

promo_rollup_fcn <- function(line_chart) {

    num_weeks <- length(line_chart[,1])
    promo_rollup <- data.frame(sp = c(0), base_price = c(0), weeks = c(0), promo_start_month = c(0), promo_end_month = c(0))
    promo_weeks <- 0
    promo_sp <- -1
    promo_base_price <- -1
    promo_start_month <- -1
    promo_end_month <- -1
    
    for (i in 1:num_weeks) {
      cur_week <- line_chart[i,]
      
      if(cur_week$promo == 1) {
        # update the number of promo weeks at the observed promo price.
        promo_weeks <- promo_weeks + 1
        
        if(promo_start_month == -1) {
          # update the promo start month if this hasn't already been updated.
          month <- get_month_num(get_date(cur_week$Week))
          promo_start_month <- month
        }
        
        if (cur_week$clean_promo_week == 1) {
          # update the promo strike price if we have a clean promo week.
          promo_sp <- cur_week$AvgPrice_dimes
        }
        if (promo_base_price == -1) {
          # update the base price
          promo_base_price <- cur_week$base_price
        }
        
      } else if (cur_week$promo == 0 & promo_weeks > 0) {
        
        # If we've been tracking a promo that ends, update the end month, and add the promo event to the data frame.
        promo_end_month <- get_month_num(get_date(cur_week$Week))
        promo_rollup <- rbind(promo_rollup, c(promo_sp, promo_base_price, promo_weeks, promo_start_month, promo_end_month))
        # reset other attributes.
        promo_weeks <- 0
        promo_sp <- -1
        promo_base_price <- -1
        promo_start_month <- -1
        promo_end_month <- -1
        
      }

      # If our line chart ends with the SKU/packtype on promo, add the currently running promo event to the data frame.
      if (i == num_weeks & cur_week$promo == 1) {
        promo_rollup <- rbind(promo_rollup, c(promo_sp, promo_base_price, promo_weeks, promo_start_month, promo_end_month))
      }
      # Want to add to this module: Check for price changes to deeper promo. This would make the module less simplistic, and able to handle more cases.
    }
    
    promo_rollup <- promo_rollup[-1,]
    return(promo_rollup)

}
