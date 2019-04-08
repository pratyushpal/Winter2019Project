################################################################################
#
# promo_code.R
# 
# Purpose: a helper module for the summarize_line.R module. 
#  Provides the promo_code function, which is used in the
#  summarize_line.R module.
# 
# Input: a line chart satisfying the conditions of the input line chart in 
#  summarize_line.R.
# 
# Output: the same line chart with an estimated promo coding for each week.
#  The promo coding is as follows, if we see a price reduction of greater than
#  10% of the base price, we code the week as a promo.
#  The output line chart now has a base_price attribute which contains the promo coding
#  for each week. A clean_base_week attribute is also added, which codes
#  whether or not the dime price in the week matches the estimated base_price.
# 
# Note: This promo coding is essentially just for TPRs, it would not be able to 
#  detect display or feature activity in the current state.
#
################################################################################ 

promo_code <- function(line_chart) {

    promo <- ((line_chart$base_price - line_chart$AvgPrice_dimes) > 0.1 * line_chart$base_price) + 1 - 1
    line_chart$promo <- promo
    
    clean_promo_week <- as.integer((promo == 1) & (line_chart$price_change_code == 0))
    clean_promo_week[1] <- 0
    line_chart$clean_promo_week <- clean_promo_week

    clean_base_week <- as.integer((promo == 0) & (line_chart$base_price == line_chart$AvgPrice_dimes))
    line_chart$clean_base_week <- clean_base_week

    return(line_chart)

}
