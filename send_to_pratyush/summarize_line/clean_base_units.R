################################################################################
#
# clean_base_units.R
# 
# Purpose: a helper module for the summarize_line.R module. 
#  Provides the clean_base_units function, which is used in the
#  summarize_line.R module.
# 
# Input: a line chart satisfying the conditions of the input line chart in 
#  summarize_line.R.
# 
# Output: the same line chart with estimated base unites for each week.
#  The output line chart now has a base_units attribute which contains the base units
#  for each week. 
#
################################################################################ 

# Note: I have thought about changing this to clean_base_week == 1 coding, not promo == 0

clean_base_units <- function(line_chart) {

    num_weeks <- length(line_chart[,1])
    base_units <- numeric(num_weeks)
    
    for(i in 1:7) {
      if (line_chart$promo[i] == 0) {
        base_units[i] <- line_chart$Units[i]
      } else {
        potential_weeks <- line_chart[1:13,]
        base_week_units <- potential_weeks[potential_weeks$promo == 0, "Units"]
        base_units[i] <- mean(base_week_units)
      }
    }
    
    for (i in 8:(num_weeks - 6)) {
      if (line_chart$promo[i] == 0) {
        base_units[i] <- line_chart$Units[i]
      } else {
        
        potential_weeks <- line_chart[(i-7):(i+6),]
        base_week_units <- potential_weeks[potential_weeks$promo == 0, "Units"]
        base_units[i] <- mean(base_week_units)
      }
    }
    
    for (i in (num_weeks - 5):num_weeks) {
      if (line_chart$promo[i] == 0) {
        base_units[i] <- line_chart$Units[i]
      } else {
        potential_weeks <- line_chart[(num_weeks - 13):num_weeks,]
        base_week_units <- potential_weeks[potential_weeks$promo == 0, "Units"]
        base_units[i] <- mean(base_week_units)
      }
      
    }

    for (i in 1:num_weeks) {
      if (is.na(base_units[i])) {
        base_units[i] <- 0
      }
    }
    
    line_chart$base_units <- base_units
    return(line_chart)

}
