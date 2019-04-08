################################################################################
#
# summarize_line.R
# 
# Purpose: given a line chart of a SKU or packtype at an account,
#  the process_line_chart function contained within the summarize_line.R file
#  codes base weeks, promo weeks, base price and base units,
#  and then attempts to summarize base events and promo events.
# 
# Input: a data frame representing a time series line chart of a SKU or packtype
#  at an account. The following fields must be present in the data frame, and
#  must be named accordingly:
#   - Week -- Needs to be in numerical format
#   - Units
#   - Dollars
#   - ACV
#   - AvgPrice
#  Additionally, there can be NO BLANKS in any of the data columns.
# 
# Output: an R list object with three attributes: line_chart, promo_rollup, and
#  base_week_rollup. promo_rollup and base_week_rollup are data frames that 
#  summarize the promotional and base events for the given SKU or packtype
#  at the given account. line_chart is the input line chart with promo coding,
#  base week coding, base units, and base price.
# 
# Example Usage:
# 
#  infile <- "clif_line_chart.csv"
#  line_chart <- read.csv(infile)
#  processed_line_chart <- process_line_chart(line_chart)
#  new_line_chart <- processed_line_chart$line_chart
#  write.csv(new_line_chart, "out.csv") # write new line chart with codings to a csv
# 
#  print(processed_line_chart$base_week_rollup) # print a summary of the base events
# 
#  print(processed_line_chart$promo_rollup) # print a summary of the promo events
# 
#
################################################################################

# 
library(zoo)

# source other modules
# source("base_week_rollup_fcn.R")
source("price_change_coding.R", chdir=T)
source("clean_base_price.R", chdir=T)
source("promo_code.R", chdir=T)
source("clean_base_units.R", chdir=T)
source("promo_rollup_fcn.R", chdir=T)

process_line_chart <- function(line_chart) {

    line_chart <- line_chart[order(line_chart$Week),]
    line_chart$AvgPrice_dimes <- round(line_chart$AvgPrice + 0.01,1) - 0.01
    num_weeks <- length(line_chart[,1])

    line_chart <- price_change_coding(line_chart)
    line_chart <- clean_base_price(line_chart)
    line_chart <- promo_code(line_chart)
    line_chart <- clean_base_units(line_chart)

    promo_rollup <- promo_rollup_fcn(line_chart)
    base_week_rollup <- base_week_rollup_fcn(line_chart, 1)

    retval <- list()

    retval$promo_rollup <- promo_rollup
    retval$base_week_rollup <- base_week_rollup
    retval$line_chart <- line_chart

    return(retval)

}
