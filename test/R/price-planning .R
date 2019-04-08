
# Promo coding

#####################################################################################################################################################################

# Constants 

SKU_LABEL <-"OM.SKU.Name"
UNITS_LABEL <- "Unit.Sales"
ACV_LABEL <- "ACV.Weighted.Distribution"
PRICE_LABEL <- "Average.Price"
ACCOUNT_LABEL <- "OM.Account"
DATE_LABEL <- "OM.Date"
PG_LABEL <- "OM.Branded.PG"
DOLLARS_LABEL <- "X."

#####################################################################################################################################################################


price_plan_model <- function(data, sku_label = SKU_LABEL, units_label = UNITS_LABEL, 
                             acv_label = ACV_LABEL, price_label = PRICE_LABEL, 
                             account_label = ACCOUNT_LABEL, trend=0, dollars_label = DOLLARS_LABEL,
                             date_label = DATE_LABEL, price_array = "null", 
                             pg_label = PG_LABEL, product_groups = "null") {
  # Selecting top product groups
  top_pg <- get_top(low_leveldata, pg_label, DOLLARS_LABEL,3)
  
  # If price_array is null - get an array of suitable prices to run the model with
  # trend is 1 - add a new column for the trend variable onto the data if it doesn't exist already
  # Have a parameter that gives input to pick certain SKUs
  # If product groups is null - get an array of product to groups to run the model over
  # make a dataframe with each product group along with the prices with model summary and
  # the respective coefficients 
  
  if (trend == 0){
    my_model <- lm(log(data[[units_label]]) ~ log(data[[acv_label]]) + factor(data[[price_label]]) + 
                      factor(data[[account_label]]) + factor(data[[sku_label]]))
    
    # print model_diagnostics
    # make a tuple for returning the prices
    
  }

}

my_sum <- function(vec){
  my_vec <- as.numeric(vec)
  returnValue(sum(my_vec, na.rm = TRUE))
}
get_top <- function(data, name_label, metric_label, number = 5) {
  # returns data frame
  
 sum_df <- aggregate(data[[metric_label]], data[name_label], my_sum)
 # metric label column will default to x 
 sum_df <- sum_df[order(sum_df$x, decreasing = TRUE),]
 print(sum_df)
 
 return(as.character(sum_df[[name_label]][1:number]))
}

low_leveldata <- subset(my_data, my_data$Lower.Level == 1)
output <- get_top(low_leveldata, ACCOUNT_LABEL, DOLLARS_LABEL,3)
print(output)
output <- get_top(low_leveldata, PG_LABEL, DOLLARS_LABEL,3)
print(output)

account <- match(as.character(my_data$OM.Account), ACCOUNT_NAME)
sku <- match(as.character(my_data$OM.SKU.Name), SKU_NAME)

len <- length(account)

sku_acc <- sum(sku, account, na.rm=TRUE)
tapply(seq_along(sku_acc), sku_acc, max)
tapply(seq_along(sku_acc), sku_acc, min)

