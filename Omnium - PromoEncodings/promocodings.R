# Promo coding

#####################################################################################################################################################################

# Constants 

PLACE_HOLDER_VAL <-  0
DOLLAR_SUBSTRING_VAL <- 2
NOISE_CORRECTION_FACTOR <- 0.2

#####################################################################################################################################################################

create_weighting <- function(x) {
  myweight <- x / sum(x^2, na.rm = TRUE)
  # Want to give higher weight to prices wth lower unit sales since 
  # our intuition says that promo prices have higher unit sales
  myweight <- 1/myweight
  myweight <- myweight/(sum(myweight^2, na.rm=TRUE))
  return(myweight)
} 

create_promo_coding <- function(data, avg_price,unit_sales, account_label,sku_label, product_group,
                                price_format="$",type="all", pg_label = 'OM.Branded.PG',listofPG = "null" ) {
  # Fixing any data formatting issues
  om_data_fix(data)
  
  if(type=="all"){
    my_accounts <- unique(data[[account_label]]) # O(N^2) worst case -> need to improve this
    my_skus <- unique(data[[sku_label]])
    data$base_code <- rep(PLACE_HOLDER_VAL, nrow(data))
    
    #Iterating for each SKU & Account pair
    for(sku in my_skus){
      for(account in my_accounts){
        
        #run promo coding algo
        
        curr_subset <- subset(data, account_label == account, sku_label == sku)
        
        my_weighting <- create_weighting(curr_subset[[unit_sales]])
        my_weighting[is.na(my_weighting)] <- PLACE_HOLDER_VAL
        
        
        # Price and units should be in correct form by now
        
        w_avgprice <- weighted.mean(curr_subset[[avg_price]], my_weighting)
        bool_vec <- curr_subset[[avg_price]] >= w_avgprice - NOISE_CORRECTION_FACTOR
        
        # converting to digital vector
        bool_vec[bool_vec == "TRUE"] <- 1
        bool_vec[bool_vec == "FALSE"] <- 0
        
        data[data$sku_label == sku & data$account_label == account]$base_code <- bool_vec #syntax issue 
      }
    }
    
  }else if(type=='pg'){
    
    if(listofPG == "null"){
      print("No list of product groups has been provided")
    }else{
      data[[pg_label]] <- as.character(data[[pg_label]])
      my_accounts <- as.character(unique(data[[account_label]])) # O(N^2) worst case -> need to improve this
      my_skus <-  as.character(unique(data[[sku_label]]))
      data[[account_label]] <- as.character(data[[account_label]])
      data[[sku_label]] <- as.character(data[[sku_label]])
      data$base_code <- rep(PLACE_HOLDER_VAL, nrow(data))
      
      for( pg in listofPG){
        for (sku in my_skus){
          for (account in my_accounts){
            curr_subset <- subset(data, data[[account_label]] == account, data[[sku_label]] == sku)
            
            my_weighting <- create_weighting(curr_subset[[unit_sales]])
            my_weighting[is.na(my_weighting)] <- PLACE_HOLDER_VAL
            
            
            # Price and units should be in correct form by now
            
            w_avgprice <- weighted.mean(curr_subset[[avg_price]], my_weighting)
            bool_vec <- curr_subset[[avg_price]] >= w_avgprice - NOISE_CORRECTION_FACTOR
            
            # converting to digital vector
            bool_vec[bool_vec == "TRUE"] <- 1
            bool_vec[bool_vec == "FALSE"] <- 0
            
            data[data$sku_label == sku & data$account_label == account]$base_code <- bool_vec #syntax issue
          }
        }
      }
    }
  }
}

# Returns a vector with the desired promo coding
promo_coding <- function(data, price_label,base_price_label,unit_sales, wm_weight, bp_weight) {
  prices <- price_to_num(data[[price_label]])
  base_prices <- price_to_num(data[[base_price_label]])
  units <- as.numeric(data[[unit_sales]])
  
  # Weighted mean approach
  weights <- create_weighting(units)
  w_price <- weighted.mean(prices, weights, na.rm = TRUE)
  bool_vec <- prices >= w_price - NOISE_CORRECTION_FACTOR
  wm_promo <- convert_bool_to_digital(bool_vec)
  
  # Base price approach
  diff <- base_prices - prices
  bool_vec <- diff < NOISE_CORRECTION_FACTOR
  bp_promo <- convert_bool_to_digital(bool_vec)
  
  # Model avg
  
  avg <- wm_weight* wm_promo + bp_weight * bp_promo
  bool_vec <- avg >= mean(wm_weight + bp_weight)
  avg_promo <- convert_bool_to_digital(bool_vec)
  
  return(avg_promo)
  
}


###################################################################################################################################################################

# Tests

output <- promo_coding(currdata, PRICE_LABEL, BASE_PRICE_LABEL, UNITS_LABEL, 0.2, 0.8)

print(output)

currdata$curr_promo <- output

# Plotting
currdata <- currdata[order(currdata$Time.Period.Continuous.Variable),]
currdata
#ggplot(data = currdata) + 
# geom_line(mapping = aes(x = currdata$Time.Period.Continuous.Variable,y = currdata$Average.Price)) +
# theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#  scale_x_continuous("Time Period", labels = as.character(currdata$Time.Period.Continuous.Variable), breaks = currdata$Time.Period.Continuous.Variable)


plot(currdata$Time.Period.Continuous.Variable,y = price_to_num(currdata$Average.Price), type='l', ylab = "Average Price", xlab = 'Time Period', col="red")
par(new=TRUE)
plot(currdata$Time.Period.Continuous.Variable,y = currdata$curr_promo,ylab = "", xlab = "", col="blue")



