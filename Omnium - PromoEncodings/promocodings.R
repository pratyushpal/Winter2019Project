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

# Returns a vector with the desired promo coding
promo_coding <- function(avg_price,base_price,unit_sales, wm_weight, bp_weight) {
  prices <- price_to_num(avg_price)
  base_prices <- price_to_num(base_price)
  units <- as.numeric(unit_sales)
  
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

om_promo<- function(data,wm_weight, bp_weight, price_label=PRICE_LABEL,
                    units_label=UNITS_LABEL, account_label=ACCOUNT_LABEL,
                    sku_label= SKU_LABEL, base_price_label=BASE_PRICE_LABEL,
                    price_format="$",type="all" ) {
  
  skus <- unique(as.character(data[[sku_label]]))
  accounts <- unique(as.character(data[[account_label]]))
  base_code <- rep(PLACE_HOLDER_VAL, nrow(data))
  
  for(sku in skus){
    for(account in accounts){
      # Getting the SKU account pair indices
      indices <- which(data[[SKU_LABEL]] == sku & data[[ACCOUNT_LABEL]] == account)
      
      # Fetching input for promo-coding algo
      avg_prices <- data[[price_label]][my_data[[account_label]] == account & my_data[[sku_label]] == sku]
      units <- data[[units_label]][my_data[[account_label]] == account & my_data[[sku_label]] == sku]
      base_prices <- data[[base_price_label]][my_data[[account_label]] == account & my_data[[sku_label]] == sku] # this step will change if we implement our own baselines
      
      # Promo coding
      curr_promo <- promo_coding(avg_prices, base_prices,units, wm_weight, bp_weight)
      
      for(index in indices){
        base_code[index] <- curr_promo[index]
      }
    }
  }
  return(base_code)
}

coding <- om_promo(relevant_data, 0.2, 0.8)



create_promo_coding <- function(data, avg_price,unit_sales, account_label,sku_label, pg_label = 'OM.Branded.PG',
                                price_format="$",type="all",listofPG = "null" ) {
  # Fixing any data formatting issues
  om_data_fix(data)
  data[[account_label]] <- as.character(data[[account_label]])
  data[[sku_label]] <- as.character(data[[sku_label]])
  
  
  if(type=="all"){
    my_accounts <- unique(data[[account_label]]) # O(N^2) worst case -> need to improve this
    my_skus <- unique(data[[sku_label]])
    base_code <- rep(PLACE_HOLDER_VAL, nrow(data))
    
    #Iterating for each SKU & Account pair
    for(sku in my_skus){
      for(account in my_accounts){
        
        indices <- which(data[[SKU_LABEL]] == sku & data[[ACCOUNT_LABEL]] == account)
        
        #run promo coding algo
        curr_subset <- subset(data, account_label == account, sku_label == sku)
        output <- promo_coding(curr_subset, PRICE_LABEL, BASE_PRICE_LABEL, UNITS_LABEL, 0.2, 0.8)
        print(output)
        
        for(i in indices){
          base_code[i] <- output[i]
        }
        
      }
    }
    
    mutate(data, auto_promo = base_code)
    
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

###################################################################################################################################################################

# Tests


vec <- create_promo_coding(my_data, PRICE_LABEL, UNITS_LABEL, SKU_LABEL, PG_LABEL,"$", "all","null")
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



