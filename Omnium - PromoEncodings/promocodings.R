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

convert_dollar_to_num <- function(data, col_name){
  #converting average price to a number
  data[[col_name]] <- as.character(data[[col_name]])
  data[[col_name]] <- substring(data[[col_name]],DOLLAR_SUBSTRING_VAL)
  data[[col_name]] <- as.numeric(data[[col_name]])
  print(data)
}

create_promo_coding <- function(data, avg_price,unit_sales, account_label,sku_label, product_group, price_format="$",type="all", pg_label = 'OM.Branded.PG',listofPG = "null" ) {
  # Correcting price formatting
  if(price_format == "$") {
    
    convert_dollar_to_num(data, avg_price)
    
  }else if(price_format == "num"){
    print("Good price format")
  }else{
    print("Bad formatting used")
  }
  
  if(type=="all"){
    my_accounts <- as.character(unique(data[[account_label]])) # O(N^2) worst case -> need to improve this
    my_skus <-  as.character(unique(data[[sku_label]]))
    data[[account_label]] <- as.character(data[[account_label]])
    data[[sku_label]] <- as.character(data[[sku_label]])
    data$base_code <- rep(PLACE_HOLDER_VAL, nrow(data))
    
    #Iterating for each SKU & Account pair
    for(sku in my_skus){
      for(account in my_accounts){
        
        #run promo coding algo
        
        curr_subset <- subset(data, account_label = account, sku_label = sku)
        
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
            curr_subset <- subset(data, data[[account_label]] = account, data[[sku_label]] = sku)
            
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
  


avg_price <- 'Average.Price'
unit_sales <- 'Unit.Sales'
sku_label <- 'OM.SKU.Name'
account_label <- 'OM.Account'

currdata[[avg_price]] <- as.character(currdata[[avg_price]])
currdata[[avg_price]] <- substring(currdata[[avg_price]],DOLLAR_SUBSTRING_VAL)
currdata[[avg_price]] <- as.numeric(currdata[[avg_price]]) 


myweighting <- create_weighting(currdata[[unit_sales]])

# Setting some starting point for the price based on a weighting by unit sales made on a price
weighted_avgprice <- weighted.mean(currdata[[avg_price]], myweighting)
# Adding promocoding to the the data
currdata <- mutate(currdata, salesweight = myweighting)
currdata$salesweight[is.na(currdata$salesweight)] <- 0

#converting time period to a date variable

currdata$Time.Period.End.Date <- as.Date(currdata$Time.Period.End.Date, "%m/%d/%Y")


# naive filter *should* solve _most_ encoding cases
bool_vec <- currdata[[avg_price]] >= (weighted_avgprice - NOISE_CORRECTION_FACTOR)

# converting to digital vector
bool_vec[bool_vec == "TRUE"] <- 1
bool_vec[bool_vec == "FALSE"] <- 0

currdata <- mutate(currdata, newpromo = bool_vec)
# comparing with actual encoding
new_bool <- bool_vec == currdata$X16oz..Promo.Coding
print(new_bool)



# Plotting

#ggplot(data = currdata) + 
#  geom_line(mapping = aes(x = currdata$Time.Period.Continuous.Variable,y = currdata$Average.Price)) +
#  geom_point( mapping = aes(x = currdata$Time.Period.Continuous.Variable,y = currdata$newpromo)) +
# theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#  scale_x_continuous("Time Period", labels = as.character(currdata$Time.Period.Continuous.Variable), breaks = currdata$Time.Period.Continuous.Variable)


#plot(currdata$Time.Period.Continuous.Variable,y = currdata$Average.Price, type='l', ylab = "Average Price", xlab = 'Time Period')
