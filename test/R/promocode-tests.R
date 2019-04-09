###################################################################################################################################################################

# Tests

new_coding_alg <- wpromo_coding(0.2, 0.8, 0.2)
cabo_coding <- cabo_fresh_auth$Base.code

my_cabo_coding <- om_promo(cabo_fresh_auth,wpromo_coding)


my_data$new_promo <- coding

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

########################################## Doing promo code function by hand #####################################################################################
data <- cabo_fresh_auth
corr_code <- data$Base.code
skus <- unique(as.character(data[[SKU_LABEL]]))
accounts <- unique(as.character(data[[ACCOUNT_LABEL]]))
base_code <- rep(PLACE_HOLDER_VAL, nrow(data))
curr_alg <- wpromo_coding(0,1,0.2)
for(sku in skus){
 for(account in accounts){
   indices <- which(data[[SKU_LABEL]] == sku & data[[ACCOUNT_LABEL]] == account)
   units <- data[[UNITS_LABEL]][data[[ACCOUNT_LABEL]] == account & data[[SKU_LABEL]] == sku]
   avg_price <- data[[PRICE_LABEL]][data[[ACCOUNT_LABEL]] == account & data[[SKU_LABEL]] == sku]
   avg_price <- price_to_num(avg_price)
   num_weeks <- length(units)
   print("avg price")
   print(avg_price)
   print("base price")
   base_prices <- get_baseline(avg_price)
   curr_promo <- new_coding_alg(avg_price, base_prices,units)
   correct_code <- data$Base.code[data[[ACCOUNT_LABEL]] == account & data[[SKU_LABEL]] == sku]
   print(base_prices)
   print("base code")
   print(curr_promo)
   print("curr code")
   print(correct_code)
   for(index in indices){
    base_code[index] <- index
   }
 }
}

print(base_code)
print(corr_code)
