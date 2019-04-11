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
data <- yucatan
corr_code <- data$Base.code
skus <- unique(as.character(data[[SKU_LABEL]]))
accounts <- unique(as.character(data[[ACCOUNT_LABEL]]))
base_code <- rep(PLACE_HOLDER_VAL, nrow(data))
curr_alg <- wpromo_coding("abs",0.2,0.1)
for(sku in skus){
 for(account in accounts){
   indices <- which(data[[SKU_LABEL]] == sku & data[[ACCOUNT_LABEL]] == account)
   units <- data[[UNITS_LABEL]][data[[ACCOUNT_LABEL]] == account & data[[SKU_LABEL]] == sku]
   avg_price <- data[[PRICE_LABEL]][data[[ACCOUNT_LABEL]] == account & data[[SKU_LABEL]] == sku]
   avg_price <- price_to_num(avg_price)
   num_weeks <- length(units)
   cat(sprintf("Account is %s\n", account))
   cat(sprintf("SKU is %s\n", sku))
   print("avg price")
   print(avg_price)
   print("base price")
   base_prices <- get_baseline(avg_price)
   base_prices[base_prices == 0] <- 100
   print(base_prices)
   curr_promo <- curr_alg(avg_price, base_prices)
   correct_code <- data$Base.code[data[[ACCOUNT_LABEL]] == account & data[[SKU_LABEL]] == sku]
   print("difference")
   print(curr_promo)
   print("curr code")
   print(correct_code)
   print("Indices")
   print(indices)
   print(length(indices) == length(curr_promo))
   ind_len <- length(indices)
   for(i in 1:ind_len) {
     index <- indices[i]
     base_code[index] <- curr_promo[i]
   }
 }
}

print(base_code)
print(corr_code)
data$auto_code <- base_code

req_autocode <- data$auto_code[data$Req.TDP == 1]
req_correct <- data$Base.code[data$Req.TDP == 1]

output <- (req_autocode == req_correct)
print(output)
output <- convert_bool_to_digital(output)
print(output)
num_corr <- sum(output)
total_len <- length(req_correct)
percent_corr <- (num_corr/total_len) * 100

acv_35 <- acv_coding(data, base_units_threshold = 0.9)
data$acv_35 <- acv_35
req_35 <- data$acv_35[data$Req.TDP == 1]

output_35 <- (req_35 == req_correct)
print(output_35)
output_35 <- convert_bool_to_digital(output_35)
print(output_35)
num_corr_35 <- sum(output_35)
total_len <- length(req_correct)
percent_corr_35 <- (num_corr_35/total_len) * 100

