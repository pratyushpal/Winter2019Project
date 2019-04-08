source("/Users/pratyushpal/Winter2019Project/test/R/promocodings.R")
# Inspiration: Keegan Morissey
get_baseline <- function(prices, n=13){
  total_weeks <- length(prices)
  base_price <- numeric(total_weeks)

  # First we take a up to n (default 13) week max for each price

  # First n weeks
  for (i in 1:n){
    base_price[i] <- max(prices[1:n])
  }

  # For the remaining weeks
  for(j in (n+1):total_weeks){
    base_price[j] <- max(prices[j-(n-1):j])
  }

  # Flattening the base price and checking for any overestimates
  for(i in total_weeks:(total_weeks-(n-1))) {
    potential_base_price <- max(prices[(total_weeks - (n-1)):total_weeks])
    if(potential_base_price < base_price[i]){
      base_price[i] <- potential_base_price
    }
  }

  for(i in (total_weeks - n) : 1) {
    potential_base_price <- max(prices[i:(i+(n-1))])
    if(potential_base_price < base_price[i]){
      base_price[i] <- potential_base_price
    }
  }

  # Forcing base price to be at least the average price

  for(i in 1:total_weeks){
    base_price[i] <- max(prices[i], base_price[i])
  }

  return(base_price)
}

#####################################################################################################################################################################

# Tests

curr_price <- price_to_num(currdata$Average.Price)
given_base <- price_to_num(currdata$Base.Price)
print(curr_price)
my_base <- get_baseline(curr_price)
print(my_base)
print(given_base)
my_newbase <- get_baseline(curr_price, 8)
print(my_newbase)

promo_newbase <- convert_bool_to_digital(my_newbase - curr_price <= 0.2)
promo_mybase <- convert_bool_to_digital(my_base - curr_price <= 0.2)
promo_given <- convert_bool_to_digital(given_base - curr_price <= 0.2)
promo_newgiven <-  convert_bool_to_digital(given_base - curr_price <= 0.2)
currdata$base_code <- promo_mybase
currdata$new_base <- promo_newbase
currdata$given_base <- promo_given
currdata$newgiven_base <- promo_newgiven

currdata <-currdata[order(currdata$Time.Period.Continuous.Variable),]
time <- currdata$Time.Period.Continuous.Variable
promo_code_testing(currdata$Average.Price, currdata$base_code,currdata$Time.Period.Continuous.Variable)
promo_code_testing(currdata$Average.Price, currdata$new_base,currdata$Time.Period.Continuous.Variable)
promo_code_testing(currdata$Average.Price, currdata$given_base,currdata$Time.Period.Continuous.Variable)
promo_code_testing(currdata$Average.Price, currdata$newgiven_base,currdata$Time.Period.Continuous.Variable)



