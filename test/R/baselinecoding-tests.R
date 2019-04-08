#####################################################################################################################################################################

# Tests for Baseline coding

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
