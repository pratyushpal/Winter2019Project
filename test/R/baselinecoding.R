source("/Users/pratyushpal/Winter2019Project/test/R/promocodings.R")

# Inspiration: Keegan Morissey
get_baseline <- function(prices, n=13){
  prices <- prices
  total_weeks <- length(prices)
  base_price <- numeric(total_weeks)

  if (total_weeks <= n){
    for(i in 1:total_weeks){
      base_price[i] <- max(prices[1:total_weeks])
    }

  }else{
    # First we take a up to n (default 13) week max for each price
    # First n weeks
    for (i in 1:n){
      base_price[i] <- max(prices[1:n])
    }

    # For the remaining weeks
    for(j in (n+1):total_weeks){
      base_price[j] <- max(prices[(j-(n-1)):j])
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
  }

    return(base_price)
}
#####################################################################################################################################################################
