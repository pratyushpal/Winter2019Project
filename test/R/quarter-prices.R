
# function to round the closest number
round_to_closer <- function(num, upper, lower){
  upper_diff <- abs(upper - num)
  lower_diff <- abs(num - lower)
  
  if(upper_diff <= lower_diff){
    return(upper)
  }else{
    return(lower)
  }
}

round_to_closer(4.89, 4.99, 4.79)

# rounds to the nearest quarter price 
round_to_quarter <- function(price){
    floor <- floor(price)
    prev_int <- floor - 1
    frac <- price - floor
    if(frac < 0.29){
      if(floor == 0){
        upper <- floor + 0.29
        lower <- floor
        round_to_closer(price, upper, lower)
      }else{
        upper <- floor + 0.29
        lower <- prev_int + 0.99
        round_to_closer(price, upper, lower)
      }
    }else if(frac < 0.49){
      upper <- floor + 0.49
      lower <- floor + 0.29
      round_to_closer(price, upper, lower)
    }else if(frac < 0.79){
      upper <- floor + 0.79
      lower <- floor + 0.49
      round_to_closer(price, upper, lower)
    }else{
      upper <- floor + 0.99
      lower <- floor + 0.79
      round_to_closer(price, upper, lower)
    }
}

# Outputs a vector of quarter prices
quarter_prices <- function(prices){
  output <- lapply(prices, round_to_quarter)
  # Converting to vector
  output<-unlist(output, use.names=FALSE)
  return(output)
}

# Tests
round_to_quarter(3.19)
prices <- c(0.1, 2.19, 16.57, 4.89, 5.67)
qtr_prices <- quarter_prices(prices)
print(qtr_prices)
