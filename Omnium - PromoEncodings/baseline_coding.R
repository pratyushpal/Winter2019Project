get_baseline <- function(x){
  vec_len <- length(x)
  base_price <- numeric(length(vec_len))
  for (i in 1:vec_len) {
    if (i == 1){
      base_price[i] = x[i]
    }else if (i == 2){
      base_price[i] = max(base_price[i-1], x[i])
    }else if (i==3){
      base_price[i] = max(base_price[i-1], base_price[i-2], x[i])
    }else if (i==4){
      base_price[i] = max(base_price[i-1], base_price[i-2], base_price[i-3], x[i])
    }else{
      base_price[i] = max(base_price[i-1], base_price[i-2], base_price[i-3], base_price[i-4],x[i])
    }
  }
  
  return(base_price)
}

get_baseline_2 <- function(x){
  vec_len <- length(x)
  base_price <- numeric(length(vec_len))
  for (i in 1:vec_len) {
    if (i == 1){
      base_price[i] = x[i]
    }else {
      base_price[i] = max(base_price[i-1], x[i])
    }
  }
  return(base_price)
}

convert_dollar_to_num(currdata, 'Base.Price')
print(currdata$Base.Price)
my_base <- get_baseline(prices)
my_base2 <- get_baseline_2(prices)
given_base <- currdata$Base.Price
diff <- my_base - given_base
diff2 <- my_base2 - given_base
print(diff)
print(diff2)

diff_promo <- given_base - prices

