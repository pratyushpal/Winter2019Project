# Promo coding

#####################################################################################################################################################################

# Constants

PLACE_HOLDER_VAL <-  0
DOLLAR_SUBSTRING_VAL <- 2
NOISE_CORRECTION_FACTOR <- 0.2

#####################################################################################################################################################################

invert <- function(x){
  len <- length(x)
  for (i in range(1:len)){
    if(x[i] != 0){
      x[i] <- 1/x[i]
    }
  }

  return(x)
}

create_weighting <- function(x) {
  myweight <- x / sum(x^2, na.rm = TRUE)
  # Want to give higher weight to prices wth lower unit sales since
  # our intuition says that promo prices have higher unit sales
  myweight <- invert(x)
  myweight <- myweight/(sum(myweight^2, na.rm=TRUE))

  return(myweight)
}

# Returns a vector with the desired promo coding
promo_coding <- function(avg_price,base_price,unit_sales, wm_weight, bp_weight, threshold) {
  prices <- price_to_num(avg_price)
  base_prices <- price_to_num(base_price)
  units <- as.numeric(unit_sales)

  # Fixing NAs
  prices[is.na(prices)==TRUE] <- 0
  base_prices[is.na(base_prices)==TRUE] <- 0
  units[is.na(units)==TRUE] <- 0

  # Weighted mean approach
  weights <- create_weighting(units)
  w_price <- weighted.mean(prices, weights, na.rm = TRUE)
  bool_vec <- prices >= w_price - NOISE_CORRECTION_FACTOR
  wm_promo <- convert_bool_to_digital(bool_vec)

  # Fixing NAs
  prices[is.na(prices)==TRUE] <- 0
  base_prices[is.na(prices)==TRUE] <- 0

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

wpromo_coding <- function(wm_weight, bp_weight, threshold){
  myfunc <- function(avg_price,base_price,unit_sales){
    promo_coding(avg_price,base_price,unit_sales, wm_weight, bp_weight, threshold)
  }

  return(myfunc)
}


om_promo<- function(data, price_label=PRICE_LABEL,units_label=UNITS_LABEL,
                    account_label=ACCOUNT_LABEL,sku_label= SKU_LABEL,
                    base_price_label=BASE_PRICE_LABEL,pg_label = PG_LABEL,
                    base_to_use = "self",type="all", product_groups = "null",
                    promo_algo = wpromo_coding(0.2,0.8, 0.2)) {

  if(type == "all") {
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

        # Pick which base prices to select
        if(base_to_use == "self"){
          base_prices <- get_baseline(avg_prices)
        }else if (base_to_use == "data"){
          base_prices <- data[[base_price_label]][my_data[[account_label]] == account & my_data[[sku_label]] == sku]
        }

        # Promo coding
        curr_promo <- promo_algo(avg_prices, base_prices,units)
        # Debugging output
        cat(sprintf("Account is: %s \n ", account))
        cat(sprintf("SKU is: %s \n", sku))
        print("Prices")
        print(avg_prices)
        print("Units")
        print(units)
        print("Coding")
        print(curr_promo)

        for(index in indices){
          base_code[index] <- curr_promo[index]
        }
      }
    }

    # Returning the base coding
    return(base_code)

  } else if(type == "pg"){

  }

}

###################################################################################################################################################################


