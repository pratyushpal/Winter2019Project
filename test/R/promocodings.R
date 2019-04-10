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


# returns a vector
acv_coding <- function(data, unit_sales = UNITS_LABEL, base_units = BASE_UNITS_LABEL, acv_anymerch = ANY_MERCH_LABEL,
                       type = "base units", base_units_threshold = 0.9, acv_anymerch_threshold = 0.35){

  promo_acv <- rep(PLACE_HOLDER_VAL, nrow(data))
  base_acv <- rep(PLACE_HOLDER_VAL, nrow(data))

  if(type == "base units"){

    base_units <- data[[base_units]]
    units <- data[[unit_sales]]
    base_acv <- base_units/units

    # Correcting for division by 0 errors
    # Setting it to an arbitrary low value so it's always smaller than the threshold

    base_acv[is.na(base_acv)] <- -100
    base_acv[base_acv == 0] <- -100
    base_acv[is.nan(base_acv)] <- -100

    print(base_acv)

    # Coding
    coding <- base_acv > base_units_threshold
    coding <- convert_bool_to_digital(coding)

  }else if(type == "acv"){
    promo_acv <- data[[acv_anymerch]]
    # Correcting for division by 0 errors
    # Setting it to an arbitrary high value so it's always bigger than the threshold
    promo_acv[is.na(promo_acv)] <- 100
    promo_acv[promo_acv == 0] <- 100
    promo_acv[is.nan(promo_acv)] <- 100

    # Coding
    coding <- promo_acv > acv_anymerch_threshold
    coding <- convert_bool_to_digital(coding)
  }


  return(coding)
}

# promo_coding: Vec(Num) Vec (Num) Vec(Num) Num Num Num Num -> Vec(Num)
# Returns a vector with the desired promo coding
promo_coding <- function(avg_price,base_price,unit_sales, wm_weight, bp_weight, threshold) {
  #prices <- price_to_num(avg_price)
  #base_prices <- price_to_num(base_price)
  prices <- avg_price
  base_prices <- base_price
  units <- as.numeric(unit_sales)

  # Fixing NAs
  prices[is.na(prices)==TRUE] <- 0
  base_prices[is.na(base_prices)==TRUE] <- 0
  units[is.na(units)==TRUE] <- 0

  # Weighted mean approach

  weights <- create_weighting(units)
  w_price <- weighted.mean(prices, weights, na.rm = TRUE)
  bool_vec <- prices >= w_price - threshold
  wm_promo <- convert_bool_to_digital(bool_vec)

  # Base price approach

  # In this approach we evaluate the difference between avg price and base price and code as base/promo
  # depending on if the difference falls below a set threshold.

  # Since we taking the difference between base and avg price, the zero prices will always differ by
  # lower than our threshold value and create problems when taking percentage difference. So we set
  # the corresponding base prices an arbitrary high value to correct it.

  base_prices[base_price == 0] <- 100 # Set the zero base prices to an arbitrary large number
  diff <- base_prices - prices
  print("here")
  print(diff)
  bool_vec <- (diff < threshold)
  bp_promo <- convert_bool_to_digital(bool_vec)

  # Model avg

  avg <- wm_weight* wm_promo + bp_weight * bp_promo
  bool_vec <- avg >= mean(wm_weight + bp_weight)
  avg_promo <- convert_bool_to_digital(bool_vec)

  return(bp_promo)

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
    print(skus)
    accounts <- unique(as.character(data[[account_label]]))
    base_code <- rep(PLACE_HOLDER_VAL, nrow(data))

    for(sku in skus){
      for(account in accounts){
        # Getting the SKU account pair indices
        indices <- which(data[[SKU_LABEL]] == sku & data[[ACCOUNT_LABEL]] == account)

        # Fetching input for promo-coding algo
        units <- data[[units_label]][data[[account_label]] == account & data[[sku_label]] == sku]
        print(units)
        avg_prices <- data[[price_label]][data[[account_label]] == account & data[[sku_label]] == sku]
        avg_prices <- price_to_num(avg_prices)

        total_weeks <- length(avg_prices)

        # Pick which base prices to select
        if(base_to_use == "self"){
          base_prices <- get_baseline(avg_prices)
        }else if (base_to_use == "data"){
          base_prices <- data[[base_price_label]][data[[account_label]] == account & data[[sku_label]] == sku]
          base_prices <- price_to_num(base_prices)
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

        for(i in 1:total_weeks){
          index <- indices[i]
          base_code[index] <- curr_promo[i]

        }
      }
    }

    # Returning the base coding
    return(base_code)

  } else if(type == "pg"){

  }

}

###################################################################################################################################################################


