# Promo coding

source("/Users/pratyushpal/Winter2019Project/test/R/omnium-dataprocess.R")
source("/Users/pratyushpal/Winter2019Project/test/R/baselinecoding.R")

#####################################################################################################################################################################

# Constants

PLACE_HOLDER_VAL <-  0
DOLLAR_SUBSTRING_VAL <- 2
NOISE_CORRECTION_FACTOR <- 0.2

#####################################################################################################################################################################

# returns modified data frame
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
promo_coding <- function(avg_price,base_price, type = "abs",abs_threshold = 0.2, percent_threshold = 0.1) {
  # to avoid unused parameter errors
  prices <- avg_price
  base_prices <- base_price
  abs <- abs_threshold
  percent <- percent_threshold

  # Fixing NAs
  prices[is.na(prices)==TRUE] <- 0
  base_prices[is.na(base_prices)==TRUE] <- 0

  # Base price approach

  # In this approach we evaluate the difference between avg price and base price and code as base/promo
  # depending on if the difference falls below a set threshold.

  # Since we taking the difference between base and avg price, the zero prices will always differ by
  # lower than our threshold value and create problems when taking percentage difference. So we set
  # the corresponding base prices an arbitrary high value to correct it.

  base_prices[base_price == 0] <- 100 # Set the zero base prices to an arbitrary large number
  diff <- base_prices - prices
  if(type == "abs"){
    bool_vec <- (diff < abs)
  }else if (type == "percent") {
    threshold <- percent * prices
    bool_vec <- (diff < threshold)
  }
  bp_promo <- convert_bool_to_digital(bool_vec)

  return(bp_promo)

}

 wpromo_coding <- function(type, abs_threshold, percent_threshold) {

   myfunc <- function(avg_price,base_price) {
    promo_coding(avg_price,base_price,type, abs_threshold, percent_threshold)
  }

  return(myfunc)
}


om_promo<- function(data, price_label=PRICE_LABEL,units_label=UNITS_LABEL,
                    dollars_label= DOLLARS_LABEL,account_label=ACCOUNT_LABEL,
                    sku_label= SKU_LABEL,base_price_label=BASE_PRICE_LABEL,
                    pg_label = PG_LABEL,base_to_use = "self",type="all",
                    product_groups = "null",promo_algo = wpromo_coding("abs",0.2,0.1),
                    debugging="not verbose") {

  if(type == "all") {
    skus <- unique(as.character(data[[sku_label]]))
    accounts <- unique(as.character(data[[account_label]]))
    base_code <- rep(PLACE_HOLDER_VAL, nrow(data))
    base_units <- rep(PLACE_HOLDER_VAL, nrow(data))
    base_dollars <- rep(PLACE_HOLDER_VAL, nrow(data))

    for(sku in skus){
      for(account in accounts){
        # Getting the SKU account pair indices
        indices <- which(data[[SKU_LABEL]] == sku & data[[ACCOUNT_LABEL]] == account)
        units <- data[[units_label]][data[[account_label]] == account & data[[sku_label]] == sku]
        dollars <- data[[dollars_label]][data[[account_label]] == account & data[[sku_label]] == sku]

        # Changing type to numeric
        units <- as.numeric(units)
        dollars <- as.numeric(dollars)

        # Fixing NAs
        units[is.na(units)] <- PLACE_HOLDER_VAL
        dollars[is.na(dollars)] <- PLACE_HOLDER_VAL


        # Fetching input for promo-coding algo
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
        curr_base <- promo_algo(avg_prices, base_prices)

        # Get base metrics
        curr_base_units <- get_base_metric(curr_base, units)
        curr_base_dollars <- get_base_metric(curr_base, dollars)

        # Debugging output
        cat(sprintf("Account is: %s \n ", account))
        cat(sprintf("SKU is: %s \n", sku))
        if(debugging == "verbose") {
          print("Prices")
          print(avg_prices)
          print("Coding")
          print(curr_base)
          print("Units")
          print(units)
          print("Base Units")
          print(curr_base_units)
          print("Dollars")
          print(dollars)
          print("Base Dollars")
          print(curr_base_dollars)
        }

        for(i in 1:total_weeks){
          index <- indices[i]
          if(avg_prices[i] == 0){
            base_code[index] <- avg_prices[i]
            base_units[index] <- units[i]
            base_dollars[index] <- dollars[i]
          }else{
            base_code[index] <- curr_promo[i]
            base_units[index] <- curr_base_units[i]
            base_dollars[index] <- curr_base_dollars[i]
          }
        }
      }
    }

    # Mutating the data

    data$Auto.Base.Code <- base_code
    data$Auto.Base.Units <- base_units
    data$Auto.Base.Dollars <- base_dollars

    return(data)

  } else if(type == "pg"){

  }

}

###################################################################################################################################################################


