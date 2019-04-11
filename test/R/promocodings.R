# Promo coding

source("/Users/pratyushpal/Winter2019Project/test/R/omnium-dataprocess.R")
source("/Users/pratyushpal/Winter2019Project/test/R/baselinecoding.R")

#####################################################################################################################################################################

# Constants

PLACE_HOLDER_VAL <-  0
DOLLAR_SUBSTRING_VAL <- 2
NOISE_CORRECTION_FACTOR <- 0.2
ZERO_CORRECTION_VAL <- 100

#####################################################################################################################################################################

# returns modified data frame
acv_coding <- function(data, unit_sales = UNITS_LABEL, promo_base_units = BASE_PROMO_UNITS_LABEL, base_units = BASE_UNITS_LABEL,
                       acv_anymerch = ANY_MERCH_LABEL, type = "base units", promo_base_threshold = 0.8,
                       base_units_threshold = 0.9, acv_anymerch_threshold = 0.35){

  promo_acv <- rep(PLACE_HOLDER_VAL, nrow(data))
  base_acv <- rep(PLACE_HOLDER_VAL, nrow(data))

  if(type == "base units"){

    base_units <- data[[base_units]]
    promo_base_units <- data[[promo_base_units]]
    units <- data[[unit_sales]]
    base_acv <- base_units/units
    promo_base_acv <- promo_base_units/base_units

    # Correcting for division by 0 errors
    # Setting it to an arbitrary low value so it's always smaller than the threshold

    base_acv[is.na(base_acv)] <- -ZERO_CORRECTION_VAL
    base_acv[base_acv == 0] <- -ZERO_CORRECTION_VAL
    base_acv[is.nan(base_acv)] <- -ZERO_CORRECTION_VAL
    promo_base_acv[is.na(promo_acv)] <- -ZERO_CORRECTION_VAL
    promo_base_acv[promo_base_acv == 0] <- -ZERO_CORRECTION_VAL
    promo_base_acv[is.nan(promo_base_acv)] <- -ZERO_CORRECTION_VAL

    print(base_acv)

    # Coding
    base_coding <- base_acv > base_units_threshold
    promo_base_coding <- promo_base_acv > promo_base_threshold

    # Correcting to booleans
    base_coding <- convert_bool_to_digital(base_coding)
    promo_base_coding <- convert_bool_to_digital(promo_base_coding)

    data$base.units.coding <- base_coding
    data$promo.base.units.coding <- promo.base.units.coding

  }else if(type == "acv"){
    promo_acv <- data[[acv_anymerch]]
    # Correcting for division by 0 errors
    # Setting it to an arbitrary high value so it's always bigger than the threshold
    promo_acv[is.na(promo_acv)] <- ZERO_CORRECTION_VAL
    promo_acv[promo_acv == 0] <- ZERO_CORRECTION_VAL
    promo_acv[is.nan(promo_acv)] <- ZERO_CORRECTION_VAL

    # Coding
    coding <- promo_acv > acv_anymerch_threshold
    coding <- convert_bool_to_digital(coding)

    data$acv.coding <- coding
  }


  return(data)
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

  base_prices[base_price == 0] <- ZERO_CORRECTION_VAL # Set the zero base prices to an arbitrary large number
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
  }else if(type == "pg"){
    if(product_groups == "null"){
      print("Please input a vector of required product group labels")
    }else{
      pg_subset <- subset(data, data[[pg_label]] %in% product_groups) # Find a more efficient way to get unique skus and accounts at these PGs
      skus <- unique(as.character(pg_subset[[sku_label]]))
      accounts <- unique(as.character(pg_subset[[account_label]]))
    }
  }

    # Making relevant vectors that will be added to the data with correct dimension

    base_code <- rep(PLACE_HOLDER_VAL, nrow(data))
    base_price <- rep(PLACE_HOLDER_VAL, nrow(data)) # Need to keep this different than the one being later defined
    base_units <- rep(PLACE_HOLDER_VAL, nrow(data))
    base_dollars <- rep(PLACE_HOLDER_VAL, nrow(data))
    promo_code <-  rep(PLACE_HOLDER_VAL, nrow(data))

    for(sku in skus){
      for(account in accounts){
        # Getting the SKU account pair indices
        indices <- which(data[[SKU_LABEL]] == sku & data[[ACCOUNT_LABEL]] == account)
        units <- data[[units_label]][data[[account_label]] == account & data[[sku_label]] == sku]
        dollars <- data[[dollars_label]][data[[account_label]] == account & data[[sku_label]] == sku]
        units <- as.numeric(units)
        dollars <- as.numeric(dollars)
        prices <- dollars/units
        prices[is.na(prices)] <- PLACE_HOLDER_VAL
        prices[is.nan(prices)] <- PLACE_HOLDER_VAL
        prices <- round(prices, 2)

        # Changing type to numeric
        units <- as.numeric(units)
        dollars <- as.numeric(dollars)

        # Fixing NAs
        units[is.na(units)] <- PLACE_HOLDER_VAL
        dollars[is.na(dollars)] <- PLACE_HOLDER_VAL


        # Fetching loop range
        total_weeks <- length(prices)

        # Pick which base prices to select
        if(base_to_use == "self"){
          base_prices <- get_baseline(prices)
        }else if (base_to_use == "data"){
          base_prices <- data[[base_price_label]][data[[account_label]] == account & data[[sku_label]] == sku]
          base_prices <- price_to_num(base_prices)
        }

        # Promo coding
        curr_base <- promo_algo(prices, base_prices)

        # Get base metrics
        curr_base_units <- get_base_metric(curr_base, units)
        curr_base_dollars <- get_base_metric(curr_base, dollars)

        # Debugging output
        cat(sprintf("Account is: %s \n ", account))
        cat(sprintf("SKU is: %s \n", sku))
        if(debugging == "verbose") {
          print("Prices")
          print(prices)
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
          if(prices[i] == 0){
            promo_code[index] <- prices[i]
            base_code[index] <- prices[i]
            base_price[index] <- prices[i]
            base_units[index] <- units[i]
            base_dollars[index] <- dollars[i]
          }else{
            promo_code[index] <- bool_complement(curr_base[i])
            base_code[index] <- curr_base[i]
            base_price[index] <- base_prices[i]
            base_units[index] <- curr_base_units[i]
            base_dollars[index] <- curr_base_dollars[i]
          }
        }
      }
    }

    # Mutating the data

    data$Auto.Promo.Code <- promo_code
    data$Auto.Base.Code <- base_code
    data$Auto.Base.Price <- base_price
    data$Auto.Base.Units <- base_units
    data$Auto.Base.Dollars <- base_dollars

    return(data)

}

###################################################################################################################################################################


