# Clearing previous memory
rm(list=ls())

# Setting directory
setwd('/Users/pratyushpal/Downloads')
getwd()

#library
library(tidyverse)

#####################################################################################################################################################################

# Constants 

SKU_LABEL <-"OM.SKU.Name"
UNITS_LABEL <- "Unit.Sales"
ACV_LABEL <- "ACV.Weighted.Distribution"
PRICE_LABEL <- "Average.Price"
BASE_PRICE_LABEL <- "Base.Price"
QUARTER_PRICE_LABEL <- "Quarter.Price" # Assuming mapping is applied
ACCOUNT_LABEL <- "OM.Account" # Assuming mapping is applied if not, put geography
DATE_LABEL <- "OM.Date" # Assuming mapping is applied
PG_LABEL <- "OM.Branded.PG" # Assuming mapping is applied
DOLLARS_LABEL <- "X." # R will default any $ to X

DATA_NAME = 'Yucatan-Trended.csv'
SKU_NAME = 'CABO FRESH_Authentic Guacamole 12 oz' 
ACCOUNT_NAME = 'Giant Landover_Ahold'

PLACE_HOLDER_VAL <-  0
DOLLAR_SUBSTRING_VAL <- 2
NOISE_CORRECTION_FACTOR <- 0.2

#####################################################################################################################################################################


# reading data
  
my_data <- read.csv(DATA_NAME)

convert_data <- function(data= my_data, col_name, fun){
  #converts all factor level vars to character
  data[[col_name]] <- fun(data[[col_name]])
}

convert_to_char <-  function(data= my_data, col_name){
  #converts all factor level vars to character
  data[[col_name]] <- as.character(data[[col_name]])
}

convert_to_num <-  function(data= my_data, col_name){
  #converts all factor level vars to character
  print("here")
  data[[col_name]] <- as.numeric(data[[col_name]])
  print("works")
}

  
convert_dollar_to_num <- function(data = my_data, col_name){
  #converting to character
  convert_to_char(data, col_name)
  # fixing digits and converting to nunber
  data[[col_name]] <- substring(data[[col_name]],DOLLAR_SUBSTRING_VAL)
  
  convert_to_num(data, col_name)
}

om_data_fix <- function(data, price_cols=c(BASE_PRICE_LABEL, PRICE_LABEL, QUARTER_PRICE_LABEL),
                        char_cols= c(ACCOUNT_LABEL, SKU_LABEL, PG_LABEL, DATE_LABEL),
                        num_cols= c(UNITS_LABEL, DOLLARS_LABEL, ACV_LABEL)) {
  
  for(price in price_cols){
    convert_dollar_to_num(data, price)
    print("price done")
  }
  
  for(char_col in char_cols){
    convert_to_char(data, char_col)
    print("char done")
  }
  
  for(num_col in num_cols){
    convert_to_num(data, num_col)
    print("num done")
  }
 
}

om_data_fix(my_data)
currdata <- subset(data, OM.SKU.Name== SKU_NAME & OM.Account== ACCOUNT_NAME)
