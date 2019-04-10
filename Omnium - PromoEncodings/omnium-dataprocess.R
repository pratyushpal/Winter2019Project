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
DATE_LABEL <- "OM.DATE" # Assumption based on what I named it
PG_LABEL <- "OM.Branded.PG" # Assuming mapping is applied
DOLLARS_LABEL <- "X." # R will default any $ to X

DATA_NAME = 'Yucatan-Trended.csv'
SKU_NAME = 'CABO FRESH_Authentic Guacamole 12 oz'
ACCOUNT_NAME = 'Giant Landover_Ahold'
PG_NAME <- "CABO FRESH_12oz Tub"

PLACE_HOLDER_VAL <-  0
DOLLAR_SUBSTRING_VAL <- 2
NOISE_CORRECTION_FACTOR <- 0.2

#####################################################################################################################################################################

# Testing repo

# reading data

my_data <- read.csv('/Users/pratyushpal/Downloads/promotest.csv')
test_data <- subset(my_data, my_data$Lower.Level==1 & my_data$Req.TDP == 1)
cabo_fresh <- subset(my_data, my_data$Lower.Level == 1 & my_data$OM.Branded.PG == "CABO FRESH_12oz Tub")
cabo_fresh_auth <- subset(my_data, my_data$Lower.Level == 1 & my_data$OM.SKU.Name == SKU_NAME)
yucatan <- subset(my_data, my_data$Lower.Level == 1 & my_data$OM.Branded.PG == "YUCATAN_8oz Tub")


# Helper functions

output_csv <- function(output_directory,data, file_name){
  # Store the current directory and switch to it once output is done
  curr_dir <- getwd()

  #Adding csv to the provided file name
  output_file_name <- paste(file_name,".csv", sep="")

  # Outputting the file with appropriate name at the given directory
  setwd(output_directory)
  write.csv(data, file = output_file_name)

  # Setting directory back to original
  setwd(curr_dir)
}

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

convert_data <- function(data=my_data, col_name, fun){
  #converts all factor level vars to character
  data[[col_name]] <- fun(data[[col_name]])
}

convert_to_char <-  function(data=my_data, col_name){
  #converts all factor level vars to character
  data[[col_name]] <- as.character(data[[col_name]])
}

convert_to_num <-  function(data=my_data, col_name){
  #converts all factor level vars to character
  return(as.numeric(data[[col_name]]))
}


convert_dollar_to_num <- function(data=my_data, col_name){
  # fixing digits and converting to nunber
  char_vec <- as.character(data[[col_name]])
  char_vec <- substring(char_vec,DOLLAR_SUBSTRING_VAL)
  data[[col_name]] <- as.numeric(char_vec)
}

price_to_num <- function(vec){
  char_vec <- as.character(vec)
  char_vec <- substring(char_vec,DOLLAR_SUBSTRING_VAL)
  num_vec <- suppressWarnings(as.numeric(char_vec))
  num_vec[is.na(num_vec)] <- 0
  return(num_vec)
}

convert_bool_to_digital<- function(vec){
  vec[vec == TRUE] <- 1
  vec[vec == FALSE] <- 0
  return(vec)
}

om_data_fix <- function(data, price_cols=c(BASE_PRICE_LABEL, PRICE_LABEL, QUARTER_PRICE_LABEL),
                        char_cols= c(ACCOUNT_LABEL, SKU_LABEL, PG_LABEL, DATE_LABEL),
                        num_cols= c(UNITS_LABEL, DOLLARS_LABEL, ACV_LABEL)) {

  # Fixing prices
  for(price in price_cols){
    char_vec <- as.character(data[[price]])
    char_vec <- substring(char_vec,DOLLAR_SUBSTRING_VAL)
    mutate(data, price = as.numeric(char_vec))
  }

  # Fixing Text columns
  for(label in char_cols){
    data[[label]] <- as.character(data[[label]])
  }

  # Fixing Numeric columns
  for(label in num_cols){
    data[[label]] <- as.numeric(data[[label]])
  }
}

test_data_fix <- function(data, price_cols=c(BASE_PRICE_LABEL, PRICE_LABEL, QUARTER_PRICE_LABEL),
                          char_cols= c(ACCOUNT_LABEL, SKU_LABEL, PG_LABEL, DATE_LABEL),
                          num_cols= c(UNITS_LABEL, DOLLARS_LABEL, ACV_LABEL)){
  # Applying the function
  om_data_fix(data)

  # Testing
  print("Checking prices")
  for(price in price_cols){
    print(head(data[[price]]))
  }
  print("Price checks done")

  print("Checking text labels")
  for(label in char_cols){
    print(head(data[[label]]))
  }
  print("Text label checks done ")

  print("Checking numeric labels")
  for(label in num_cols){
    print(head(data[[label]]))
  }
  print("Numeric label checks done")

}

currdata <- subset(my_data, OM.SKU.Name== SKU_NAME & OM.Account== ACCOUNT_NAME)
relevant_data <- subset(my_data, Lower.Level == 1)
test_data_fix(my_data)
test_data_fix(currdata)
