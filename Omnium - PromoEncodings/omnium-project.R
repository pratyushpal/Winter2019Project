# Clearing previous memory
rm(list=ls())

# Setting directory
setwd('/Users/pratyushpal/Downloads')
getwd()

#library
library(dplyr)

# reading data

data <- read.csv('Yucatan-Trended.csv')

currdata <- subset(data, OM.SKU.Name=='CABO FRESH_Authentic Guacamole 12 oz' 
                   & OM.Account=='Publix_CORP')


create_weighting <- function(x) {
  myweight <- x / sum(x^2, na.rm = TRUE)
  # Want to give higher weight to prices wth lower unit sales since 
  # our intuition says that promo prices have higher unit sales
  myweight <- 1/myweight
  myweight <- myweight/(sum(myweight^2, na.rm=TRUE))
  return(myweight)
}

currdata <- mutate(currdata, salesweight = create_weighting(currdata$Unit.Sales))
currdata$salesweight[is.na(currdata$salesweight)] <- 0

#converting average price to a number
currdata$Average.Price <- as.character(currdata$Average.Price)
currdata$Average.Price <- substring(currdata$Average.Price,2)
currdata$Average.Price <- as.numeric(currdata$Average.Price)

# Setting some starting point for the price based on a weighting by unit sales made on a price
weighted_avgprice <- weighted.mean(currdata$Average.Price, currdata$salesweight)

# naive filter *should* solve _most_ encoding cases
bool_vec <- currdata$Average.Price >= (weighted_avgprice - 0.2)

# converting to digital vector
bool_vec[bool_vec == "TRUE"] <- 1
bool_vec[bool_vec == "FALSE"] <- 0

# comparing with actual encoding
new_bool <- bool_vec == currdata$X16oz..Promo.Coding
print(new_bool)

