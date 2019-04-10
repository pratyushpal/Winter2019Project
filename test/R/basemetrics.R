# Outputs a vector
# metric can either be a vector with dollar sales or unit sales
get_base_metric <- function(basecoding, metrics, n = 13){
  total_weeks <- length(metrics)
  my_df <- data.frame("base.coding" = basecoding, "metrics" = metrics)
  # Setting all non-base weeks to have 0 units since we will be correcting them
  # This will get rid of NAs as well since they are coded to be a non-base week

  my_df$metrics[my_df$base.coding == 0] <- 0

  base_metrics <- numeric(total_weeks)

  # Compute base units for non-base weeks to be the mean of base week units
  # in a (n/2,n/2) neighbourhood around each non-base week

  radius <- ceiling(n/2) # If n is odd then radius is (n/2 + 1)

  # starting till radius
  for(i in 1:radius){
    if(my_df$base.coding[i] == 1){
      base_metrics[i] <- metrics[i]
    }else{
      neighbourhood <- my_df[1:n,]
      metrics_to_consider <- neighbourhood$metrics[neighbourhood$base.coding == 1]
      base_metrics[i] <- mean(metrics_to_consider)
    }
  }

  # total weeks - (radius - 1) weeks
  for(i in (radius+1):(total_weeks - (radius - 1))){
    if(my_df$base.coding[i] == 1){
      base_metrics[i] <- metrics[i]
    }else{
      neighbourhood <- my_df[(i - radius):(i+(radius-1)),]
      metrics_to_consider <- neighbourhood$metrics[neighbourhood$base.coding == 1]
      base_metrics[i] <- mean(metrics_to_consider)
    }
  }

  # last radius-1 amount of weeks
  for(i in (total_weeks - (radius - 2)):total_weeks){
    if(my_df$base.coding[i] == 1){
      base_metrics[i] <- metrics[i]
    }else{
      neighbourhood <- my_df[(total_weeks - n):total_weeks,]
      metrics_to_consider <- neighbourhood$metrics[neighbourhood$base.coding == 1]
      base_metrics[i] <- mean(metrics_to_consider)
    }
  }

  # Making sure all weeks that have price as 0 have 0 units and that
  # base units are at most units
  for(i in 1:total_weeks){
    base_metrics[i] <- min(base_metrics[i], metrics[i])
  }

  #Debugging output
  print("Debugging")
  cat(sprintf("The given metric values are:\n"))
  print(metrics)
  cat(sprintf("The given base coding is:\n"))
  print(basecoding)
  cat(sprintf("The predicted base metric values are:\n"))
  print(base_metrics)

  return(base_metrics)
}

mypromo <- om_promo(currdata)
units <- currdata$Unit.Sales
dollars <- as.numeric(currdata$X.)
base_units <- get_base_metric(mypromo, units)
base_dollars <- get_base_metric(mypromo, dollars)
