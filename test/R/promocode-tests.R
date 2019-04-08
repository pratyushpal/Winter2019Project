###################################################################################################################################################################

# Tests

new_coding_alg <- wpromo_coding(0.2, 0.8)
coding <- om_promo(my_data, 0.2, 0.8, type="all", promo_algo = new_coding_alg)

my_data$new_promo <- coding

output <- promo_coding(currdata, PRICE_LABEL, BASE_PRICE_LABEL, UNITS_LABEL, 0.2, 0.8)

print(output)

currdata$curr_promo <- output

# Plotting
currdata <- currdata[order(currdata$Time.Period.Continuous.Variable),]
currdata
#ggplot(data = currdata) +
# geom_line(mapping = aes(x = currdata$Time.Period.Continuous.Variable,y = currdata$Average.Price)) +
# theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#  scale_x_continuous("Time Period", labels = as.character(currdata$Time.Period.Continuous.Variable), breaks = currdata$Time.Period.Continuous.Variable)


plot(currdata$Time.Period.Continuous.Variable,y = price_to_num(currdata$Average.Price), type='l', ylab = "Average Price", xlab = 'Time Period', col="red")
par(new=TRUE)
plot(currdata$Time.Period.Continuous.Variable,y = currdata$curr_promo,ylab = "", xlab = "", col="blue")
