source("summarize_line/summarize_line.R", chdir=T)

# Process a weekly account data file to add process metrics.
# Specifically, this will clean base units and give a clean base week
#  coding, which can be used to build sales rate tables.
# Will output the new file: "new_processed_data.csv" into the "data" folder.
process_data <- function(filename) {
    
    # Set the file name, and read in the file.
    path_prefix <- "data/"
    filename_path <- paste(path_prefix, filename, sep="")
    data <- read.csv(filename_path)

    # Check that headers are correct
    necessary_headers <- c(
        "Market", "Product", "Week", "Units", "Dollars", "ACV", "TDP", "Channel", "acv_mm", "account_use", "L52"
    )
    current_headers <- colnames(data)
    missing_headers <- setdiff(necessary_headers, current_headers)
    if (length(missing_headers) > 0) {
        # Stop the function and print error
        error_message <- "The following headers are either missing or mis-named: "
        print(error_message)
        print(missing_headers)
        stop("Insufficient columns.")
    }

    # Verify that the required number or integer columns are numeric
    non_numeric_cols_that_should_be_numeric <- c()
    if (!is.numeric(data$Week)) {
        non_numeric_cols_that_should_be_numeric <- append(non_numeric_cols_that_should_be_numeric, "Week")
    }
    if (!is.numeric(data$Units)) {
        non_numeric_cols_that_should_be_numeric <- append(non_numeric_cols_that_should_be_numeric, "Units")
    }
    if (!is.numeric(data$Dollars)) {
        non_numeric_cols_that_should_be_numeric <- append(non_numeric_cols_that_should_be_numeric, "Dollars")
    }
    if (!is.numeric(data$ACV)) {
        non_numeric_cols_that_should_be_numeric <- append(non_numeric_cols_that_should_be_numeric, "ACV")
    }
    if (!is.numeric(data$TDP)) {
        non_numeric_cols_that_should_be_numeric <- append(non_numeric_cols_that_should_be_numeric, "TDP")
    }
    if (!is.numeric(data$acv_mm)) {
        non_numeric_cols_that_should_be_numeric <- append(non_numeric_cols_that_should_be_numeric, "acv_mm")
    }
    if (!is.numeric(data$account_use)) {
        non_numeric_cols_that_should_be_numeric <- append(non_numeric_cols_that_should_be_numeric, "account_use")
    }
    if (!is.numeric(data$L52)) {
        non_numeric_cols_that_should_be_numeric <- append(non_numeric_cols_that_should_be_numeric, "L52")
    }

    # Give error message if there are any non numeric columns that should be numeric
    if (length(non_numeric_cols_that_should_be_numeric) > 0) {
        print("The following headers are non-numeric, but should be numeric: ")
        print(non_numeric_cols_that_should_be_numeric)
        stop("Error in column type.")
    }

    # Calculate average price, and catch errors.
    data$AvgPrice <- data$Dollars / data$Units
    data$AvgPrice[is.na(data$AvgPrice) == 1] <- 0

    # All markets and all products
    all_markets <- unique(data$Market)
    all_products <- unique(data$Product)

    # Initialize new_data
    new_data <- -1

    # Loop through all Market/Product combinations, and add process metrics.
    for (market in all_markets) {
        print(market)
        for (product in all_products) {
            print(product)
            cur_line_chart <- subset(data, Market == market & Product == product)
            cur_line_chart[is.na(cur_line_chart)] <- 0
            processed_line_chart <- process_line_chart(cur_line_chart)$line_chart

            # Trying this for now...
            # processed_line_chart$usp_acv_tdp <- processed_line_chart$Units / (processed_line_chart$TDP)
            # processed_line_chart$usp_acv_tdp[is.na(processed_line_chart$usp_acv_tdp) == 1] <- 0
            # mean_base_week_usp_acv_tdp <- mean(processed_line_chart$usp_acv_tdp[processed_line_chart$clean_base_week == 1 & processed_line_chart$ACV > 10])
            # sd_base_week_usp_acv_tdp <- sd(processed_line_chart$usp_acv_tdp[processed_line_chart$clean_base_week == 1 & processed_line_chart$ACV > 10])
            # processed_line_chart$usp_acv_tdp_scaled <- (processed_line_chart$usp_acv_tdp - mean_base_week_usp_acv_tdp) / sd_base_week_usp_acv_tdp

            if (new_data == -1) {
                new_data <- processed_line_chart
            } else {
                new_data <- rbind(new_data, processed_line_chart)
            }
        }
    }

    # Write output file
    outfile_name <- paste("data/new_processed_data", filename, sep="")
    write.csv(new_data, outfile_name)

}
