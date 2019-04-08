# Make sure to test this, not so sure that it works right.
#  Should be approx right, but maybe off by a few days.
get_date <- function(num) {
  offset <- 25567
  retval <- as.Date(num - offset - 2)
  return(retval)
}

get_month <- function(date) {
  month <- format(date, "%m")
  if (month == "01") {
    retval <- "January"
  } else if (month == "02") {
    retval <- "February"
  } else if (month == "03") {
    retval <- "March"
  } else if (month == "04") {
    retval <- "April"
  } else if (month == "05") {
    retval <- "May"
  } else if (month == "06") {
    retval <- "June"
  } else if (month == "07") {
    retval <- "July"
  } else if (month == "08") {
    retval <- "August"
  } else if (month == "09") {
    retval <- "September"
  } else if (month == "10") {
    retval <- "October"
  } else if (month == "11") {
    retval <- "November"
  } else if (month == "12") {
    retval <- "December"
  }
  return(retval)
}

get_month_num <- function(date) {
  month <- format(date, "%m")
  retval <- as.numeric(month)
  return(retval)
}
