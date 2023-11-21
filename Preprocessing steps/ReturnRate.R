# Load the necessary library
install.packages("dplyr")
install.packages("dummy")
library(dplyr)
library(dummy)

#Defining some functions 
impute <- function(x, method = mean, val = NULL) {
  if (is.null(val)) {
    val <- method(x, na.rm = TRUE)
  }
  x[is.na(x)] <- val
  return(x)
}
modus <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Read the CSV file into an R data frame
sales_train <- read.csv("Original Data/sales_yr1.csv")
sales_train$date_id <- as.Date(as.character(sales_train$date_id), format = "%Y%m%d")
sales_train$date_id <- impute(sales_train$date_id, method = mean) 
sales_train$net_sales_amount <- impute(sales_train$net_sales_amount, method = mean) 

#group the channels based on online vs offline purchases
online_purchases_index <- which(sales_train$channel == "CH//Online")
sales_train[-c(online_purchases_index), 'channel'] <- 0
sales_train[c(online_purchases_index), 'channel'] <- 1
sales_train$channel <- as.numeric(sales_train$channel)
table(sales_train$channel)

sales_train_grouped <- sales_train %>%
  group_by(pid) %>%
  summarize(
    channel = modus(channel), 
    date_id = modus(date_id), 
    net_sales_amount = sum(net_sales_amount)
  )

# Perform the calculations
return_rate_yr1 <- sales_train %>%
  mutate(net_sales_amount = as.numeric(net_sales_amount)) %>%  
  group_by(pid) %>%  # Group data by product ID ('pid')
  summarize(
    total_returned = sum(net_sales_amount[net_sales_amount < 0], na.rm = TRUE),  # Sum negative amounts
    total_sold = sum(net_sales_amount[net_sales_amount > 0], na.rm = TRUE),  # Sum positive amounts
    return_rate = -total_returned / total_sold  # Calculate return rate
  ) %>%
  mutate(return_rate = ifelse(is.nan(return_rate) | is.infinite(return_rate), 0, return_rate)) %>%  
  ungroup() 

sales_train_grouped[c('year', 'month', 'day')] <- str_split_fixed(sales_train_grouped$date_id, '-', 3)
sales_train_grouped[c('day_of_the_week')] <- weekdays(data.frame(date = as.Date(sales_train_grouped$date_id))$date)

cats <- categories(sales_train_grouped[, c("month", "day_of_the_week")], p = "all")
dummies_products <- dummy(sales_train_grouped[, c("month", "day_of_the_week")],
                          object = cats)
sales_train_grouped <- subset(sales_train_grouped, select = -c(month, day_of_the_week, year,day))
sales_train_grouped <- cbind(sales_train_grouped, dummies_products)


return_rate_yr1 <- merge(return_rate_yr1, sales_train_grouped, by = "pid")
file_path <- "New Data/return_rate_yr1.csv"
write.csv(return_rate_yr1,file = file_path,row.names = FALSE)

#Doing the same for the test set 
sales_test <- read.csv("Original Data/sales_yr2.csv")
sales_test$date_id <- as.Date(as.character(sales_test$date_id), format = "%Y%m%d")
sales_test$date_id <- impute(sales_test$date_id, method = mean) 
sales_test$net_sales_amount <- impute(sales_test$net_sales_amount, method = mean) 
online_purchases_index <- which(sales_test$channel == "CH//Online")
sales_test[-c(online_purchases_index), 'channel'] <- 0
sales_test[c(online_purchases_index), 'channel'] <- 1


sales_test_grouped <- sales_test %>%
  group_by(pid) %>%
  summarize(
    channel = modus(channel), 
    date_id = modus(date_id), 
    net_sales_amount = sum(net_sales_amount)
  )

return_rate_yr2 <- sales_test %>%
  mutate(net_sales_amount = as.numeric(net_sales_amount)) %>%  # Ensure 'amount' is numeric
  group_by(pid) %>%  # Group data by product ID ('pid')
  summarize(
    total_returned = sum(net_sales_amount[net_sales_amount < 0], na.rm = TRUE),  # Sum negative amounts
    total_sold = sum(net_sales_amount[net_sales_amount > 0], na.rm = TRUE),  # Sum positive amounts
    return_rate = -total_returned / total_sold  # Calculate return rate
  ) %>%
  mutate(return_rate = ifelse(is.nan(return_rate) | is.infinite(return_rate), 0, return_rate)) %>%  # Handle NaN and Inf
  ungroup()

sales_test_grouped[c('year', 'month', 'day')] <- str_split_fixed(sales_test_grouped$date_id, '-', 3)
sales_test_grouped[c('day_of_the_week')] <- weekdays(data.frame(date = as.Date(sales_test_grouped$date_id))$date)

cats <- categories(sales_test_grouped[, c("month", "day_of_the_week")], p = "all")
dummies_products <- dummy(sales_test_grouped[, c("month", "day_of_the_week")],
                          object = cats)
sales_test_grouped <- subset(sales_test_grouped, select = -c(month, day_of_the_week, year,day))
sales_test_grouped <- cbind(sales_test_grouped, dummies_products)


return_rate_yr2 <- merge(return_rate_yr2, sales_test_grouped, by = "pid")
file_path <- "New Data/return_rate_yr2.csv"
write.csv(return_rate_yr2,file = file_path,row.names = FALSE)
